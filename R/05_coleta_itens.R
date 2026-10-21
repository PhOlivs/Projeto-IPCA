# R/05_coleta_itens.R
# Coleta o IPCA por SUBGRUPO (2 dígitos) e ITEM (4 dígitos) na tabela 7060 do SIDRA.
# Necessário para testar as hipóteses (decomposição dos picos e de Educação em fevereiro).
# Execução: Rscript R/05_coleta_itens.R
# Gera: data/raw/sidra_7060_itens_<data_hora>.csv e ..._itens_meta_<data_hora>.json
#
# Os códigos NÃO são fixados: o script lê o catálogo da classificação 315 e escolhe as categorias
# pela numeração do nome ("81.", "8101." ...). Se a numeração for diferente, o script mostra a contagem
# por nível e para, em vez de seguir com dados errados.

suppressPackageStartupMessages({ library(tidyverse); library(httr2); library(jsonlite) })
if (!all(dir.exists(c("R", "data")))) stop("Execute a partir da raiz do projeto.", call. = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

API_BASE <- "https://apisidra.ibge.gov.br/values"; TABELA <- "7060"; PERIODO_0 <- "202001"
TAM_LOTE <- 40  # categorias por requisição (evita URLs muito longas)

sidra_get <- function(caminho) {
  url <- paste0(API_BASE, caminho); message("Consultando: ", url)
  resp <- tryCatch(
    request(url) |> req_user_agent("projeto-ipca-portfolio (R/httr2)") |> req_timeout(180) |>
      req_retry(max_tries = 3, backoff = ~ 5) |> req_perform(),
    error = function(e) stop("Falha na requisição ao SIDRA: ", conditionMessage(e), "\nURL: ", url, call. = FALSE))
  list(url = url, txt = resp_body_string(resp, encoding = "UTF-8"))
}
parse_sidra <- function(txt) {
  df <- tryCatch(fromJSON(txt), error = function(e) stop("Resposta não é JSON válido: ", substr(txt, 1, 300), call. = FALSE))
  if (!is.data.frame(df) || nrow(df) < 2 || !"V" %in% names(df)) stop("Estrutura inesperada da resposta.", call. = FALSE)
  df
}
detecta_dim <- function(df, padrao) {
  cols_n <- grep("^D[0-9]+N$", names(df), value = TRUE)
  achou <- cols_n[vapply(cols_n, function(n) grepl(padrao, df[[n]][1], ignore.case = TRUE), logical(1))]
  if (length(achou) != 1) stop("Dimensão '", padrao, "' não identificada.", call. = FALSE)
  sub("N$", "", achou)
}

# 1) catálogo de categorias
r1 <- sidra_get(sprintf("/t/%s/n1/all/v/63/p/%s/c315/all", TABELA, PERIODO_0))
df1 <- parse_sidra(r1$txt); d_cat <- detecta_dim(df1, "geral|grupo")
cats <- tibble(cod = df1[[paste0(d_cat, "C")]][-1], nome = df1[[paste0(d_cat, "N")]][-1]) |>
  mutate(cod_num = str_extract(nome, "^[0-9]+(?=\\.)"), nivel = nchar(cod_num))
cat("\nCategorias por nível de numeração (NA = índice geral):\n"); print(count(cats, nivel))
sel <- filter(cats, nivel %in% c(2, 4))
if (nrow(sel) == 0 || !any(sel$nivel == 4)) {
  print(head(cats, 40)); stop("Não encontrei subgrupos (2 dígitos) e itens (4 dígitos). Veja os nomes acima.", call. = FALSE)
}
message("Selecionados: ", sum(sel$nivel == 2), " subgrupos e ", sum(sel$nivel == 4), " itens.")

# 2) dados (variação mensal 63 e peso mensal 66) em lotes
lotes <- split(sel$cod, ceiling(seq_along(sel$cod) / TAM_LOTE))
urls <- character(0)
dados <- map_dfr(lotes, function(cods) {
  r <- sidra_get(sprintf("/t/%s/n1/all/v/63,66/p/all/c315/%s", TABELA, paste(cods, collapse = ",")))
  urls <<- c(urls, r$url)
  d <- parse_sidra(r$txt); dv <- detecta_dim(d, "^Vari"); dm <- detecta_dim(d, "^M[eê]s"); dc <- detecta_dim(d, "geral|grupo")
  x <- d[-1, ]
  tibble(variavel_cod = x[[paste0(dv, "C")]], mes_cod = x[[paste0(dm, "C")]],
         categoria_cod = x[[paste0(dc, "C")]], categoria = x[[paste0(dc, "N")]], valor_txt = x$V)
})
faltam <- setdiff(sel$cod, unique(dados$categoria_cod))
if (length(faltam)) stop("Categorias ausentes na resposta: ", paste(head(faltam, 10), collapse = ", "), call. = FALSE)
message("Linhas coletadas: ", nrow(dados), " | meses: ", n_distinct(dados$mes_cod),
        " | último mês: ", max(dados$mes_cod))

# 3) gravar sem sobrescrever
carimbo <- format(Sys.time(), "%Y%m%d_%H%M%S")
tmp <- tempfile(fileext = ".csv"); write_csv(dados, tmp)
exist <- list.files("data/raw", "^sidra_7060_itens_[0-9]{8}_[0-9]{6}\\.csv$", full.names = TRUE)
if (length(exist) && any(unname(tools::md5sum(exist)) == unname(tools::md5sum(tmp)))) {
  message("Conteúdo idêntico a uma coleta anterior; nada novo foi gravado."); quit(save = "no", status = 0)
}
arq <- file.path("data/raw", paste0("sidra_7060_itens_", carimbo, ".csv")); invisible(file.copy(tmp, arq))
write_json(list(fonte = "IBGE/SIDRA - Tabela 7060 (IPCA), subgrupos e itens", data_coleta = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                urls = c(r1$url, urls), n_categorias = nrow(sel), n_linhas = nrow(dados), arquivo = basename(arq)),
           file.path("data/raw", paste0("sidra_7060_itens_meta_", carimbo, ".json")), auto_unbox = TRUE, pretty = TRUE)
message("\nColeta concluída: ", arq)
