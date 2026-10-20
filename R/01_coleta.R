# R/01_coleta.R
# Coleta o IPCA (SIDRA, tabela 7060) pela API oficial e preserva a resposta bruta.
# Execução: Rscript R/01_coleta.R
#
# Estrutura da tabela 7060 (mensal, a partir de 202001), conforme documentação consultada:
#   variáveis: 63 = variação mensal | 66 = peso mensal | 69 = acumulada no ano | 2265 = acumulada em 12 meses
#   classificação 315 = "Geral, grupo, subgrupo, item e subitem" (365 categorias; 7169 = índice geral)
# Os códigos dos 9 grupos NÃO são fixados aqui: o script os descobre pela própria API (passo 1).
# URL da API:  /values/t/<tabela>/n1/all (Brasil)/v/<variáveis>/p/<períodos>/c315/<categorias>

suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr) })

if (!all(dir.exists(c("R", "data")))) {
  stop("Execute a partir da raiz do projeto (pasta que contém R/ e data/).", call. = FALSE)
}
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

API_BASE  <- "https://apisidra.ibge.gov.br/values"
TABELA    <- "7060"
VARIAVEIS <- c("63", "66", "69", "2265")
PERIODO_0 <- "202001"

sidra_get <- function(caminho) {
  url <- paste0(API_BASE, caminho)
  message("Consultando: ", url)
  resp <- tryCatch(
    request(url) |>
      req_user_agent("projeto-ipca-portfolio (R/httr2)") |>
      req_timeout(120) |>
      req_retry(max_tries = 3, backoff = ~ 5) |>
      req_perform(),
    error = function(e) stop(
      "Falha na requisição ao SIDRA: ", conditionMessage(e), "\nURL: ", url,
      "\nVerifique a conexão ou use a alternativa por CSV descrita no README.", call. = FALSE))
  list(url = url, txt = resp_body_string(resp, encoding = "UTF-8"))
}

parse_sidra <- function(txt) {
  df <- tryCatch(fromJSON(txt), error = function(e)
    stop("A resposta não é JSON válido. Início da resposta:\n", substr(txt, 1, 300), call. = FALSE))
  if (!is.data.frame(df) || nrow(df) < 2 || !"V" %in% names(df)) {
    stop("Estrutura inesperada (esperado: linha de cabeçalho + dados, coluna 'V'). ",
         "Rode Rscript R/00_inspecao_dados.R e adapte.", call. = FALSE)
  }
  df
}

# A 1ª linha da resposta é um cabeçalho descritivo; usamos esse texto para localizar as colunas.
detecta_dim <- function(df, padrao) {
  cols_n <- grep("^D[0-9]+N$", names(df), value = TRUE)
  achou <- cols_n[vapply(cols_n, function(n) grepl(padrao, df[[n]][1], ignore.case = TRUE), logical(1))]
  if (length(achou) != 1) {
    cab <- paste(vapply(cols_n, function(n) paste0(n, "=", df[[n]][1]), ""), collapse = " | ")
    stop("Não consegui identificar a dimensão '", padrao, "'. Cabeçalhos: ", cab, call. = FALSE)
  }
  sub("N$", "", achou)
}

# ---- Passo 1: descobrir categorias (um mês, todas as categorias) ----
r1  <- sidra_get(sprintf("/t/%s/n1/all/v/63/p/%s/c315/all", TABELA, PERIODO_0))
df1 <- parse_sidra(r1$txt)
d_cat <- detecta_dim(df1, "geral|grupo")
cats <- tibble(cod = df1[[paste0(d_cat, "C")]][-1], nome = df1[[paste0(d_cat, "N")]][-1])
message("Categorias na classificação 315: ", nrow(cats))

eh_geral <- grepl("^(Índice|Indice) geral", cats$nome, ignore.case = TRUE)
eh_grupo <- grepl("^[1-9]\\.[^0-9]", cats$nome)  # "1.Alimentação e bebidas"; subgrupos têm 2+ dígitos
if (sum(eh_geral) != 1 || sum(eh_grupo) != 9) {
  print(head(cats, 40))
  stop("Esperava 1 índice geral e 9 grupos; achei ", sum(eh_geral), " e ", sum(eh_grupo),
       ". Veja os nomes acima e ajuste as expressões regulares.", call. = FALSE)
}
sel <- cats[eh_geral | eh_grupo, ]
print(as.data.frame(sel))

# ---- Passo 2: variáveis 63, 66, 69, 2265 das 10 categorias, todos os meses ----
r2  <- sidra_get(sprintf("/t/%s/n1/all/v/%s/p/all/c315/%s", TABELA,
                         paste(VARIAVEIS, collapse = ","), paste(sel$cod, collapse = ",")))
df2 <- parse_sidra(r2$txt)
d_var  <- detecta_dim(df2, "^Vari")
d_mes  <- detecta_dim(df2, "^M[eê]s")
d_cat2 <- detecta_dim(df2, "geral|grupo")

vars_ok <- unique(df2[[paste0(d_var, "C")]][-1])
if (!all(VARIAVEIS %in% vars_ok)) {
  stop("Variáveis ausentes na resposta: ", paste(setdiff(VARIAVEIS, vars_ok), collapse = ", "), call. = FALSE)
}
if (length(unique(df2[[paste0(d_cat2, "C")]][-1])) != 10) stop("A resposta não contém as 10 categorias.", call. = FALSE)

periodos <- sort(unique(df2[[paste0(d_mes, "C")]][-1]))
message("Período retornado: ", periodos[1], " a ", periodos[length(periodos)],
        " (", length(periodos), " meses). Linhas de dados: ", nrow(df2) - 1)

# ---- Passo 3: gravar sem sobrescrever ----
carimbo <- format(Sys.time(), "%Y%m%d_%H%M%S")
tmp <- tempfile(fileext = ".json")
writeLines(r2$txt, tmp, useBytes = TRUE)
existentes <- list.files("data/raw", "^sidra_7060_dados_.*\\.json$", full.names = TRUE)
if (length(existentes) && any(unname(tools::md5sum(existentes)) == unname(tools::md5sum(tmp)))) {
  message("Conteúdo idêntico a um arquivo já coletado; nada novo foi gravado.")
  quit(save = "no", status = 0)
}
arq_dados <- file.path("data/raw", paste0("sidra_7060_dados_", carimbo, ".json"))
stopifnot(!file.exists(arq_dados))
file.copy(tmp, arq_dados)

write_json(list(
  fonte = "IBGE/SIDRA - Tabela 7060 (IPCA)", url_dados = r2$url, url_categorias = r1$url,
  data_coleta = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  variaveis = VARIAVEIS, primeiro_periodo = periodos[1], ultimo_periodo = periodos[length(periodos)],
  n_linhas_dados = nrow(df2) - 1, arquivo_dados = basename(arq_dados), versao_r = R.version.string),
  file.path("data/raw", paste0("sidra_7060_meta_", carimbo, ".json")), auto_unbox = TRUE, pretty = TRUE)
write.csv(sel, file.path("data/raw", paste0("sidra_7060_categorias_", carimbo, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

message("\nColeta concluída: ", arq_dados)
message("Confira com:  ls -lh data/raw/   e   Rscript R/00_inspecao_dados.R")
