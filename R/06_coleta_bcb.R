# R/06_coleta_bcb.R
# Coleta séries do Banco Central (SGS) usadas para testar as hipóteses:
#   1     = Taxa de câmbio - Livre - Dólar americano (venda) - diário (R$/US$)
#   4449  = IPCA - Preços monitorados - Total (variação % mensal)
#   11428 = IPCA - Itens livres (variação % mensal)
# Códigos conferidos no catálogo https://dadosabertos.bcb.gov.br/
# Execução: Rscript R/06_coleta_bcb.R
# Gera: data/raw/bcb_sgs_<código>_<data_hora>.csv e data/raw/bcb_meta_<data_hora>.json
#
# A API do BCB é instável (HTTP 502 e páginas de erro em HTML com status 200). Por isso cada consulta é refeita
# até 6 vezes com espera crescente, e NENHUM arquivo é gravado se houver janelas sem resposta ou lacunas na série.
# Se falhar, rode de novo em alguns minutos: janelas já obtidas não são salvas, mas a coleta é rápida.

suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr) })
if (!all(dir.exists(c("R", "data")))) stop("Execute a partir da raiz do projeto.", call. = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

INICIO <- as.Date("2019-12-01")   # dez/2019 é necessário para a variação do câmbio em jan/2020
FIM <- Sys.Date()
SERIES <- list(list(codigo = 1, diaria = TRUE), list(codigo = 4449, diaria = FALSE), list(codigo = 11428, diaria = FALSE))
MAX_LACUNA_DIAS <- 10   # série diária: maior intervalo aceito entre observações (fins de semana e feriados)

bcb_get <- function(codigo, ini, fim, tentativas = 6) {
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s&dataFinal=%s",
                 codigo, format(ini, "%d/%m/%Y"), format(fim, "%d/%m/%Y"))
  message("Consultando: ", url)
  for (k in seq_len(tentativas)) {
    resp <- tryCatch(request(url) |> req_user_agent("projeto-ipca-portfolio (R/httr2)") |> req_timeout(120) |>
                       req_error(is_error = function(r) FALSE) |> req_perform(), error = function(e) NULL)
    if (!is.null(resp)) {
      status <- resp_status(resp); txt <- resp_body_string(resp, encoding = "UTF-8")
      if (status < 400 && identical(trimws(txt), "[]")) return(list(url = url, df = data.frame(data = character(), valor = character())))
      df <- tryCatch(fromJSON(txt), error = function(e) NULL)
      if (status < 400 && is.data.frame(df) && all(c("data", "valor") %in% names(df)) && nrow(df) > 0) {
        return(list(url = url, df = df[, c("data", "valor")]))
      }
      trecho <- substr(gsub("\\s+", " ", txt), 1, 100)
    } else { status <- NA; trecho <- "falha de conexão" }
    message("  Tentativa ", k, "/", tentativas, " falhou (HTTP ", status, "): ", trecho)
    Sys.sleep(3 * k)
  }
  NULL
}

trimestres <- function(ini, fim) {
  inicios <- seq(as.Date(format(ini, "%Y-%m-01")), fim, by = "3 months")
  data.frame(ini = pmax(inicios, ini),
             fim = pmin(seq(inicios[1], by = "3 months", length.out = length(inicios) + 1)[-1] - 1, fim))
}

carimbo <- format(Sys.time(), "%Y%m%d_%H%M%S")
coletas <- list()
for (s in SERIES) {   # 1) coleta TUDO e valida; 2) só depois grava
  if (s$diaria) {
    j <- trimestres(INICIO, FIM)
    res <- lapply(seq_len(nrow(j)), function(i) { r <- bcb_get(s$codigo, j$ini[i], j$fim[i]); Sys.sleep(0.5); r })
    falhas <- which(vapply(res, is.null, logical(1)))
    if (length(falhas)) stop("Janelas sem resposta após várias tentativas (início): ", paste(format(j$ini[falhas]), collapse = ", "),
                             ".\nA API do BCB está instável; rode novamente em alguns minutos. Nada foi gravado.", call. = FALSE)
    df <- distinct(bind_rows(lapply(res, `[[`, "df"))); urls <- vapply(res, `[[`, "", "url")
  } else {
    r <- bcb_get(s$codigo, INICIO, FIM)
    if (is.null(r)) stop("Série mensal obrigatória sem resposta após várias tentativas: ", s$codigo, ". Rode novamente em alguns minutos. Nada foi gravado.", call. = FALSE)
    df <- distinct(r$df); urls <- r$url
  }
  datas <- sort(unique(as.Date(df$data, format = "%d/%m/%Y")))
  if (!length(datas) || anyNA(datas)) stop("Datas inválidas ou ausentes na série ", s$codigo, call. = FALSE)
  if (s$diaria) {
    lacuna <- max(as.numeric(diff(datas)))
    if (lacuna > MAX_LACUNA_DIAS || min(datas) > INICIO + 10 || max(datas) < FIM - 15) {
      stop("Série ", s$codigo, " com lacuna (maior intervalo: ", lacuna, " dias; de ", format(min(datas)), " a ", format(max(datas)),
           "). Nada foi gravado; rode novamente.", call. = FALSE)
    }
  }
  message("Série ", s$codigo, ": ", nrow(df), " observações, de ", format(min(datas)), " a ", format(max(datas)), " (validada)")
  coletas[[as.character(s$codigo)]] <- list(df = df, urls = urls, primeira = min(datas), ultima = max(datas))
}

meta <- list(); gravados <- 0
for (cod in names(coletas)) {
  x <- coletas[[cod]]
  tmp <- tempfile(fileext = ".csv"); write.csv(x$df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  exist <- list.files("data/raw", sprintf("^bcb_sgs_%s_[0-9]{8}_[0-9]{6}\\.csv$", cod), full.names = TRUE)
  if (length(exist) && any(unname(tools::md5sum(exist)) == unname(tools::md5sum(tmp)))) { message("Série ", cod, ": idêntica a coleta anterior; não gravada."); next }
  arq <- file.path("data/raw", sprintf("bcb_sgs_%s_%s.csv", cod, carimbo)); invisible(file.copy(tmp, arq)); gravados <- gravados + 1
  meta[[cod]] <- list(arquivo = basename(arq), urls = x$urls, n = nrow(x$df), primeira = format(x$primeira), ultima = format(x$ultima))
}
if (length(meta)) write_json(c(list(fonte = "Banco Central do Brasil - SGS", data_coleta = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), meta),
                             file.path("data/raw", paste0("bcb_meta_", carimbo, ".json")), auto_unbox = TRUE, pretty = TRUE)
message("\nColeta BCB concluída. Arquivos novos: ", gravados)
