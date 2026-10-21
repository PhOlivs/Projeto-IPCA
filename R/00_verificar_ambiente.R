# R/00_verificar_ambiente.R
# Verifica se o ambiente está pronto: versão do R, pacotes, Quarto, gráficos e acesso ao SIDRA.
# Execução (na raiz do projeto): Rscript R/00_verificar_ambiente.R

cat("== Verificação do ambiente ==\n")
cat("R:", R.version.string, "\n")
if (getRversion() < "4.1.0") stop("Este projeto usa o pipe nativo |>, que exige R >= 4.1.0.", call. = FALSE)

pacotes <- c("tidyverse", "lubridate", "httr2", "jsonlite", "janitor",
             "scales", "knitr", "rmarkdown")
instalado <- vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
for (p in pacotes) cat(sprintf("  [%s] %s\n", ifelse(instalado[p], "ok", "FALTA"), p))
if (!all(instalado)) {
  cat("\nInstale os pacotes que faltam com:\n  Rscript -e 'install.packages(c(",
      paste0('"', pacotes[!instalado], '"', collapse = ", "),
      "), repos = \"https://cloud.r-project.org\")'\n", sep = "")
}

q <- Sys.which("quarto")
cat(if (nzchar(q)) paste("  [ok] Quarto em", q) else
      "  [AVISO] Quarto não encontrado (necessário só para o relatório HTML).", "\n")

if (all(instalado)) {
  suppressPackageStartupMessages(library(ggplot2))
  arq <- tempfile(fileext = ".png")
  ok <- tryCatch({
    ggsave(arq, ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_line(),
           width = 4, height = 3, dpi = 100)
    file.exists(arq)
  }, error = function(e) { message("  Erro ao salvar PNG: ", conditionMessage(e)); FALSE })
  cat(sprintf("  [%s] gravação de PNG com ggplot2\n", ifelse(ok, "ok", "FALHA")))
  if (!ok) cat("  Dica: install.packages('ragg') costuma resolver problemas de dispositivo gráfico.\n")

  suppressPackageStartupMessages(library(httr2))
  url <- "https://apisidra.ibge.gov.br/values/t/7060/n1/all/v/63/p/202001/c315/7169"
  tryCatch({
    r <- request(url) |> req_timeout(30) |> req_perform()
    cat("  [ok] API do SIDRA respondeu (HTTP", resp_status(r), ")\n")
  }, error = function(e) cat("  [FALHA] API do SIDRA:", conditionMessage(e), "\n"))
  tryCatch({
    r <- request("https://api.bcb.gov.br/dados/serie/bcdata.sgs.4449/dados/ultimos/1?formato=json") |> req_timeout(30) |> req_perform()
    cat("  [ok] API do Banco Central (SGS) respondeu (HTTP", resp_status(r), ")\n")
  }, error = function(e) cat("  [FALHA] API do Banco Central:", conditionMessage(e), "\n"))
}
cat("\nSe todos os itens estão [ok], siga para: Rscript R/01_coleta.R\n")
