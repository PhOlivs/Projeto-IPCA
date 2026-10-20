# R/00_inspecao_dados.R
# Mostra a estrutura REAL do arquivo bruto mais recente, para você conferir/adaptar a limpeza.
# Execução: Rscript R/00_inspecao_dados.R

suppressPackageStartupMessages({ library(jsonlite); library(dplyr) })
arqs <- sort(list.files("data/raw", "^sidra_7060_dados_.*\\.json$", full.names = TRUE))
if (!length(arqs)) stop("Nenhum arquivo bruto em data/raw/. Rode antes: Rscript R/01_coleta.R", call. = FALSE)
arq <- tail(arqs, 1); cat("Arquivo:", arq, "\n\n")
df <- fromJSON(arq)

cat("Dimensões:", nrow(df), "linhas x", ncol(df), "colunas\n")
cat("\n--- Nomes das colunas e seu significado (linha de cabeçalho da API) ---\n")
print(data.frame(coluna = names(df), significado = unlist(df[1, ]), row.names = NULL))
cat("\n--- 5 primeiras linhas de dados ---\n"); print(as_tibble(df[2:6, ]))
dados <- df[-1, ]
cat("\n--- Valores não numéricos em V (ausentes/especiais) ---\n")
print(table(dados$V[!grepl("^-?[0-9]+(\\.[0-9]+)?$", dados$V)], useNA = "ifany"))
for (n in grep("^D[0-9]+N$", names(df), value = TRUE)) {
  u <- unique(dados[[n]]); cat("\n---", df[[n]][1], ":", length(u), "valores distintos ---\n")
  print(head(u, 12))
}
