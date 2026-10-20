# R/02_limpeza.R
# Lê o JSON bruto MAIS RECENTE, padroniza, valida e grava a base tratada em data/processed/.
# Execução: Rscript R/02_limpeza.R
# Gera: data/processed/ipca_tratado.csv e outputs/tables/validacao_cobertura.csv

suppressPackageStartupMessages({
  library(tidyverse); library(lubridate); library(jsonlite); library(janitor)
})
if (!all(dir.exists(c("R", "data")))) stop("Execute a partir da raiz do projeto.", call. = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

arqs <- sort(list.files("data/raw", "^sidra_7060_dados_.*\\.json$", full.names = TRUE))
if (!length(arqs)) stop("Sem dados brutos. Rode antes: Rscript R/01_coleta.R", call. = FALSE)
arq <- tail(arqs, 1); message("Lendo: ", arq)
df <- fromJSON(arq)
stopifnot(is.data.frame(df), "V" %in% names(df), nrow(df) > 1)

detecta_dim <- function(df, padrao) {
  cols_n <- grep("^D[0-9]+N$", names(df), value = TRUE)
  achou <- cols_n[vapply(cols_n, function(n) grepl(padrao, df[[n]][1], ignore.case = TRUE), logical(1))]
  if (length(achou) != 1) stop("Dimensão '", padrao, "' não identificada. Rode R/00_inspecao_dados.R.", call. = FALSE)
  sub("N$", "", achou)
}
d_var <- detecta_dim(df, "^Vari"); d_mes <- detecta_dim(df, "^M[eê]s"); d_cat <- detecta_dim(df, "geral|grupo")
dados <- df[-1, ]

# 1) nomes padronizados e tipos
tidy <- tibble(
  variavel_cod  = dados[[paste0(d_var, "C")]],
  mes_cod       = dados[[paste0(d_mes, "C")]],
  categoria_cod = dados[[paste0(d_cat, "C")]],
  categoria     = dados[[paste0(d_cat, "N")]],
  valor_txt     = dados$V
) |> clean_names()

# 2) valores ausentes/especiais: o SIDRA usa símbolos como "-", "..", "...", "X" em vez de números
especiais <- tidy |> filter(!str_detect(valor_txt, "^-?[0-9]+(\\.[0-9]+)?$")) |> count(valor_txt, name = "n")
if (nrow(especiais)) { message("Valores não numéricos encontrados (viram NA, NÃO são removidos):"); print(especiais) }

tidy <- tidy |>
  mutate(valor = suppressWarnings(as.numeric(valor_txt)),
         data  = as.Date(paste0(mes_cod, "01"), format = "%Y%m%d"))
if (anyNA(tidy$data)) stop("Há períodos que não puderam ser convertidos em data.", call. = FALSE)

mapa <- c("63" = "var_mensal", "66" = "peso_mensal", "69" = "acum_ano", "2265" = "acum_12m")
if (!all(names(mapa) %in% tidy$variavel_cod)) stop("Faltam variáveis esperadas (63, 66, 69, 2265).", call. = FALSE)
tidy <- tidy |>
  mutate(var_nome = unname(mapa[variavel_cod]),
         tipo  = if_else(str_detect(categoria, regex("^(Índice|Indice) geral", ignore_case = TRUE)), "geral", "grupo"),
         grupo = if_else(tipo == "geral", "IPCA geral", str_remove(categoria, "^[0-9]+\\.\\s*")))

# 3) duplicatas
dup <- get_dupes(tidy, data, categoria_cod, variavel_cod)
if (nrow(dup)) {
  message("Duplicatas encontradas: ", nrow(dup), " linhas. Removendo apenas cópias exatas.")
  tidy <- distinct(tidy)
  if (nrow(get_dupes(tidy, data, categoria_cod, variavel_cod))) stop("Duplicatas conflitantes (mesma chave, valores diferentes).", call. = FALSE)
}

# 4) período de interesse
tidy <- filter(tidy, data >= as.Date("2020-01-01"))

# 5) cobertura temporal por categoria e variável
meses_esp <- seq(min(tidy$data), max(tidy$data), by = "month")
cobertura <- tidy |>
  group_by(grupo, var_nome) |>
  summarise(primeiro = min(data), ultimo = max(data), n_meses = n_distinct(data),
            n_na = sum(is.na(valor)), .groups = "drop") |>
  mutate(completo = n_meses == length(meses_esp) & n_na == 0)
write_csv(cobertura, "outputs/tables/validacao_cobertura.csv")
print(cobertura, n = Inf)
if (any(!cobertura$completo[cobertura$var_nome == "var_mensal"])) {
  stop("Há lacunas/NA em var_mensal. Veja outputs/tables/validacao_cobertura.csv antes de seguir.", call. = FALSE)
}
if (any(!cobertura$completo)) warning("Outras variáveis têm lacunas; análises que as usam serão limitadas.")
if (n_distinct(tidy$categoria_cod) != 10) stop("Esperava 10 categorias (geral + 9 grupos).", call. = FALSE)

# 6) formato largo: uma linha por mês x categoria
tratado <- tidy |>
  select(data, categoria_cod, categoria, grupo, tipo, var_nome, valor) |>
  pivot_wider(names_from = var_nome, values_from = valor) |>
  arrange(tipo != "geral", categoria_cod, data)
write_csv(tratado, "data/processed/ipca_tratado.csv")
message("\nBase tratada: data/processed/ipca_tratado.csv (", nrow(tratado), " linhas)")
message("Período: ", format(min(tratado$data), "%m/%Y"), " a ", format(max(tratado$data), "%m/%Y"))
