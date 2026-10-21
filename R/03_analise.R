# R/03_analise.R
# Análise exploratória. Execução: Rscript R/03_analise.R
# Gera tabelas em outputs/tables/ e data/processed/ipca_indices.csv
#
# Conceitos (todos em %, exceto o índice):
#  - Variação mensal: variação do IPCA no mês (var. 63).
#  - Acumulada no ano: composta de jan até o mês (var. 69). Acumulada em 12 meses: composta nos últimos 12 meses (var. 2265).
#  - Índice (base 100 = dez/2019): 100 * produto(1 + var_mensal/100). Aumento acumulado = índice - 100.
#    NUNCA se somam variações mensais: 1% + 1% = 2,01% composto, não 2%.
#  - Peso: participação do grupo na cesta (%). Contribuição (p.p.) ~ peso x variação / 100 (estimativa simplificada).

suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
if (!file.exists("data/processed/ipca_tratado.csv")) stop("Rode antes: Rscript R/02_limpeza.R", call. = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dados <- read_csv("data/processed/ipca_tratado.csv", show_col_types = FALSE)
tab <- function(x, nome) { write_csv(x, file.path("outputs/tables", nome)); invisible(x) }

# 1) Índices compostos
ind <- dados |> arrange(grupo, data) |> group_by(grupo) |>
  mutate(indice = 100 * cumprod(1 + var_mensal / 100), aumento_acum_pct = indice - 100, ano = year(data)) |>
  ungroup()
write_csv(ind, "data/processed/ipca_indices.csv")
geral <- filter(ind, tipo == "geral"); grupos <- filter(ind, tipo == "grupo")

# 2) Validação: acumulados calculados x publicados (diferenças pequenas vêm de arredondamento a 2 casas)
val <- ind |> group_by(grupo) |> arrange(data) |>
  mutate(i = row_number(),
         ref12 = case_when(i == 12 ~ 100, i > 12 ~ lag(indice, 12), TRUE ~ NA_real_),
         acum12_calc = (indice / ref12 - 1) * 100) |>
  group_by(grupo, ano) |> mutate(acum_ano_calc = (cumprod(1 + var_mensal / 100) - 1) * 100) |> ungroup() |>
  mutate(dif_12m = acum12_calc - acum_12m, dif_ano = acum_ano_calc - acum_ano)
resumo_val <- val |> group_by(grupo) |>
  summarise(max_abs_dif_12m_pp = max(abs(dif_12m), na.rm = TRUE), max_abs_dif_ano_pp = max(abs(dif_ano), na.rm = TRUE))
tab(resumo_val, "validacao_acumulados.csv"); cat("\n== Validação dos acumulados (calculado x publicado) ==\n"); print(resumo_val)

# 3) Estatísticas da inflação mensal
est_fun <- function(d) d |> summarise(
  n_meses = n(), media = mean(var_mensal), mediana = median(var_mensal), desvio_padrao = sd(var_mensal),
  maximo = max(var_mensal), mes_maximo = data[which.max(var_mensal)],
  minimo = min(var_mensal), mes_minimo = data[which.min(var_mensal)],
  aumento_acum_pct = last(aumento_acum_pct))
est_geral <- tab(est_fun(geral), "estatisticas_geral.csv")
est_grupos <- tab(ind |> group_by(grupo) |> est_fun(), "estatisticas_por_grupo.csv")
cat("\n== IPCA geral ==\n"); print(as.data.frame(est_geral))

# 4) Ranking de aumento acumulado
aum_geral <- est_geral$aumento_acum_pct
rk <- grupos |> group_by(grupo) |>
  summarise(indice_final = last(indice), aumento_acum_pct = indice_final - 100,
            peso_medio = mean(peso_mensal, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(aumento_acum_pct)) |>
  mutate(posicao = row_number(), acima_do_geral = aumento_acum_pct > aum_geral)
tab(rk, "ranking_grupos.csv"); cat("\n== Ranking (aumento acumulado) ==\n"); print(rk)

# 5) Picos mensais
tab(geral |> arrange(desc(var_mensal)) |> select(data, var_mensal, acum_12m) |> head(10), "picos_mensais_geral.csv")
tab(grupos |> arrange(desc(var_mensal)) |> select(grupo, data, var_mensal) |> head(15), "picos_mensais_grupos.csv")

# 6) Alimentação e bebidas x geral
alim <- grupos |> filter(str_detect(grupo, "^Alimenta"))
comp <- inner_join(select(alim, data, var_alim = var_mensal, indice_alim = indice),
                   select(geral, data, var_geral = var_mensal, indice_geral = indice), by = "data") |>
  mutate(diferenca_indice = indice_alim - indice_geral, razao_indices = indice_alim / indice_geral,
         alim_acima_no_mes = var_alim > var_geral)
tab(comp, "comparacao_alimentacao_geral.csv")
cat("\nMeses em que Alimentação > geral:", sum(comp$alim_acima_no_mes), "de", nrow(comp), "\n")

# 7) Sazonalidade aparente (por mês do calendário; poucos anos -> indício, não prova)
sazon <- ind |> filter(tipo == "geral" | str_detect(grupo, "^Alimenta")) |>
  mutate(mes = month(data)) |> group_by(grupo, mes) |>
  summarise(n_anos = n(), media = mean(var_mensal), mediana = median(var_mensal),
            minimo = min(var_mensal), maximo = max(var_mensal), .groups = "drop")
tab(sazon, "sazonalidade.csv")

# 8) Peso x variação e contribuição aproximada
contrib <- grupos |> mutate(contrib_pp = peso_mensal * var_mensal / 100)
write_csv(contrib, "data/processed/contribuicao_mensal.csv")
chk <- contrib |> group_by(data) |> summarise(soma_pesos = sum(peso_mensal), soma_contrib = sum(contrib_pp), .groups = "drop") |>
  left_join(select(geral, data, var_geral = var_mensal), by = "data") |> mutate(residuo_pp = var_geral - soma_contrib)
tab(chk, "validacao_contribuicao.csv")
cat("\n== Contribuição: resíduo (IPCA geral - soma das contribuições) ==\n")
cat("Maior |resíduo| (p.p.):", round(max(abs(chk$residuo_pp)), 3), "| soma dos pesos entre", round(min(chk$soma_pesos), 2), "e", round(max(chk$soma_pesos), 2), "\n")
contrib_g <- contrib |> group_by(grupo) |>
  summarise(peso_medio = mean(peso_mensal, na.rm = TRUE), soma_contrib_pp = sum(contrib_pp, na.rm = TRUE), .groups = "drop") |>
  mutate(participacao_pct = 100 * soma_contrib_pp / sum(soma_contrib_pp)) |>
  left_join(select(rk, grupo, aumento_acum_pct), by = "grupo") |> arrange(desc(soma_contrib_pp))
tab(contrib_g, "contribuicao_grupos.csv"); print(contrib_g)

rho <- suppressWarnings(cor(rk$peso_medio, rk$aumento_acum_pct, method = "spearman"))
tab(tibble(correlacao_spearman_peso_x_aumento = rho, n_grupos = nrow(rk)), "correlacao_peso_aumento.csv")
cat("\nCorrelação de Spearman peso x aumento acumulado (n = 9, apenas descritiva):", round(rho, 2), "\n")
message("\nTabelas salvas em outputs/tables/")
