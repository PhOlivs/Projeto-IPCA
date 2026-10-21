# R/08_visualizacoes_hipoteses.R
# Gráficos dos testes de hipóteses (PNG em outputs/figures/). Execução: Rscript R/08_visualizacoes_hipoteses.R
# Requer: scripts 03 e 07 já executados.

suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(scales) })
need <- c("data/processed/ipca_indices.csv", "data/processed/itens_contribuicao.csv", "data/processed/h3_series_mensais.csv",
          "outputs/tables/h1_picos_itens.csv", "outputs/tables/h3_correlacao_cambio.csv")
if (!all(file.exists(need))) stop("Rode antes: Rscript R/07_hipoteses.R", call. = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

ind <- read_csv(need[1], show_col_types = FALSE); itens <- read_csv(need[2], show_col_types = FALSE)
ser <- read_csv(need[3], show_col_types = FALSE); pic <- read_csv(need[4], show_col_types = FALSE); cor_t <- read_csv(need[5], show_col_types = FALSE)

meses_pt <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
fm <- function(d) paste0(meses_pt[month(d)], "/", year(d))
num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
fim <- max(ind$data)
fonte <- paste0("Fonte: IBGE, SIDRA, tabela 7060; Banco Central, SGS (séries 1, 4449 e 11428). Elaboração própria. Dados até ", fm(fim), ".")
theme_set(theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold", size = 15), plot.subtitle = element_text(color = "grey30"),
  plot.caption = element_text(color = "grey40", hjust = 0, size = 9), panel.grid.minor = element_blank(),
  legend.position = "bottom", plot.title.position = "plot", plot.caption.position = "plot"))
salvar <- function(p, nome) ggsave(file.path("outputs/figures", nome), p, width = 10, height = 6, dpi = 200, bg = "white")

# Figura 6 — H2: Educação por mês do calendário
edu <- ind |> filter(str_detect(grupo, "^Educa")) |> mutate(ano = year(data), mes = month(data))
g6 <- ggplot(edu, aes(factor(mes, levels = 1:12, labels = meses_pt), var_mensal)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey40", width = 0.6) +
  geom_point(aes(color = factor(ano)), size = 2.6, position = position_jitter(width = 0.12, height = 0, seed = 1)) +
  scale_color_viridis_d(name = "Ano", end = 0.9) +
  scale_y_continuous(labels = function(x) paste0(num(x, 1), "%")) +
  labs(title = "Educação: variação mensal por mês do calendário", x = NULL, y = "Variação mensal (%)",
       subtitle = "Cada ponto é um ano; a caixa resume os anos disponíveis em cada mês", caption = fonte)
salvar(g6, "06_h2_educacao_por_mes.png")

# Figura 7 — H2: contribuição dos subgrupos de Educação em fevereiro
sub <- itens |> filter(str_detect(grupo, "^Educa"), nivel == 4, month(data) == 2) |> mutate(ano = year(data))
g7 <- ggplot(sub, aes(factor(ano), contrib_pp, fill = nome)) +
  geom_col(width = 0.7) + geom_hline(yintercept = 0, color = "grey50") +
  scale_fill_viridis_d(name = NULL, end = 0.9) + guides(fill = guide_legend(nrow = 2)) +
  labs(title = "Educação em fevereiro: contribuição por item", x = NULL, y = "Contribuição aproximada (p.p.)",
       subtitle = "Contribuição aproximada = peso x variação / 100 (estimativa simplificada)", caption = fonte)
salvar(g7, "07_h2_educacao_fevereiro_itens.png")

# Figura 8 — H1: principais itens nos picos
d8 <- pic |> mutate(mes_lab = fm(data), chave = paste(nome, mes_lab, sep = "___"), mes_f = fct_reorder(mes_lab, data))
g8 <- ggplot(d8, aes(contrib_pp, fct_reorder(chave, contrib_pp), fill = grupo)) +
  geom_col(width = 0.7) +
  facet_wrap(~ mes_f, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(x) str_trunc(sub("___.*$", "", x), 30)) +
  scale_fill_viridis_d(name = NULL, end = 0.9) + guides(fill = guide_legend(nrow = 2)) +
  theme(strip.text = element_text(face = "bold"), axis.text.y = element_text(size = 8)) +
  labs(title = "Itens que mais contribuíram nos cinco maiores picos do IPCA", x = "Contribuição aproximada (p.p.)", y = NULL,
       subtitle = "Os 8 itens de maior contribuição em cada mês de pico", caption = fonte)
ggsave("outputs/figures/08_h1_picos_itens.png", g8, width = 10, height = 9, dpi = 200, bg = "white")

# Figura 9 — H3: preços livres x monitorados
base_dt <- seq(min(ser$mes), by = "-1 month", length.out = 2)[2]
d9 <- ser |> select(mes, `IPCA geral` = geral, `Preços livres` = livres, `Preços monitorados` = monitorados) |>
  pivot_longer(-mes, names_to = "serie", values_to = "v") |> group_by(serie) |> arrange(mes, .by_group = TRUE) |>
  mutate(indice = 100 * cumprod(1 + v / 100)) |> ungroup()
d9 <- bind_rows(d9, tibble(mes = base_dt, serie = unique(d9$serie), v = NA_real_, indice = 100))
g9 <- ggplot(d9, aes(mes, indice, color = serie, linetype = serie)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("IPCA geral" = "black", "Preços livres" = "#0072B2", "Preços monitorados" = "#D55E00"), name = NULL) +
  scale_linetype_manual(values = c("IPCA geral" = "dashed", "Preços livres" = "solid", "Preços monitorados" = "solid"), name = NULL) +
  scale_x_date(breaks = seq(as.Date("2020-01-01"), fim, by = "1 year"), date_labels = "%Y") +
  labs(title = "Preços livres x preços monitorados (administrados)", x = NULL, y = paste0("Índice (base 100 = ", fm(base_dt), ")"),
       subtitle = "Índices construídos por composição das variações mensais do IPCA (BCB/SGS)", caption = fonte)
salvar(g9, "09_h3_livres_monitorados.png")

# Figura 10 — H3: correlação defasada com o câmbio
ordem <- c("IPCA geral", sort(setdiff(unique(cor_t$grupo), "IPCA geral")))
d10 <- cor_t |> mutate(grupo = factor(grupo, levels = rev(ordem)), rotulo = paste0(num(correlacao), ifelse(acima_limiar, "*", "")))
m <- max(abs(d10$correlacao), na.rm = TRUE)
g10 <- ggplot(d10, aes(factor(defasagem_meses), grupo, fill = correlacao)) +
  geom_tile(color = "white", linewidth = 0.6) + geom_text(aes(label = rotulo), size = 3.2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-m, m), name = "Correlação") +
  theme(panel.grid = element_blank(), legend.position = "right") +
  labs(title = "Correlação entre a inflação dos grupos e a variação do câmbio", x = "Defasagem do câmbio (meses)", y = NULL,
       subtitle = "* = acima de 2/raiz(n) (limiar aproximado de ruído). Com ~70 testes, alguns * ocorrem por acaso; não indica causalidade.",
       caption = fonte)
salvar(g10, "10_h3_correlacao_cambio.png")
message("5 gráficos de hipóteses salvos em outputs/figures/ (06 a 10)")
