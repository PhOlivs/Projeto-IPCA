# R/04_visualizacoes.R
# Cria 5 gráficos PNG em outputs/figures/. Execução: Rscript R/04_visualizacoes.R
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(scales) })
need <- c("data/processed/ipca_indices.csv", "outputs/tables/ranking_grupos.csv", "outputs/tables/contribuicao_grupos.csv")
if (!all(file.exists(need))) stop("Rode antes: Rscript R/03_analise.R", call. = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

ind <- read_csv(need[1], show_col_types = FALSE); rk <- read_csv(need[2], show_col_types = FALSE)
cg <- read_csv(need[3], show_col_types = FALSE)
geral <- filter(ind, tipo == "geral"); grupos <- filter(ind, tipo == "grupo")

meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
fmt_mes <- function(d) paste0(meses_pt[month(d)], "/", year(d))
num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pct_lab <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")
fim <- max(ind$data)
fonte <- paste0("Fonte: IBGE, SIDRA, tabela 7060 (IPCA). Elaboração própria. Dados até ", fmt_mes(fim), ".")

theme_set(theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold", size = 15), plot.subtitle = element_text(color = "grey30"),
  plot.caption = element_text(color = "grey40", hjust = 0, size = 9), panel.grid.minor = element_blank(),
  legend.position = "bottom", plot.title.position = "plot", plot.caption.position = "plot"))
salvar <- function(p, nome) ggsave(file.path("outputs/figures", nome), p, width = 10, height = 6, dpi = 200, bg = "white")

# Gráfico 1 — inflação mensal
dest <- bind_rows(slice_max(geral, var_mensal, n = 1), slice_min(geral, var_mensal, n = 1))
g1 <- ggplot(geral, aes(data, var_mensal)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "#1F4E79", linewidth = 0.8) +
  geom_point(data = dest, color = "#B2182B", size = 2.5) +
  geom_text(data = dest, aes(label = paste0(num(var_mensal), "%\n", fmt_mes(data))),
            vjust = ifelse(dest$var_mensal > 0, -0.4, 1.3), size = 3.3, lineheight = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = pct_lab, expand = expansion(mult = 0.2)) +
  labs(title = "Inflação mensal medida pelo IPCA", subtitle = "Variação percentual em relação ao mês anterior",
       x = NULL, y = "Variação mensal (%)", caption = fonte)
salvar(g1, "01_inflacao_mensal.png")

# Gráfico 2 — índice por grupo (base 100 = mês anterior ao início da série)
base_dt <- seq(min(ind$data), by = "-1 month", length.out = 2)[2]
d2 <- bind_rows(select(ind, grupo, tipo, data, indice), distinct(ind, grupo, tipo) |> mutate(data = base_dt, indice = 100))
nomes_g <- sort(unique(grupos$grupo))
pal <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#7B3294", "#0072B2", "#D55E00", "#CC79A7", "#8C510A", "#5D6D7E")[seq_along(nomes_g)], nomes_g)
g2 <- ggplot() +
  geom_line(data = filter(d2, tipo == "grupo"), aes(data, indice, color = grupo), linewidth = 0.8) +
  geom_line(data = filter(d2, tipo == "geral"), aes(data, indice), color = "black", linewidth = 1.3, linetype = "dashed") +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_date(breaks = seq(as.Date("2020-01-01"), fim, by = "1 year"), date_labels = "%Y",
               limits = c(base_dt, fim + 45), expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = function(x) num(x, 0)) +
  guides(color = guide_legend(nrow = 3)) +
  labs(title = "Evolução acumulada dos preços por grupo", x = NULL, y = paste0("Índice (base 100 = ", fmt_mes(base_dt), ")"),
       subtitle = "Linha tracejada preta = IPCA geral. Índice construído por composição das variações mensais.", caption = fonte)
salvar(g2, "02_indice_acumulado_grupos.png")

# Gráfico 3 — ranking de aumento acumulado
aum_geral <- last(geral$aumento_acum_pct)
g3 <- rk |> mutate(grupo = fct_reorder(grupo, aumento_acum_pct), acima = factor(acima_do_geral, levels = c(FALSE, TRUE))) |>
  ggplot(aes(aumento_acum_pct, grupo, fill = acima)) +
  geom_col(width = 0.7) +
  geom_text(aes(x = max(aumento_acum_pct) * 1.02, label = paste0(num(aumento_acum_pct, 1), "%")), hjust = 0, size = 3.6) +
  geom_vline(xintercept = aum_geral, linetype = "dashed") +
  annotate("text", x = aum_geral, y = 1, label = paste0("IPCA geral: ", num(aum_geral, 1), "%"), hjust = -0.05, size = 3.4) +
  scale_fill_manual(values = c("FALSE" = "#9AA5B1", "TRUE" = "#B2182B"), drop = FALSE,
                    labels = c("Até o IPCA geral", "Acima do IPCA geral"), name = NULL) +
  scale_x_continuous(labels = pct_lab, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Quais grupos mais encareceram desde 2020", x = paste0("Aumento acumulado de preços, ", fmt_mes(min(geral$data)), " a ", fmt_mes(fim), " (%)"),
       y = NULL, subtitle = "Aumento acumulado = índice composto final - 100", caption = fonte)
salvar(g3, "03_ranking_aumento_acumulado.png")

# Gráfico 4 — mapa de calor
hm <- geral |> mutate(ano = year(data), mes = month(data)) |> select(ano, mes, var_mensal) |> complete(ano, mes = 1:12)
lim <- max(abs(hm$var_mensal), na.rm = TRUE)
g4 <- ggplot(hm, aes(factor(mes, levels = 1:12, labels = meses_pt), factor(ano, levels = rev(sort(unique(ano)))), fill = var_mensal)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(data = filter(hm, !is.na(var_mensal)), aes(label = num(var_mensal), color = ifelse(abs(var_mensal) >= 0.9 * lim * 0.7, "w", "k")), size = 3.2) +
  scale_color_manual(values = c(w = "white", k = "grey15"), guide = "none") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-lim, lim),
                       na.value = "grey92", name = "Variação\nmensal (%)") +
  scale_x_discrete(drop = FALSE, position = "top") +
  theme(panel.grid = element_blank(), legend.position = "right") +
  labs(title = "Mapa de calor da inflação mensal (IPCA geral)", x = NULL, y = NULL,
       subtitle = "Cada célula é a variação percentual do mês; células cinzas = mês ainda não divulgado", caption = fonte)
salvar(g4, "04_heatmap_inflacao_mensal.png")

# Gráfico 5 — peso e contribuição aproximada
rotulos <- c(peso_medio = "Peso médio na cesta (%)", soma_contrib_pp = "Contribuição acumulada aproximada (p.p.)")
g5 <- cg |> mutate(grupo = fct_reorder(grupo, soma_contrib_pp)) |>
  pivot_longer(c(peso_medio, soma_contrib_pp), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = factor(rotulos[metrica], levels = unname(rotulos))) |>
  ggplot(aes(valor, grupo)) +
  geom_col(fill = "#1F4E79", width = 0.7) +
  geom_text(aes(label = num(valor, 1)), hjust = -0.1, size = 3.4) +
  facet_wrap(~ metrica, scales = "free_x") +
  theme(panel.spacing.x = unit(2, "lines")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Peso na cesta e contribuição aproximada para o IPCA", x = NULL, y = NULL,
       subtitle = "Contribuição aproximada = soma mensal de (peso x variação)/100. Estimativa simplificada, não o impacto oficial do IBGE.",
       caption = fonte)
salvar(g5, "05_peso_contribuicao_grupos.png")
message("5 gráficos salvos em outputs/figures/")
