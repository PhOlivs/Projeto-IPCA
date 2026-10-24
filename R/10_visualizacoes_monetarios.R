# R/10_visualizacoes_monetarios.R
# Gráficos dos agregados monetários (PNG em outputs/figures/, numeração 11 a 15).
# Execução: Rscript R/10_visualizacoes_monetarios.R   (requer o script 09 já executado)

suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(scales) })
need <- c("data/processed/monetarios_ipca.csv", "outputs/tables/m_correlacoes_defasagens.csv", "outputs/tables/m_regressoes.csv")
if (!all(file.exists(need))) stop("Rode antes: Rscript R/09_monetarios.R", call. = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

d <- read_csv(need[1], show_col_types = FALSE); cors <- read_csv(need[2], show_col_types = FALSE); regs <- read_csv(need[3], show_col_types = FALSE)
ini_ipca <- min(d$mes[!is.na(d$ipca_m)]); fim_ipca <- max(d$mes[!is.na(d$ipca_m)]); fim_m <- max(d$mes)
fim_comum <- min(fim_ipca, fim_m)
jan <- filter(d, mes >= ini_ipca, mes <= fim_comum)

meses_pt <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
fm <- function(x) paste0(meses_pt[month(x)], "/", year(x))
num <- function(x, dig = 2) formatC(x, format = "f", digits = dig, decimal.mark = ",", big.mark = ".")
pct_lab <- function(x) paste0(formatC(x, format = "f", digits = 0, decimal.mark = ","), "%")
fonte <- paste0("Fonte: Banco Central do Brasil (SGS 27810, 27813 e 27815) e IBGE (SIDRA, tabela 7060). Elaboração própria. Dados até ", fm(fim_comum), ".")
pal <- c(M2 = "#0072B2", M3 = "#E69F00", M4 = "#009E73")

theme_set(theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold", size = 15), plot.subtitle = element_text(color = "grey30"),
  plot.caption = element_text(color = "grey40", hjust = 0, size = 9), panel.grid.minor = element_blank(),
  legend.position = "bottom", plot.title.position = "plot", plot.caption.position = "plot"))
salvar <- function(p, nome, altura = 6) ggsave(file.path("outputs/figures", nome), p, width = 10, height = altura, dpi = 200, bg = "white")

# Figura 11 — saldos (histórico completo da série)
g11 <- ggplot(d, aes(mes, saldo_tri, color = agregado)) +
  annotate("rect", xmin = ini_ipca, xmax = fim_m, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) num(x, 0)) +
  labs(title = "Agregados monetários amplos: M2, M3 e M4", x = NULL, y = "Saldo no fim do mês (R$ trilhões, valores correntes)",
       subtitle = "Os agregados são aninhados (M2 ⊂ M3 ⊂ M4). Área cinza = período do IPCA analisado no projeto.", caption = fonte)
salvar(g11, "11_m_saldos_historicos.png")

# Figura 12 — crescimento em 12 meses x IPCA em 12 meses
g12 <- ggplot() +
  geom_line(data = jan, aes(mes, cresc_12m, color = agregado), linewidth = 0.9) +
  geom_line(data = filter(jan, agregado == "M2", !is.na(ipca_12m)), aes(mes, ipca_12m), color = "black", linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_date(breaks = seq(as.Date("2020-01-01"), fim_comum, by = "1 year"), date_labels = "%Y") +
  scale_y_continuous(labels = pct_lab) +
  labs(title = "Crescimento dos agregados e inflação em 12 meses", x = NULL, y = "Variação acumulada em 12 meses (%)",
       subtitle = "Linha preta tracejada = IPCA acumulado em 12 meses (disponível a partir de dez/2020)", caption = fonte)
salvar(g12, "12_m_crescimento_12m_ipca.png")

# Figura 13 — taxas mensais
d13 <- jan |> select(mes, agregado, `Crescimento do agregado` = cresc_mensal, `IPCA mensal` = ipca_m) |>
  pivot_longer(c(`Crescimento do agregado`, `IPCA mensal`), names_to = "serie", values_to = "v")
g13 <- ggplot(d13, aes(mes, v, color = serie)) +
  geom_hline(yintercept = 0, color = "grey60") + geom_line(linewidth = 0.7) +
  facet_wrap(~ agregado, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Crescimento do agregado" = "#0072B2", "IPCA mensal" = "black"), name = NULL) +
  scale_x_date(breaks = seq(as.Date("2020-01-01"), fim_comum, by = "1 year"), date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")) +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Taxas de crescimento mensais: agregados x IPCA", x = NULL, y = "Variação mensal (%)",
       subtitle = "Saldos de fim de mês, sem ajuste sazonal (a sazonalidade afeta a taxa mensal)", caption = fonte)
salvar(g13, "13_m_crescimento_mensal_ipca.png", altura = 8)

# Figura 14 — correlação por defasagem
lim <- cors |> group_by(tipo) |> summarise(l = max(limiar_ruido), .groups = "drop")
rot <- c(mensal = "Taxas mensais (n = nº de meses)", `12m` = "Taxas em 12 meses (n efetivo ≈ n/12)")
c14 <- cors |> mutate(tipo = factor(rot[tipo], levels = rot)); lim <- lim |> mutate(tipo = factor(rot[tipo], levels = rot))
g14 <- ggplot(c14, aes(defasagem_meses, pearson, color = agregado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_hline(data = lim, aes(yintercept = l), linetype = "dotted") + geom_hline(data = lim, aes(yintercept = -l), linetype = "dotted") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.4) +
  facet_wrap(~ tipo) + scale_color_manual(values = pal, name = NULL) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + scale_y_continuous(limits = c(-1, 1)) +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Correlação entre o crescimento do agregado (t − k) e o IPCA (t)", x = "Defasagem k do agregado (meses)", y = "Correlação de Pearson",
       subtitle = "Linhas pontilhadas = limiar aproximado de ruído (2/√n). Associação e precedência temporal não provam causalidade.", caption = fonte)
salvar(g14, "14_m_correlacao_defasagens.png")

# Figura 15 — coeficientes das regressões (IC aproximado de 95%, erros HAC)
r15 <- regs |> filter(termo == "x") |> mutate(ymin = coef - 1.96 * ep_hac, ymax = coef + 1.96 * ep_hac)
g15 <- ggplot(r15, aes(defasagem_meses, coef, color = agregado)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_pointrange(aes(ymin = ymin, ymax = ymax), position = position_dodge(width = 1), size = 0.5) +
  facet_wrap(~ modelo) + scale_color_manual(values = pal, name = NULL) + scale_x_continuous(breaks = c(3, 6, 9, 12)) +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Regressões exploratórias: efeito defasado do agregado sobre o IPCA mensal", x = "Defasagem do agregado (meses)",
       y = "Coeficiente (p.p. de IPCA por p.p. de crescimento do agregado)",
       subtitle = "Pontos = coeficiente; barras = IC aproximado de 95% (Newey-West). Cada agregado em modelo separado.", caption = fonte)
salvar(g15, "15_m_coeficientes_regressao.png")
message("5 gráficos monetários salvos em outputs/figures/ (11 a 15)")
