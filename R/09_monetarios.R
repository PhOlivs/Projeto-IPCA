# R/09_monetarios.R
# Agregados monetários (M2, M3, M4) e sua relação com o IPCA. Execução: Rscript R/09_monetarios.R
# Pré-requisitos: scripts 02-03 (IPCA) e  Rscript R/06_coleta_bcb.R monetarios  (ou o 06 completo).
# O IPCA vem do projeto (data/processed/ipca_indices.csv, SIDRA 7060); só os agregados são dados novos (BCB/SGS).
#
# SÉRIES (conferidas no catálogo do BCB; mensais, saldo em final de período, milhares de R$ correntes):
#   27810 = M2 | 27813 = M3 | 27815 = M4   ("Novo": revisão histórica desde dez/2001)
#   M2 = M1 + depósitos de poupança + títulos privados de instituições depositárias
#   M3 = M2 + quotas de fundos de investimento depositários + operações compromissadas
#   M4 = M3 + títulos públicos federais   ->  os agregados são ANINHADOS (componentes sobrepostos)
#
# CAUTELAS (também repetidas no relatório):
#  - correlação/regressão = ASSOCIAÇÃO; defasagem que "antecede" = PRECEDÊNCIA TEMPORAL; nenhuma das duas prova CAUSALIDADE.
#  - M2, M3 e M4 nunca entram juntos numa regressão (colinearidade); cada um é analisado separadamente.
#  - Taxas acumuladas em 12 meses se sobrepõem (forte autocorrelação): servem para descrever, não para inferência.
#    Para elas o limiar de ruído usa n efetivo aproximado = n/12.
#  - A amostra do IPCA do projeto é curta (jan/2020 em diante) e inclui o choque da pandemia: tudo é exploratório.
#  - Séries não dessazonalizadas: a sazonalidade da taxa mensal é um possível fator de confusão (não controlada).

suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
if (!file.exists("data/processed/ipca_indices.csv")) stop("Rode antes: Rscript R/03_analise.R", call. = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE); dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
tab <- function(x, nome) { write_csv(x, file.path("outputs/tables", nome)); invisible(x) }
ult <- function(padrao) { a <- sort(list.files("data/raw", padrao, full.names = TRUE)); if (length(a)) tail(a, 1) else NA_character_ }
fm <- function(d) paste0(c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")[month(d)], "/", year(d))

KS <- c(0, 3, 6, 9, 12)                 # defasagens (meses); 0 = referência contemporânea
ADF_CRITICO_5PCT <- -2.86               # Dickey-Fuller com constante, valor assintótico (MacKinnon)
AGREGADOS <- tibble(agregado = c("M2", "M3", "M4"), codigo = c(27810, 27813, 27815))

# ---- 1) Agregados: leitura e validação ----
ler_agregado <- function(ag, cod) {
  a <- ult(sprintf("^bcb_sgs_%s_[0-9]{8}_[0-9]{6}\\.csv$", cod))
  if (is.na(a)) stop("Sem dados de ", ag, " (SGS ", cod, "). Rode antes: Rscript R/06_coleta_bcb.R monetarios", call. = FALSE)
  read_csv(a, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
    transmute(mes = floor_date(dmy(data), "month"), agregado = ag, saldo_mil_reais = as.numeric(valor))
}
m <- bind_rows(Map(ler_agregado, AGREGADOS$agregado, AGREGADOS$codigo)) |> arrange(agregado, mes)
stopifnot(!anyNA(m$mes), !anyNA(m$saldo_mil_reais), !anyDuplicated(m[c("agregado", "mes")]))
val <- m |> group_by(agregado) |>
  summarise(primeiro_mes = min(mes), ultimo_mes = max(mes), n_meses = n(), .groups = "drop") |>
  mutate(n_esperado = (year(ultimo_mes) - year(primeiro_mes)) * 12 + month(ultimo_mes) - month(primeiro_mes) + 1, completo = n_meses == n_esperado) |>
  left_join(AGREGADOS, by = "agregado")
hier <- m |> pivot_wider(names_from = agregado, values_from = saldo_mil_reais)
viol <- sum(hier$M2 > hier$M3 | hier$M3 > hier$M4, na.rm = TRUE)
val$violacoes_hierarquia_M2_M3_M4 <- viol
tab(val, "m_validacao.csv"); cat("== Validação das séries ==\n"); print(as.data.frame(val))
if (!all(val$completo)) stop("Há meses faltando nas séries de agregados monetários.", call. = FALSE)
if (viol > 0) warning(viol, " meses violam M2 <= M3 <= M4; confira as séries (pela definição, os agregados são aninhados).")

m <- m |> group_by(agregado) |>
  mutate(saldo_tri = saldo_mil_reais * 1000 / 1e12,   # milhares de R$ -> R$ trilhões
         cresc_mensal = (saldo_mil_reais / lag(saldo_mil_reais) - 1) * 100,
         cresc_12m = (saldo_mil_reais / lag(saldo_mil_reais, 12) - 1) * 100) |> ungroup()

# ---- 2) IPCA do projeto (e câmbio, se já coletado, como controle) ----
ind <- read_csv("data/processed/ipca_indices.csv", show_col_types = FALSE)
geral <- ind |> filter(tipo == "geral") |> transmute(mes = data, ipca_m = var_mensal, ipca_12m = acum_12m)
ini_ipca <- min(geral$mes); fim_comum <- min(max(geral$mes), max(m$mes))
cat("\nJanela da análise (IPCA do projeto x agregados):", fm(ini_ipca), "a", fm(fim_comum), "\n")
cb <- NULL
if (file.exists("data/processed/h3_series_mensais.csv")) {
  cb <- read_csv("data/processed/h3_series_mensais.csv", show_col_types = FALSE) |> select(mes, cambio_var)
} else message("Aviso: sem data/processed/h3_series_mensais.csv; modelos com câmbio serão pulados (rode 06 completo e 07).")

mw <- m |> select(mes, agregado, cresc_mensal, cresc_12m) |>
  pivot_wider(names_from = agregado, values_from = c(cresc_mensal, cresc_12m), names_glue = "{.value}_{agregado}")
full <- mw |> left_join(geral, by = "mes") |> arrange(mes)
if (!is.null(cb)) full <- left_join(full, cb, by = "mes")
amostra <- filter(full, mes >= ini_ipca, mes <= fim_comum)
write_csv(m |> left_join(geral, by = "mes") |> select(mes, agregado, saldo_tri, cresc_mensal, cresc_12m, ipca_m, ipca_12m),
          "data/processed/monetarios_ipca.csv")

# ---- 3) Resumo do crescimento na janela ----
dez <- ini_ipca %m-% months(1)
ipca_acum <- (prod(1 + geral$ipca_m[geral$mes <= fim_comum] / 100) - 1) * 100
resumo <- m |> filter(mes >= ini_ipca, mes <= fim_comum) |> group_by(agregado) |>
  summarise(media_mensal = mean(cresc_mensal), mediana_mensal = median(cresc_mensal), dp_mensal = sd(cresc_mensal),
            max_12m = max(cresc_12m, na.rm = TRUE), mes_max_12m = mes[which.max(cresc_12m)], ultimo_12m = last(cresc_12m), .groups = "drop") |>
  left_join(m |> filter(mes == dez) |> select(agregado, saldo_inicial = saldo_mil_reais), by = "agregado") |>
  left_join(m |> filter(mes == fim_comum) |> select(agregado, saldo_final = saldo_mil_reais, saldo_final_tri = saldo_tri), by = "agregado") |>
  mutate(janela_inicio = ini_ipca, janela_fim = fim_comum, acumulado_pct = (saldo_final / saldo_inicial - 1) * 100,
         ipca_acumulado_pct = ipca_acum, acumulado_real_pct = ((1 + acumulado_pct / 100) / (1 + ipca_acum / 100) - 1) * 100) |>
  select(-saldo_inicial, -saldo_final)
tab(resumo, "m_resumo_crescimento.csv"); cat("\n== Crescimento dos agregados na janela ==\n"); print(as.data.frame(resumo))

# ---- 4) Correlações com defasagens (agregado em t-k x IPCA em t) ----
cor_defas <- function(tipo, ag, k, de = ini_ipca, ate = fim_comum) {
  y <- full[[if (tipo == "mensal") "ipca_m" else "ipca_12m"]]
  x <- dplyr::lag(full[[paste0("cresc_", tipo, "_", ag)]], k)
  ok <- complete.cases(x, y) & full$mes >= de & full$mes <= ate
  tibble(tipo = tipo, agregado = ag, defasagem_meses = k, n = sum(ok),
         pearson = if (sum(ok) > 3) cor(x[ok], y[ok]) else NA_real_, spearman = if (sum(ok) > 3) cor(x[ok], y[ok], method = "spearman") else NA_real_)
}
grade <- expand_grid(tipo = c("mensal", "12m"), agregado = AGREGADOS$agregado, k = KS)
cors <- bind_rows(pmap(grade, function(tipo, agregado, k) cor_defas(tipo, agregado, k))) |>
  mutate(n_efetivo = if_else(tipo == "12m", n / 12, as.numeric(n)), limiar_ruido = 2 / sqrt(n_efetivo), acima_limiar = abs(pearson) > limiar_ruido)
tab(cors, "m_correlacoes_defasagens.csv"); cat("\n== Correlações (Pearson) por defasagem ==\n"); print(as.data.frame(cors |> select(tipo, agregado, defasagem_meses, n, pearson, spearman, limiar_ruido, acima_limiar)))

per <- list(c("2020-2022", as.character(ini_ipca), "2022-12-31"), c("2023 em diante", "2023-01-01", as.character(fim_comum)))
cors_sub <- bind_rows(lapply(per, function(p) bind_rows(pmap(grade |> filter(tipo == "mensal"),
  function(tipo, agregado, k) cor_defas(tipo, agregado, k, as.Date(p[2]), as.Date(p[3])))) |> mutate(periodo = p[1])))
tab(cors_sub, "m_correlacoes_subperiodos.csv")

cor_ag <- bind_rows(lapply(c("mensal", "12m"), function(tp) bind_rows(lapply(combn(AGREGADOS$agregado, 2, simplify = FALSE), function(pr) {
  a <- amostra[[paste0("cresc_", tp, "_", pr[1])]]; b <- amostra[[paste0("cresc_", tp, "_", pr[2])]]; ok <- complete.cases(a, b)
  tibble(tipo = tp, par = paste(pr, collapse = " x "), correlacao = cor(a[ok], b[ok]), n = sum(ok))
}))))
tab(cor_ag, "m_correlacao_entre_agregados.csv"); cat("\n== Correlação entre os próprios agregados (sobreposição) ==\n"); print(as.data.frame(cor_ag))

# ---- 5) Estacionariedade (Dickey-Fuller aumentado, 3 defasagens; triagem de baixo poder com n ~ 70-80) ----
adf_t <- function(y, p = 3) {
  y <- y[!is.na(y)]; n <- length(y); dy <- diff(y); j <- (p + 1):(n - 1)
  d <- data.frame(dy = dy[j], ylag = y[j]); for (i in seq_len(p)) d[[paste0("dl", i)]] <- dy[j - i]
  summary(lm(dy ~ ., data = d))$coefficients["ylag", "t value"]
}
vars_adf <- c("ipca_m", "ipca_12m", paste0("cresc_mensal_", AGREGADOS$agregado), paste0("cresc_12m_", AGREGADOS$agregado), if (!is.null(cb)) "cambio_var")
est <- tibble(serie = vars_adf) |> mutate(n = map_int(serie, ~ sum(!is.na(amostra[[.x]]))), t_adf = map_dbl(serie, ~ adf_t(amostra[[.x]])),
  critico_5pct = ADF_CRITICO_5PCT, rejeita_raiz_unitaria_5pct = t_adf < ADF_CRITICO_5PCT)
tab(est, "m_estacionariedade.csv"); cat("\n== Estacionariedade (ADF, constante) ==\n"); print(as.data.frame(est))

# ---- 6) Regressões exploratórias (erros HAC de Newey-West; M2, M3, M4 separadamente) ----
ols_hac <- function(formula, dados) {
  fit <- lm(formula, data = dados); X <- model.matrix(fit); u <- residuals(fit); n <- nrow(X); L <- floor(4 * (n / 100)^(2 / 9))
  A <- X * u; S <- crossprod(A) / n
  for (l in seq_len(L)) {
    G <- crossprod(A[(l + 1):n, , drop = FALSE], A[1:(n - l), , drop = FALSE]) / n
    S <- S + (1 - l / (L + 1)) * (G + t(G))
  }
  B <- solve(crossprod(X) / n); V <- B %*% S %*% B / n; b <- coef(fit); se <- sqrt(diag(V))
  tibble(termo = names(b), coef = unname(b), ep_hac = unname(se), z = unname(b) / unname(se), p_aprox = 2 * pnorm(-abs(unname(b) / unname(se))),
         n = n, lag_hac = L, r2 = summary(fit)$r.squared)
}
reg_um <- function(ag, k, modelo) {
  d <- full |> mutate(y = ipca_m, x = dplyr::lag(.data[[paste0("cresc_mensal_", ag)]], k), y1 = dplyr::lag(ipca_m, 1))
  if (!is.null(cb)) d <- mutate(d, fx = dplyr::lag(cambio_var, k))
  d <- filter(d, mes >= ini_ipca, mes <= fim_comum)
  f <- if (modelo == "1: simples") y ~ x else y ~ x + fx + y1
  d <- d[complete.cases(d[, all.vars(f)]), ]
  ols_hac(f, d) |> mutate(agregado = ag, defasagem_meses = k, modelo = modelo)
}
gr_reg <- expand_grid(ag = AGREGADOS$agregado, k = c(3, 6, 9, 12), modelo = c("1: simples", if (!is.null(cb)) "2: com câmbio e IPCA(t-1)"))
regs <- bind_rows(pmap(gr_reg, function(ag, k, modelo) reg_um(ag, k, modelo)))
tab(regs, "m_regressoes.csv"); cat("\n== Regressões: coeficiente do agregado defasado (IPCA mensal em p.p. por 1 p.p. de crescimento mensal) ==\n")
print(as.data.frame(regs |> filter(termo == "x") |> select(modelo, agregado, defasagem_meses, n, coef, ep_hac, p_aprox, r2)))

# ---- 7) Precedência temporal: testes do tipo Granger (F), nos dois sentidos ----
granger <- function(direcao, ag, p) {
  d <- full |> mutate(ipca = ipca_m, mon = .data[[paste0("cresc_mensal_", ag)]])
  for (i in seq_len(p)) { d[[paste0("ipca_l", i)]] <- dplyr::lag(d$ipca, i); d[[paste0("mon_l", i)]] <- dplyr::lag(d$mon, i) }
  d <- filter(d, mes >= ini_ipca, mes <= fim_comum)
  ip_dep <- direcao == "agregado -> IPCA"; y <- if (ip_dep) "ipca" else "mon"
  own <- paste0(if (ip_dep) "ipca_l" else "mon_l", seq_len(p)); oth <- paste0(if (ip_dep) "mon_l" else "ipca_l", seq_len(p))
  d <- d[complete.cases(d[, c(y, own, oth)]), ]
  a <- anova(lm(reformulate(own, y), d), lm(reformulate(c(own, oth), y), d))
  tibble(direcao = direcao, agregado = ag, p_defasagens = p, n = nrow(d), F = a$F[2], p_valor = a$`Pr(>F)`[2])
}
gr_g <- expand_grid(direcao = c("agregado -> IPCA", "IPCA -> agregado"), ag = AGREGADOS$agregado, p = c(3, 6))
gc <- bind_rows(pmap(gr_g, function(direcao, ag, p) granger(direcao, ag, p)))
tab(gc, "m_granger.csv"); cat("\n== Testes tipo Granger (precedência, não causalidade) ==\n"); print(as.data.frame(gc))
message("\nTabelas salvas em outputs/tables/ (m_*) e data/processed/monetarios_ipca.csv")
