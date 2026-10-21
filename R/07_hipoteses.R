# R/07_hipoteses.R
# Testa (de forma exploratória) as três hipóteses levantadas no relatório.
# Execução: Rscript R/07_hipoteses.R   (antes rode os scripts 01 a 06)
#
# LÓGICA: cada hipótese vira previsões que podem FALHAR, com limiares definidos ANTES de olhar os resultados
# (veja LIM abaixo). Os limiares são escolhas de julgamento, não regras estatísticas: altere-os e justifique.
# "Consistente" significa que os dados NÃO contradizem a hipótese; não significa que ela esteja provada.
#
# H1  Choques de oferta/custos pressionaram alimentos e transportes nos picos de inflação.
#     -> (a) poucos itens concentram a alta nos picos; (b) Alimentação+Transportes explicam a maior parte da alta
#        nesses meses. A parte CAUSAL (choque de oferta/custos) NÃO é testável só com o IPCA.
# H2  O padrão de fevereiro em Educação vem de reajustes de mensalidades.
#     -> (a) fevereiro é o mês de maior alta na maioria dos anos; (b) fevereiro responde pela maior parte da alta anual;
#        (c) o subgrupo de "cursos regulares" responde pela maior parte da contribuição de fevereiro.
# H3  Diferenças entre grupos refletem câmbio, preços administrados etc.
#     -> descritivo: preços livres x monitorados e correlação defasada dos grupos com a variação do câmbio.

suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
if (!dir.exists("R")) stop("Execute a partir da raiz do projeto (pasta que contém a pasta R/).", call. = FALSE)
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
if (!file.exists("data/processed/ipca_indices.csv")) stop("Rode antes: Rscript R/03_analise.R", call. = FALSE)

LIM <- list(h2_prop_anos_fev = 0.8, h2_share_fev = 0.5, h2_share_regulares = 0.7,
            h1_share_top5 = 0.5, h1_share_alim_transp = 0.5, h1_prop_picos = 0.6, residuo_itens_pp = 0.05)
ult <- function(padrao) { a <- sort(list.files("data/raw", padrao, full.names = TRUE)); if (length(a)) tail(a, 1) else NA_character_ }
arq_it <- ult("^sidra_7060_itens_[0-9]{8}_[0-9]{6}\\.csv$"); arq_cb <- ult("^bcb_sgs_1_[0-9]{8}_[0-9]{6}\\.csv$")
arq_lv <- ult("^bcb_sgs_11428_[0-9]{8}_[0-9]{6}\\.csv$"); arq_mo <- ult("^bcb_sgs_4449_[0-9]{8}_[0-9]{6}\\.csv$")
if (is.na(arq_it)) stop("Sem dados de itens. Rode antes: Rscript R/05_coleta_itens.R", call. = FALSE)
if (anyNA(c(arq_cb, arq_lv, arq_mo))) stop("Sem dados do BCB. Rode antes: Rscript R/06_coleta_bcb.R", call. = FALSE)
tab <- function(x, nome) { write_csv(x, file.path("outputs/tables", nome)); invisible(x) }
fm <- function(d) paste0(c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")[month(d)], "/", year(d))

ind <- read_csv("data/processed/ipca_indices.csv", show_col_types = FALSE)
gmap <- ind |> filter(tipo == "grupo") |> distinct(grupo, categoria) |>
  mutate(grupo_num = str_extract(categoria, "^[0-9]+")) |> select(grupo_num, grupo)

# ---- Itens e subgrupos: limpeza ----
it <- read_csv(arq_it, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
  mutate(valor = suppressWarnings(as.numeric(valor_txt)), data = as.Date(paste0(mes_cod, "01"), format = "%Y%m%d"),
         var_nome = unname(c("63" = "var_mensal", "66" = "peso_mensal")[variavel_cod])) |>
  filter(!is.na(var_nome), data >= as.Date("2020-01-01")) |>
  select(data, categoria_cod, categoria, var_nome, valor) |> distinct()
stopifnot(!anyDuplicated(it[c("data", "categoria_cod", "var_nome")]))
cat("Valores ausentes (NA) em itens/subgrupos:", sum(is.na(it$valor)), "de", nrow(it), "\n")
itens <- it |> pivot_wider(names_from = var_nome, values_from = valor) |>
  mutate(cod_num = str_extract(categoria, "^[0-9]+"), nivel = nchar(cod_num), grupo_num = substr(cod_num, 1, 1),
         nome = str_remove(categoria, "^[0-9]+\\.\\s*"), contrib_pp = peso_mensal * var_mensal / 100) |>
  left_join(gmap, by = "grupo_num")
if (anyNA(itens$grupo)) warning("Há categorias sem grupo identificado; confira a numeração.")
write_csv(itens, "data/processed/itens_contribuicao.csv")

# validação: soma das contribuições dos itens x contribuição do grupo
val <- itens |> filter(nivel == 4) |> group_by(grupo, data) |>
  summarise(soma_itens = sum(contrib_pp, na.rm = TRUE), n_na = sum(is.na(contrib_pp)), .groups = "drop") |>
  left_join(ind |> filter(tipo == "grupo") |> transmute(grupo, data, contrib_grupo = peso_mensal * var_mensal / 100), by = c("grupo", "data")) |>
  mutate(residuo = contrib_grupo - soma_itens)
val_res <- val |> group_by(grupo) |> summarise(max_abs_residuo_pp = max(abs(residuo)), mediana_abs_residuo_pp = median(abs(residuo)), meses_com_na = sum(n_na > 0))
tab(val_res, "h_validacao_itens.csv"); cat("\n== Validação: soma dos itens x grupo (p.p.) ==\n"); print(val_res)
if (any(val_res$max_abs_residuo_pp > LIM$residuo_itens_pp)) warning("Resíduo > ", LIM$residuo_itens_pp, " p.p. em algum grupo: itens podem não cobrir o grupo; interprete com cautela.")

# ================= H2: Educação em fevereiro =================
edu_num <- gmap$grupo_num[str_detect(gmap$grupo, "^Educa")]; stopifnot(length(edu_num) == 1)
edu <- ind |> filter(str_detect(grupo, "^Educa")) |> mutate(ano = year(data), mes = month(data))
por_ano <- edu |> group_by(ano) |> summarise(
  meses = n(), mes_maximo = mes[which.max(var_mensal)], var_fev = var_mensal[mes == 2][1], var_mar = var_mensal[mes == 3][1],
  var_ano_composta = (prod(1 + var_mensal / 100) - 1) * 100, .groups = "drop") |>
  mutate(completo = meses == 12, fev_e_maximo = mes_maximo == 2, share_fev = var_fev / var_ano_composta,
         var_fev_mar = ((1 + var_fev / 100) * (1 + var_mar / 100) - 1) * 100, share_fev_mar = var_fev_mar / var_ano_composta)
tab(por_ano, "h2_educacao_por_ano.csv"); cat("\n== H2: Educação por ano ==\n"); print(as.data.frame(por_ano))

edu_it <- itens |> filter(grupo_num == edu_num, nivel %in% c(2, 4)) |> mutate(fev = month(data) == 2)
tab_it <- edu_it |> group_by(nivel, nome) |> summarise(
  peso_medio = mean(peso_mensal, na.rm = TRUE), var_media_fev = mean(var_mensal[fev], na.rm = TRUE),
  var_media_demais_meses = mean(var_mensal[!fev], na.rm = TRUE), contrib_total_fev = sum(contrib_pp[fev], na.rm = TRUE), .groups = "drop") |>
  group_by(nivel) |> mutate(participacao_fev = contrib_total_fev / sum(contrib_total_fev)) |> ungroup() |> arrange(nivel, desc(participacao_fev))
tab(tab_it, "h2_educacao_itens_fevereiro.csv"); cat("\n== H2: subgrupos (nível 2) e itens (nível 4) de Educação ==\n"); print(as.data.frame(tab_it))

# Correção: Educação tem um único subgrupo (nível 2); o item "Cursos regulares" está no nível 4.
reg <- filter(tab_it, nivel == 4, str_detect(nome, regex("regular", ignore_case = TRUE)))
c1 <- mean(por_ano$fev_e_maximo); c2 <- median(por_ano$share_fev[por_ano$completo], na.rm = TRUE)
c3 <- if (nrow(reg)) sum(reg$participacao_fev) else NA_real_
h2c <- tibble(criterio = c("Proporção de anos em que fevereiro é o mês de maior alta de Educação",
                           "Mediana da razão (alta de fevereiro / alta do ano), anos completos",
                           "Participação do item 'Cursos regulares' na contribuição de fevereiro (itens de Educação)"),
              valor = c(c1, c2, c3), limiar = c(LIM$h2_prop_anos_fev, LIM$h2_share_fev, LIM$h2_share_regulares)) |>
  mutate(atendido = valor >= limiar)
tab(h2c, "h2_criterios.csv"); print(as.data.frame(h2c))
v2 <- if (any(h2c$atendido %in% FALSE)) "não consistente" else if (anyNA(h2c$atendido)) "inconclusivo (critério não avaliável)" else "consistente"

# ================= H1: picos =================
geral <- filter(ind, tipo == "geral"); grupos <- filter(ind, tipo == "grupo") |> mutate(contrib_pp = peso_mensal * var_mensal / 100)
picos <- geral |> slice_max(var_mensal, n = 5, with_ties = FALSE) |> transmute(data, var_geral = var_mensal) |> arrange(data)
pic_it <- itens |> filter(nivel == 4) |> inner_join(picos, by = "data") |> group_by(data) |>
  arrange(desc(contrib_pp), .by_group = TRUE) |> mutate(rank = row_number(), share_var_geral = contrib_pp / var_geral) |>
  filter(rank <= 8) |> ungroup() |> select(data, var_geral, rank, nome, grupo, var_mensal, peso_mensal, contrib_pp, share_var_geral)
tab(pic_it, "h1_picos_itens.csv"); cat("\n== H1: principais itens nos 5 maiores picos ==\n"); print(as.data.frame(pic_it))
pic_gr <- grupos |> inner_join(picos, by = "data") |> mutate(share_var_geral = contrib_pp / var_geral) |>
  select(data, grupo, var_mensal, contrib_pp, share_var_geral) |> arrange(data, desc(contrib_pp))
tab(pic_gr, "h1_picos_grupos.csv")
sh_top5 <- pic_it |> filter(rank <= 5) |> group_by(data) |> summarise(share_top5 = sum(contrib_pp) / first(var_geral), .groups = "drop")
sh_at <- pic_gr |> filter(str_detect(grupo, "^Alimenta|^Transp")) |> group_by(data) |> summarise(share_alim_transp = sum(share_var_geral), .groups = "drop")
h1p <- picos |> left_join(sh_top5, by = "data") |> left_join(sh_at, by = "data"); tab(h1p, "h1_picos_resumo.csv"); print(as.data.frame(h1p))
# Verificação de robustez (não altera os critérios): os picos são mais concentrados que um mês típico?
conc <- itens |> filter(nivel == 4) |> group_by(data) |>
  summarise(conc_top5 = sum(sort(contrib_pp, decreasing = TRUE)[1:5]) / sum(pmax(contrib_pp, 0), na.rm = TRUE), .groups = "drop") |>
  mutate(pico = data %in% picos$data)
tab(conc, "h1_concentracao_baseline.csv")
base_conc <- tibble(mediana_todos_meses = median(conc$conc_top5), mediana_picos = median(conc$conc_top5[conc$pico]))
tab(base_conc, "h1_concentracao_resumo.csv"); cat("\n== H1: concentração (5 maiores itens / contribuições positivas) ==\n"); print(as.data.frame(base_conc))
h1c <- tibble(criterio = c("Proporção de picos em que os 5 maiores itens explicam >= limiar da alta do IPCA",
                           "Proporção de picos em que Alimentação+Transportes explicam >= limiar da alta do IPCA"),
              valor = c(mean(h1p$share_top5 >= LIM$h1_share_top5, na.rm = TRUE), mean(h1p$share_alim_transp >= LIM$h1_share_alim_transp, na.rm = TRUE)),
              limiar = LIM$h1_prop_picos) |> mutate(atendido = valor >= limiar)
tab(h1c, "h1_criterios.csv"); print(as.data.frame(h1c))
v1 <- if (any(h1c$atendido %in% FALSE)) "não consistente (parte descritiva)" else "consistente (parte descritiva; causa não testada)"

# ================= H3: câmbio e preços livres/monitorados =================
rd_bcb <- function(a) read_csv(a, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
  mutate(data = dmy(data), valor = as.numeric(valor))
cb <- rd_bcb(arq_cb) |> mutate(mes = floor_date(data, "month")) |> group_by(mes) |>
  summarise(cambio_media = mean(valor, na.rm = TRUE), n_dias = n(), .groups = "drop") |> arrange(mes) |>
  mutate(cambio_var = (cambio_media / lag(cambio_media) - 1) * 100)
if (any(cb$n_dias < 15)) message("Aviso: meses de câmbio com menos de 15 dias úteis: ", paste(fm(cb$mes[cb$n_dias < 15]), collapse = ", "))
lv <- rd_bcb(arq_lv) |> transmute(mes = floor_date(data, "month"), livres = valor)
mo <- rd_bcb(arq_mo) |> transmute(mes = floor_date(data, "month"), monitorados = valor)
series <- geral |> transmute(mes = data, geral = var_mensal) |> left_join(lv, by = "mes") |> left_join(mo, by = "mes") |>
  left_join(select(cb, mes, cambio_media, cambio_var), by = "mes") |> arrange(mes)
if (anyNA(series$livres) || anyNA(series$monitorados)) warning("Faltam meses de preços livres/monitorados no BCB.")
write_csv(series, "data/processed/h3_series_mensais.csv")

acum <- function(v) (prod(1 + v / 100) - 1) * 100
lm_ano <- series |> mutate(ano = as.character(year(mes))) |> group_by(ano) |>
  summarise(across(c(geral, livres, monitorados), acum), meses = n(), .groups = "drop")
lm_tot <- series |> summarise(across(c(geral, livres, monitorados), acum), meses = n()) |> mutate(ano = "Período todo")
lm_tab <- bind_rows(lm_ano, lm_tot) |> select(ano, meses, geral, livres, monitorados); tab(lm_tab, "h3_livres_monitorados.csv")
cat("\n== H3: acumulado composto (%) por ano ==\n"); print(as.data.frame(lm_tab))

wide <- ind |> select(data, grupo, var_mensal) |> pivot_wider(names_from = grupo, values_from = var_mensal) |>
  rename(mes = data) |> arrange(mes) |> left_join(select(cb, mes, cambio_var), by = "mes")
cor_lag <- function(y, x, k) {
  n <- length(y); if (k > 0) { yy <- y[(k + 1):n]; xx <- x[1:(n - k)] } else { yy <- y; xx <- x }
  ok <- complete.cases(yy, xx); c(r = cor(yy[ok], xx[ok]), n = sum(ok))
}
grid <- expand.grid(grupo = setdiff(names(wide), c("mes", "cambio_var")), defasagem_meses = 0:6, stringsAsFactors = FALSE)
res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  cl <- cor_lag(wide[[grid$grupo[i]]], wide$cambio_var, grid$defasagem_meses[i])
  data.frame(grupo = grid$grupo[i], defasagem_meses = grid$defasagem_meses[i], correlacao = unname(cl["r"]), n = unname(cl["n"]))
}))
res <- as_tibble(res) |> mutate(limiar_ruido = 2 / sqrt(n), acima_limiar = abs(correlacao) > limiar_ruido)
tab(res, "h3_correlacao_cambio.csv")
melhores <- res |> group_by(grupo) |> slice_max(abs(correlacao), n = 1, with_ties = FALSE) |> ungroup() |> arrange(desc(abs(correlacao)))
cat("\n== H3: maior |correlação| de cada grupo com a variação do câmbio (defasagens 0-6) ==\n"); print(as.data.frame(melhores))
n_acima <- sum(res$acima_limiar, na.rm = TRUE)
h3r <- tibble(medida = c("Acumulado IPCA geral (%)", "Acumulado preços livres (%)", "Acumulado preços monitorados (%)",
                         "Correlações acima do limiar de ruído", "Total de correlações testadas", "Acima do limiar esperado só por acaso (~5%)"),
              valor = c(lm_tot$geral, lm_tot$livres, lm_tot$monitorados, n_acima, nrow(res), round(0.05 * nrow(res), 1)))
tab(h3r, "h3_resumo.csv"); print(as.data.frame(h3r))
v3 <- if (n_acima > 0.05 * nrow(res) * 2) "indício descritivo de relação com o câmbio (sem causalidade)" else "inconclusivo (poucas correlações acima do acaso)"

# ================= Veredito =================
verd <- tibble(hipotese = c("H1 - choques de oferta/custos em alimentos e transportes nos picos",
                            "H2 - Educação em fevereiro e reajuste de mensalidades",
                            "H3 - câmbio, preços livres e administrados"),
               veredito = c(v1, v2, v3),
               observacao = c("Testado: concentração da alta em poucos itens e peso de Alimentação+Transportes. Não testado: a causa (oferta/custos), que exige evidência externa.",
                              "Testado com os próprios dados do IBGE (itens e subgrupos). A confirmação do mecanismo (mensalidades) depende da metodologia do IBGE.",
                              "Apenas descritivo: ~70 correlações simultâneas, 81 meses, sem controles. Não prova causalidade."))
tab(verd, "veredito_hipoteses.csv"); cat("\n== VEREDITO ==\n"); print(as.data.frame(verd))
message("\nTabelas salvas em outputs/tables/ (h1_*, h2_*, h3_*, veredito_hipoteses.csv)")
