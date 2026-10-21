#!/usr/bin/env bash
# Executa o pipeline completo. Uso: bash run_all.sh
set -euo pipefail
mkdir -p data data/raw outputs outputs/figures outputs/tables
Rscript R/00_verificar_ambiente.R
Rscript R/01_coleta.R
Rscript R/02_limpeza.R
Rscript R/03_analise.R
Rscript R/04_visualizacoes.R
Rscript R/05_coleta_itens.R
Rscript R/06_coleta_bcb.R
Rscript R/07_hipoteses.R
Rscript R/08_visualizacoes_hipoteses.R
if command -v quarto >/dev/null; then quarto render analise_inflacao.qmd; else echo "Quarto ausente: relatório não gerado."; fi
