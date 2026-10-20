# Inflação e Custo de Vida no Brasil: uma análise exploratória do IPCA desde 2020

Projeto de portfólio em R (Linux Mint + VS Code, sem RStudio) com dados oficiais do IBGE/SIDRA.

**Pergunta:** quais grupos de consumo mais encareceram no Brasil desde 2020 e como se comparam ao IPCA geral?

## Fonte e período
- IBGE/SIDRA, tabela 7060 (IPCA), via API oficial: <https://sidra.ibge.gov.br/tabela/7060>
- Período: jan/2020 até o último mês disponível na coleta. **Data da coleta:** `PREENCHER` · **Último mês:** `PREENCHER`

## Tecnologias
R (≥ 4.1), `tidyverse`, `lubridate`, `httr2`, `jsonlite`, `janitor`, `scales`, `knitr`/`rmarkdown`, Quarto, Git/GitHub.

## Metodologia (resumo)
Índice base 100 = dez/2019, por composição: `100 × ∏(1 + v/100)`; aumento acumulado = índice − 100. Contribuição aproximada = peso × variação / 100 (simplificada, não é o impacto oficial do IBGE).

## Instalação (Linux Mint)
```bash
sudo apt update
sudo apt install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff-dev libjpeg-dev libicu-dev
Rscript -e 'install.packages(c("tidyverse","httr2","jsonlite","janitor","scales","knitr","rmarkdown","languageserver"), repos="https://cloud.r-project.org")'
```
Quarto: baixe o `.deb` em <https://quarto.org/docs/get-started/> e rode `sudo dpkg -i ~/Downloads/quarto-*-linux-amd64.deb`.
VS Code: extensões *R* (REditorSupport.r) e *Quarto*.

## Execução
```bash
Rscript R/00_verificar_ambiente.R
Rscript R/01_coleta.R
Rscript R/00_inspecao_dados.R   # confira a estrutura real
Rscript R/02_limpeza.R
Rscript R/03_analise.R
Rscript R/04_visualizacoes.R
quarto render analise_inflacao.qmd
```
Ou tudo de uma vez: `bash run_all.sh`.

## Gráficos
![Inflação mensal](outputs/figures/01_inflacao_mensal.png)
![Índice por grupo](outputs/figures/02_indice_acumulado_grupos.png)
![Ranking](outputs/figures/03_ranking_aumento_acumulado.png)
![Mapa de calor](outputs/figures/04_heatmap_inflacao_mensal.png)
![Peso e contribuição](outputs/figures/05_peso_contribuicao_grupos.png)

## Resultados
`PREENCHER após a execução, copiando números de outputs/tables/ (ranking_grupos.csv, estatisticas_geral.csv...)`.

## Estrutura
`R/` scripts numerados · `data/raw/` respostas brutas da API com carimbo de data (nunca sobrescritas) · `data/processed/` bases tratadas · `outputs/figures|tables/` resultados · `analise_inflacao.qmd` relatório.

## Limitações
Análise descritiva; poucos anos de dados; contribuição aproximada; IPCA é média de famílias de 1 a 40 salários mínimos.

## Publicar no GitHub
```bash
git init && git add . && git commit -m "Projeto IPCA: versão inicial"
git branch -M main
git remote add origin git@github.com:SEU_USUARIO/analise-inflacao-brasil.git
git push -u origin main
```
(crie antes o repositório vazio em github.com/new; use a URL HTTPS se não tiver chave SSH)

## Referências
IBGE, SIDRA tabela 7060; IBGE, metodologia do IPCA.
