# Inflação e Custo de Vida no Brasil: uma análise exploratória do IPCA desde 2020

Projeto de Ciência de Dados em **R** com dados oficiais do **IBGE/SIDRA**, desenvolvido em Linux Mint com Visual Studio Code (sem RStudio). O pipeline é reproduzível: coleta pela API, limpeza, análise, cinco gráficos e um relatório HTML em Quarto.

📄 **Relatório completo:** [`analise_inflacao.html`](analise_inflacao.html) (baixe e abra no navegador)

## Pergunta de pesquisa

> Quais grupos de consumo mais encareceram no Brasil desde 2020, e como sua evolução se compara à inflação geral medida pelo IPCA?

Perguntas secundárias: evolução da inflação mensal, maiores picos, padrões sazonais aparentes, peso dos grupos e contribuição de cada um para a inflação geral.

## Objetivos

- Coletar o IPCA diretamente da API oficial do SIDRA, preservando os dados brutos.
- Construir um índice de preços por grupo por **composição** das variações mensais (base 100 = dez/2019), sem somar percentuais.
- Comparar os nove grupos principais com o IPCA geral.
- Estimar, de forma explicitamente simplificada, a contribuição de cada grupo.

## Fonte e período dos dados

- **Fonte:** IBGE, SIDRA, [tabela 7060 (IPCA)](https://sidra.ibge.gov.br/tabela/7060), obtida pela API oficial.
- **Variáveis:** variação mensal (63), peso mensal (66), acumulada no ano (69) e acumulada em 12 meses (2265).
- **Categorias:** índice geral e os nove grupos principais (subgrupos, itens e subitens não foram usados).
- **Período:** jan/2020 a set/2026 (81 meses).
- **Data da coleta:** 19/10/2026.

## Tecnologias

R (≥ 4.1), `tidyverse` (dplyr, ggplot2, tidyr, readr), `lubridate`, `httr2`, `jsonlite`, `janitor`, `scales`, `knitr`/`rmarkdown`, Quarto, Git e GitHub.

## Metodologia

- **Índice de preços:** `Índice = 100 × (1 + v₁/100) × … × (1 + vₜ/100)`, em que *v* é a variação mensal em %. O **aumento acumulado** é o índice final menos 100.
- **Validação:** os acumulados recalculados foram comparados aos publicados pelo IBGE; a maior diferença absoluta em 12 meses foi de 0,03 p.p. (compatível com arredondamento a duas casas).
- **Contribuição aproximada:** `peso × variação / 100`, somada mês a mês. É uma **estimativa simplificada**, não o impacto oficial do IBGE: por mês reproduz o IPCA geral com resíduo máximo de 0,007 p.p., mas a soma de contribuições mensais não equivale à contribuição composta.
- **Valores ausentes:** o SIDRA marca com `...` o acumulado em 12 meses dos primeiros 11 meses (a série começa em jan/2020). Eles viram `NA` e não foram removidos.

## Principais resultados

| Indicador | Resultado |
|---|---|
| IPCA geral acumulado (jan/2020 a set/2026) | **44,7%** |
| Inflação mensal média / mediana | 0,46% / 0,43% |
| Maior variação mensal | 1,62% (mar/2022) |
| Menor variação mensal | −0,68% (jul/2022) |
| Maior acumulado em 12 meses | 12,13% (abr/2022) |

**Aumento acumulado por grupo**

| Posição | Grupo | Aumento acumulado | Peso médio |
|---|---|---|---|
| 1 | Alimentação e bebidas | 60,7% | 21,2% |
| 2 | Saúde e cuidados pessoais | 46,7% | 13,3% |
| 3 | Vestuário | 45,6% | 4,6% |
| 4 | Educação | 45,1% | 6,0% |
| 5 | Habitação | 43,8% | 15,4% |
| 6 | Transportes | 41,6% | 20,6% |
| 7 | Despesas pessoais | 39,5% | 10,2% |
| 8 | Artigos de residência | 33,5% | 3,7% |
| 9 | Comunicação | 13,8% | 5,0% |

- Alimentação e bebidas encareceu 16,1 p.p. a mais que o IPCA geral e superou o índice geral em 47 dos 81 meses. A distância máxima entre os dois índices foi de 18,4 pontos, em abr/2025.
- As cinco maiores variações mensais do IPCA geral ocorreram em dez/2020, set/2021, out/2021, mar/2022 e fev/2025.
- As maiores médias mensais do IPCA geral ocorrem em fevereiro, março e dezembro, e as menores em agosto e julho. Todas as cinco maiores altas mensais de grupos individuais são de Educação em fevereiro. Com 6 a 7 observações por mês, são indícios, não conclusões estatísticas.
- Alimentação e bebidas, Transportes e Habitação respondem por 62% da contribuição aproximada.

## Gráficos

![Inflação mensal do IPCA](outputs/figures/01_inflacao_mensal.png)
![Índice de preços por grupo](outputs/figures/02_indice_acumulado_grupos.png)
![Ranking de aumento acumulado](outputs/figures/03_ranking_aumento_acumulado.png)
![Mapa de calor da inflação mensal](outputs/figures/04_heatmap_inflacao_mensal.png)
![Peso e contribuição aproximada](outputs/figures/05_peso_contribuicao_grupos.png)

## Como instalar

Linux Mint (Ubuntu):

```bash
sudo apt update
sudo apt install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff-dev libjpeg-dev libicu-dev

Rscript -e 'install.packages(c("tidyverse","httr2","jsonlite","janitor","scales","knitr","rmarkdown","languageserver"), repos="https://cloud.r-project.org")'
```

- **Quarto:** baixe o `.deb` em <https://quarto.org/docs/get-started/> e instale com `sudo dpkg -i quarto-*-linux-amd64.deb`.
- **VS Code:** instale as extensões *R* (REditorSupport.r) e *Quarto*.

## Como executar

Na raiz do projeto, no terminal integrado do VS Code:

```bash
Rscript R/00_verificar_ambiente.R   # confere pacotes, gráficos e acesso à API
Rscript R/01_coleta.R               # coleta o IPCA e grava o JSON bruto em data/raw/
Rscript R/00_inspecao_dados.R       # (opcional) mostra a estrutura real dos dados
Rscript R/02_limpeza.R              # gera data/processed/ipca_tratado.csv
Rscript R/03_analise.R              # gera as tabelas em outputs/tables/
Rscript R/04_visualizacoes.R        # gera os cinco gráficos em outputs/figures/
quarto render analise_inflacao.qmd  # gera analise_inflacao.html
```

Ou tudo de uma vez: `bash run_all.sh`.

Cada execução da coleta grava um novo arquivo com data e hora em `data/raw/`; nada é sobrescrito. A limpeza usa sempre o arquivo bruto mais recente.

## Estrutura do repositório

```
.
├── README.md
├── analise_inflacao.qmd      # relatório reproduzível (Quarto)
├── analise_inflacao.html     # relatório renderizado
├── run_all.sh                # executa o pipeline completo
├── R/
│   ├── 00_verificar_ambiente.R
│   ├── 00_inspecao_dados.R
│   ├── 01_coleta.R
│   ├── 02_limpeza.R
│   ├── 03_analise.R
│   └── 04_visualizacoes.R
├── data/
│   ├── raw/                  # respostas brutas da API (com carimbo de data)
│   └── processed/            # bases tratadas
└── outputs/
    ├── figures/              # gráficos em PNG
    └── tables/               # tabelas de resultados em CSV
```

## Limitações

- Análise **descritiva**: não estabelece causalidade nem testa explicações econômicas.
- Poucos anos de dados, com choques atípicos no período; sazonalidade e correlações entre grupos (n = 9) são frágeis.
- A contribuição é uma aproximação, não o impacto oficial do IBGE.
- O IPCA é um índice agregado, calculado para famílias urbanas com rendimento de 1 a 40 salários mínimos. A inflação sentida por cada família depende da composição de seus gastos.
- Os grupos escondem variações entre itens, subgrupos e regiões.
- Os dados são de um único momento de coleta e podem ser revisados pelo IBGE.

## Referências

- IBGE. SIDRA, Tabela 7060: IPCA. <https://sidra.ibge.gov.br/tabela/7060>
- IBGE. IPCA, Índice Nacional de Preços ao Consumidor Amplo. <https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html>

## Autor

[PhOlivs](https://github.com/PhOlivs)
