
# 📊 Análise Estatística e Modelagem de Defeitos em Software OO

Este repositório contém um projeto completo de análise estatística, modelagem preditiva e aplicação web interativa desenvolvido em **R**, com base no dataset `dataset_KC1_classlevel_numdefect.xlsx`.

---

## Objetivo

Desenvolver um app para explorar e analisar estatisticamente métricas de classes de um sistema orientado a objetos, prever a quantidade de defeitos (`NUMDEFECTS`) com um modelo de regressão linear e disponibilizar a solução em uma **API REST com Plumber** e uma **aplicação Shiny interativa**, publicada no [shinyapps.io](https://www.shinyapps.io).

---

## 📁 Estrutura do Projeto

```bash
├── analise.R             # Script com análise descritiva, normalidade, correlação e regressão
├── plumber.R             # API REST em Plumber
├── app.R                 # APP Shiny para interação com o modelo
├── dataset_KC1_classlevel_numdefect.xlsx  # Dataset original
└── README.md             # README
```

---

## 🛠️ Etapas Realizadas

### 1. 📊 Análise Estatística Descritiva
- Média, mediana, moda
- Amplitude, desvio padrão, variância
- Quartis, histogramas e boxplots
- Testes de normalidade (Shapiro-Wilk, Anderson-Darling)

### 2. 📈 Correlação
- Matriz de correlação com `PerformanceAnalytics`
- Identificação de variáveis correlacionadas com `NUMDEFECTS`
- Gráficos de dispersão com linha de tendência

### 3. 🔢 Regressão Linear
- Modelo linear simples: `NUMDEFECTS ~ sumLOC_TOTAL`
- Análise dos coeficientes, R², diagnóstico de resíduos

### 4. 🌐 API REST com Plumber
- Endpoint: `/prever?sumLOC_TOTAL=valor`
- Retorna o número previsto de defeitos

### 5. 💻 Aplicativo Shiny
- Interface para inserção de métricas
- Previsão de defeitos com base no modelo
- Visualizações estatísticas

---

## Como Executar Localmente

### 🔹 Requisitos

- R 4.x instalado
- RStudio
- Pacotes R:
  ```r
  install.packages(c("readxl", "psych", "ggplot2", "ggpubr", 
                     "PerformanceAnalytics", "nortest", "plumber", "shiny", "tidyr"))
  ```

### 🔹 Executar a API REST

```r
library(plumber)
pr <- plumb("plumber.R")
pr$run(port = 8000)
```

Acesse: [http://localhost:8000/prever?sumLOC_TOTAL=1500](http://localhost:8000/prever?sumLOC_TOTAL=1500)

### 🔹 Executar a Aplicação Shiny

```r
runApp("app.R")
```

---

## 🌍 App Online

Publicado em:  
🔗 [https://renan-fig.shinyapps.io/app-defect-prediction-r/](https://renan-fig.shinyapps.io/app-defect-prediction-r/)

---

## 📚 Referências

- Dataset: NASA Metrics Data Program (KC1)
- Documentação dos pacotes: `plumber`, `shiny`, `ggplot2`, `PerformanceAnalytics`


