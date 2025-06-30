library(readxl)
library(psych)
library(ggplot2)
library(ggpubr)
library(PerformanceAnalytics)
library(nortest)
library(tidyr)

# Data Loading
dados <- read_excel("dataset_KC1_classlevel_numdefect.xlsx")

# Initial Exploration
View(dados)
str(dados)
summary(dados)

# Descriptive Statistics
describe(dados)

moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sapply(dados, function(x) if(is.numeric(x)) moda(x) else NA)

# Visualization
dados %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.8) +
  geom_density(color = "firebrick", size = 1) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 12) +
  labs(title = "Distribution Plots")

dados %>%
  gather(variable, value) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "goldenrod", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 12) +
  labs(title = "Boxplot Analysis")

# Normality Tests
sapply(dados, function(x) if(is.numeric(x)) shapiro.test(x)$p.value else NA)

# Correlation Analysis
chart.Correlation(
  dados[, sapply(dados, is.numeric)], 
  histogram = TRUE, 
  pch = 21,
  bg = "dodgerblue",
  col = "black"
)

correlacoes <- cor(dados[, sapply(dados, is.numeric)])
sort(correlacoes[,"NUMDEFECTS"], decreasing = TRUE)

# Regression Analysis
ggscatter(
  dados, 
  x = "sumLOC_TOTAL", 
  y = "NUMDEFECTS", 
  add = "reg.line", 
  conf.int = TRUE,
  color = "navy",
  size = 3
) +
  labs(title = "Defects vs LOC")

modelo <- lm(NUMDEFECTS ~ sumLOC_TOTAL, data = dados)
summary(modelo)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(modelo, pch = 19, col = "darkgreen")