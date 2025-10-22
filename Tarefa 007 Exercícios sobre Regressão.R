# Ariel Ladislau Reises

# =========================================================
# Exercício 3 – Expectativa de Vida (Indicadores de Saúde)
# =========================================================

# =========================================================
# 1. Configuração e Importação de Dados
# =========================================================

# Carregar a base de dados
dados_vida <- read.csv("life_expectancy.csv")

# Visualizando as primeiras linhas
head(dados_vida)

# Verificando a estrutura dos dados
str(dados_vida)

# Resumo estatístico
summary(dados_vida)


# =========================================================
# 2. Limpeza e Tratamento de Dados
# =========================================================

# Verificando valores ausentes
colSums(is.na(dados_vida))

# Removendo linhas com valores ausentes
dados_vida <- na.omit(dados_vida)

# Conferindo se não restaram NA
colSums(is.na(dados_vida))


# =========================================================
# 3. Análise Exploratória
# =========================================================

# Histograma da expectativa de vida
hist(dados_vida$Life_expectancy,
     main="Distribuição da Expectativa de Vida",
     xlab="Expectativa de Vida (anos)",
     col="lightblue", border="black")

# Scatterplots para analisar relações com outras variáveis
pairs(~Life_expectancy + Adult_mortality + GDP + Schooling + BMI + Alcohol,
      data=dados_vida,
      main="Matriz de Dispersão")

# Correlação entre variáveis numéricas
cor(dados_vida[, c("Life_expectancy", "Adult_mortality", "GDP", "Schooling", "BMI", "Alcohol")])


# =========================================================
# 4. Modelagem: Regressão Linear Múltipla
# =========================================================

# Criando modelo linear
modelo <- lm(Life_expectancy ~ Adult_mortality + GDP + Schooling + BMI + Alcohol, data=dados_vida)

# Resumo do modelo
summary(modelo)

# Explicação:
# - Coeficiente positivo: variável aumenta a expectativa de vida
# - Coeficiente negativo: variável diminui a expectativa de vida
# - p-value: indica significância estatística
# - R-squared: indica quão bem o modelo explica os dados


# =========================================================
# 5. Avaliação do Modelo
# =========================================================

# Resíduos vs Valores Ajustados
plot(modelo$fitted.values, modelo$residuals,
     xlab="Valores Ajustados", ylab="Resíduos",
     main="Resíduos vs Valores Ajustados")
abline(h=0, col="red")

# Histograma dos resíduos
hist(modelo$residuals,
     main="Histograma dos Resíduos",
     col="pink")

# Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(modelo$residuals)

# Verificando multicolinearidade (VIF)
library(car)
vif(modelo)


# =========================================================
# 6. Previsão de Novos Valores (opcional)
# =========================================================

# Criando novo conjunto de dados hipotético
novos_dados <- data.frame(
  Mortalidade_adulta = c(100, 150),
  PIB = c(10000, 20000),
  Escolaridade = c(12, 14),
  IMC = c(25, 27),
  Alcool = c(5, 3)
)

# Previsão de Expectativa de Vida
predict(modelo, newdata = novos_dados)
