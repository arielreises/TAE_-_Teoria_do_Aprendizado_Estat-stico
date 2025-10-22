# Ariel Ladislau Reises

# =========================================================
# Exercício 2 – Boston Housing (Preço de Imóveis)
# =========================================================

# =========================================================
# 1. Configuração e Importação de Dados
# =========================================================

# Definindo diretório onde está o CSV
setwd("C:/Users/User/Documents/datasets")

# Carregando a base de dados
dados_imoveis <- read.csv("boston_housing.csv")

# Visualizando as primeiras linhas
head(dados_imoveis)

# Verificando a estrutura dos dados
str(dados_imoveis)

# Resumo estatístico
summary(dados_imoveis)


# =========================================================
# 2. Limpeza e Tratamento de Dados
# =========================================================

# Verificando valores ausentes
colSums(is.na(dados_imoveis))

# Removendo linhas com valores ausentes (se houver)
dados_imoveis <- na.omit(dados_imoveis)

# Conferindo se não restaram NA
colSums(is.na(dados_imoveis))


# =========================================================
# 3. Análise Exploratória
# =========================================================

# Histograma do valor mediano das casas
hist(dados_imoveis$medv,
     main="Distribuição do Valor Mediano das Casas",
     xlab="Valor Mediano (milhares de dólares)",
     col="lightgreen", border="black")

# Scatterplots para analisar relações com variáveis selecionadas
pairs(~medv + crim + nox + rm + age + tax + lstat,
      data=dados_imoveis,
      main="Matriz de Dispersão - Boston Housing")

# Correlação entre variáveis numéricas
cor(dados_imoveis[, c("medv", "crim", "nox", "rm", "age", "tax", "lstat")])


# =========================================================
# 4. Modelagem: Regressão Linear Múltipla
# =========================================================

# Criando modelo linear para prever o preço das casas (medv)
modelo_casas <- lm(medv ~ crim + nox + rm + age + tax + lstat, data=dados_imoveis)

# Resumo do modelo
summary(modelo_casas)

# Interpretação:
# - Coeficiente positivo: aumenta o valor mediano
# - Coeficiente negativo: diminui o valor mediano
# - p-value indica significância estatística
# - R-squared indica quão bem o modelo explica a variação do preço


# =========================================================
# 5. Avaliação do Modelo
# =========================================================

# Resíduos vs Valores Ajustados
plot(modelo_casas$fitted.values, modelo_casas$residuals,
     xlab="Valores Ajustados", ylab="Resíduos",
     main="Resíduos vs Valores Ajustados")
abline(h=0, col="red")

# Histograma dos resíduos
hist(modelo_casas$residuals,
     main="Histograma dos Resíduos",
     col="pink")

# Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(modelo_casas$residuals)

# Verificando multicolinearidade (VIF)
library(car)
vif(modelo_casas)


# =========================================================
# 6. Previsão de Novos Valores (opcional)
# =========================================================

# Criando novo conjunto de dados hipotético
novos_imoveis <- data.frame(
  crim = c(0.5, 3.0),
  nox = c(0.4, 0.7),
  rm = c(6, 8),
  age = c(30, 70),
  tax = c(300, 600),
  lstat = c(5, 25)
)

# Previsão do valor mediano das casas
predict(modelo_casas, newdata = novos_imoveis)