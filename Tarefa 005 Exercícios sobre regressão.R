# Ariel Ladislau Reises

# =========================================================
# Exercício 1 – Auto MPG (Consumo de Combustível)
# =========================================================

# =========================================================
# 1. Configuração e Importação de Dados
# =========================================================

# Definindo diretório onde está o CSV (ajuste conforme necessário)
setwd("C:/Users/User/Documents/datasets")

# Carregando a base de dados
dados_carros <- read.csv("autos.csv")

# Visualizando as primeiras linhas
head(dados_carros)

# Verificando a estrutura dos dados
str(dados_carros)

# Resumo estatístico
summary(dados_carros)


# =========================================================
# 2. Limpeza e Tratamento de Dados
# =========================================================

# Verificando valores ausentes
colSums(is.na(dados_carros))

# Removendo linhas com valores ausentes (se houver)
dados_carros <- na.omit(dados_carros)

# Conferindo se não restaram NA
colSums(is.na(dados_carros))


# =========================================================
# 3. Análise Exploratória
# =========================================================

# Histograma do consumo de combustível (mpg)
hist(dados_carros$mpg,
     main="Distribuição do Consumo de Combustível (MPG)",
     xlab="Milhas por Galão",
     col="lightblue", border="black")

# Scatterplots para analisar relações com variáveis selecionadas
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + model_year + origin,
      data=dados_carros,
      main="Matriz de Dispersão - Auto MPG")

# Correlação entre variáveis numéricas
cor(dados_carros[, c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year","origin")])


# =========================================================
# 4. Modelagem: Regressão Linear Múltipla
# =========================================================

# Criando modelo linear para prever o consumo de combustível (mpg)
modelo_carros <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + model_year + origin,
                    data=dados_carros)

# Resumo do modelo
summary(modelo_carros)

# Interpretação:
# - Coeficiente positivo: aumenta o consumo (mpg)
# - Coeficiente negativo: diminui o consumo (mpg)
# - p-value indica significância estatística
# - R-squared indica quão bem o modelo explica a variação do consumo


# =========================================================
# 5. Avaliação do Modelo
# =========================================================

# Resíduos vs Valores Ajustados
plot(modelo_carros$fitted.values, modelo_carros$residuals,
     xlab="Valores Ajustados", ylab="Resíduos",
     main="Resíduos vs Valores Ajustados")
abline(h=0, col="red")

# Histograma dos resíduos
hist(modelo_carros$residuals,
     main="Histograma dos Resíduos",
     col="pink")

# Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(modelo_carros$residuals)

# Verificar multicolinearidade (VIF)
library(car)
vif(modelo_carros)


# =========================================================
# 6. Previsão de Novos Valores
# =========================================================

# Criando novo conjunto de dados hipotético
novos_carros <- data.frame(
  cylinders = c(4, 8),
  displacement = c(200, 400),
  horsepower = c(100, 300),
  weight = c(2200, 4000),
  acceleration = c(15, 12),
  model_year = c(80, 82),
  origin = c(1, 3)
)

# Previsão do consumo de combustível (mpg)
predict(modelo_carros, newdata = novos_carros)