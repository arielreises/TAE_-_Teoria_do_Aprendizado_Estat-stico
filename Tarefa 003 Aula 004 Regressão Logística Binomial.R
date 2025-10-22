# Ariel Ladislau Reises

# =========================================================
# Exercícios de Regressão Logística Binomial
# =========================================================

# =========================================================
# 0. Pacotes necessários
# =========================================================
# install.packages("ISLR")
# install.packages("ISLR2")
library(ISLR)
library(ISLR2)

# Função auxiliar para gerar vetor de predição qualitativa
vetor_predicao <- function(probabilidades, limite=0.5, classes=c("No","Yes")) {
  pred <- ifelse(probabilidades > limite, classes[2], classes[1])
  return(factor(pred, levels=classes))
}

# =========================================================
# a) ISLR::Default - variável dependente: student
# =========================================================
data_default <- Default
data_default$student <- as.factor(data_default$student)

# Regressão logística
modelo_default <- glm(student ~ ., data=data_default, family=binomial)

# Vetor de predição qualitativa
pred_default <- vetor_predicao(predict(modelo_default, type="response"), 
                               classes=c("No","Yes"))

# Visualizar as primeiras predições
head(pred_default)


# =========================================================
# b) ISLR2::Smarket - variável dependente: Direction
# =========================================================
data_smarket <- Smarket
data_smarket$Direction <- as.factor(data_smarket$Direction)

# Regressão logística
modelo_smarket <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                      data=data_smarket, family=binomial)

# Vetor de predição qualitativa
pred_smarket <- vetor_predicao(predict(modelo_smarket, type="response"), 
                               classes=c("Down","Up"))

# Visualizar as primeiras predições
head(pred_smarket)


# =========================================================
# c) ISLR2::Weekly - variável dependente: Direction
# =========================================================
data_weekly <- Weekly
data_weekly$Direction <- as.factor(data_weekly$Direction)

# Regressão logística
modelo_weekly <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                     data=data_weekly, family=binomial)

# Vetor de predição qualitativa
pred_weekly <- vetor_predicao(predict(modelo_weekly, type="response"), 
                              classes=c("Down","Up"))

# Visualizar as primeiras predições
head(pred_weekly)


# =========================================================
# d) ISLR2::Caravan - variável dependente: Purchase
# =========================================================
data_caravan <- Caravan
data_caravan$Purchase <- as.factor(data_caravan$Purchase)

# Regressão logística
modelo_caravan <- glm(Purchase ~ ., data=data_caravan, family=binomial)

# Vetor de predição qualitativa
pred_caravan <- vetor_predicao(predict(modelo_caravan, type="response"), 
                               classes=c("No","Yes"))

# Visualizar as primeiras predições
head(pred_caravan)
