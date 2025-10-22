# Ariel Ladislau Reises

# =========================================================
# Exercícios de Regressão Logística Binomial
# =========================================================

# =========================================================
# 0. Instalar e carregar pacotes necessários
# =========================================================
# install.packages("ISLR")
# install.packages("ISLR2")
# install.packages("caret")  # para matriz de confusão
library(ISLR)
library(ISLR2)
library(caret)

# Função para avaliar modelo: Acurácia, Sensibilidade e Especificidade
avaliar_modelo <- function(predicoes, verdadeiros) {
  cm <- confusionMatrix(predicoes, verdadeiros, positive="Yes")
  cat("Matriz de Confusão:\n")
  print(cm$table)
  
  cat("\nAcurácia:", round(cm$overall['Accuracy'], 3), "\n")
  cat("Sensibilidade (Recall):", round(cm$byClass['Sensitivity'], 3), "\n")
  cat("Especificidade:", round(cm$byClass['Specificity'], 3), "\n\n")
  
  # Resposta qualitativa
  cat("Interpretação qualitativa:\n")
  if(cm$overall['Accuracy'] >= 0.8) cat("- Acurácia alta\n") else cat("- Acurácia moderada ou baixa\n")
  if(cm$byClass['Sensitivity'] >= 0.8) cat("- O modelo detecta bem os casos positivos\n") else cat("- O modelo tem dificuldade em detectar casos positivos\n")
  if(cm$byClass['Specificity'] >= 0.8) cat("- O modelo identifica bem os casos negativos\n") else cat("- O modelo tem dificuldade em identificar casos negativos\n")
  cat("-------------------------------------------------------\n\n")
}

# =========================================================
# a) ISLR::Default - variável dependente: student
# =========================================================

# Transformar variável dependente em fator
data_default <- Default
data_default$student <- as.factor(data_default$student)

# Regressão logística
modelo_default <- glm(student ~ ., data=data_default, family=binomial)

# Previsão probabilística
pred_prob_default <- predict(modelo_default, type="response")
pred_default <- ifelse(pred_prob_default > 0.5, "Yes", "No")
pred_default <- factor(pred_default, levels=c("No","Yes"))

# Avaliar modelo
avaliar_modelo(pred_default, data_default$student)


# =========================================================
# b) ISLR2::Smarket - variável dependente: Direction
# =========================================================

data_smarket <- Smarket
data_smarket$Direction <- as.factor(data_smarket$Direction)

# Regressão logística
modelo_smarket <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                      data=data_smarket, family=binomial)

# Previsão
pred_prob_smarket <- predict(modelo_smarket, type="response")
pred_smarket <- ifelse(pred_prob_smarket > 0.5, "Up", "Down")
pred_smarket <- factor(pred_smarket, levels=c("Down","Up"))

# Avaliar modelo
avaliar_modelo(pred_smarket, data_smarket$Direction)


# =========================================================
# c) ISLR2::Weekly - variável dependente: Direction
# =========================================================

data_weekly <- Weekly
data_weekly$Direction <- as.factor(data_weekly$Direction)

# Regressão logística
modelo_weekly <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                     data=data_weekly, family=binomial)

# Previsão
pred_prob_weekly <- predict(modelo_weekly, type="response")
pred_weekly <- ifelse(pred_prob_weekly > 0.5, "Up", "Down")
pred_weekly <- factor(pred_weekly, levels=c("Down","Up"))

# Avaliar modelo
avaliar_modelo(pred_weekly, data_weekly$Direction)


# =========================================================
# d) ISLR2::Caravan - variável dependente: Purchase
# =========================================================

data_caravan <- Caravan
data_caravan$Purchase <- as.factor(data_caravan$Purchase)

# Regressão logística
modelo_caravan <- glm(Purchase ~ ., data=data_caravan, family=binomial)

# Previsão
pred_prob_caravan <- predict(modelo_caravan, type="response")
pred_caravan <- ifelse(pred_prob_caravan > 0.5, "Yes", "No")
pred_caravan <- factor(pred_caravan, levels=c("No","Yes"))

# Avaliar modelo
avaliar_modelo(pred_caravan, data_caravan$Purchase)
