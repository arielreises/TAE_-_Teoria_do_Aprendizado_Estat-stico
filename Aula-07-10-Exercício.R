# =======================================================
# ÁRVORE DE DECISÃO - Análise da variável Ethnicity (ISLR::Credit)
# =======================================================

# 1. Instalar pacotes necessários (execute apenas uma vez)
install.packages("ISLR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("dplyr")

# 2.  Carregar bibliotecas
library(ISLR)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# =======================================================
# 3. Carregar e explorar os dados
# =======================================================
dados <- ISLR::Credit

# Ver primeiras linhas e estrutura
head(dados)
str(dados)

# Ver níveis da variável alvo (Ethnicity)
table(dados$Ethnicity)

# =======================================================
# 4. Particionar os dados (Treino 70% / Teste 30%)
# =======================================================
set.seed(123)  # Reprodutibilidade

indice_treino <- createDataPartition(dados$Ethnicity, p = 0.7, list = FALSE)
treino <- dados[indice_treino, ]
teste  <- dados[-indice_treino, ]

# Verificar tamanhos
cat("Treino:", nrow(treino), " | Teste:", nrow(teste), "\n")

# =======================================================
# 5. Criar o modelo de árvore de decisão
# =======================================================
arvore_ethnicity <- rpart(
  formula = Ethnicity ~ .,
  data = treino,
  method = "class",              # classificação
  parms = list(split = "gini"),  # critério GINI
  control = rpart.control(
    cp = 0.001,      # complexidade mínima
    minsplit = 10,   # mínimo de observações para dividir
    maxdepth = 6     # profundidade máxima
  )
)

# Mostrar o resumo da árvore
print(arvore_ethnicity)

# =======================================================
# 6. Plotar a árvore
# =======================================================
rpart.plot(
  arvore_ethnicity,
  type = 3,
  extra = 104,
  under = TRUE,
  faclen = 0,
  fallen.leaves = TRUE,
  main = "Árvore de Decisão - Ethnicity (Dataset Credit)"
)

# =======================================================
# 7. Fazer previsões no conjunto de teste
# =======================================================
# Predição de classes
pred_classes <- predict(arvore_ethnicity, teste, type = "class")

# Predição de probabilidades (opcional)
pred_prob <- predict(arvore_ethnicity, teste, type = "prob")

# Ver primeiros resultados
head(pred_classes)
head(pred_prob)

# =======================================================
# 8. Avaliar a acurácia do modelo
# =======================================================
matriz_confusao <- confusionMatrix(pred_classes, as.factor(teste$Ethnicity))
print(matriz_confusao)

# Mostrar apenas a acurácia
cat("Acurácia do modelo:", round(matriz_confusao$overall["Accuracy"], 4), "\n")

# =======================================================
# 9. Importância das variáveis
# =======================================================
cat("\nImportância das variáveis:\n")
print(arvore_ethnicity$variable.importance)
