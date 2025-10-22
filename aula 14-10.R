# =======================================================
# ÁRVORE DE DECISÃO - Regressão (Exemplo com dados simulados)
# =======================================================

# 1. Instalar pacotes necessários (execute apenas uma vez)
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("dplyr")

# 2. Carregar bibliotecas
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# =======================================================
# 3. Criar o dataset
# =======================================================
dados02 <- data.frame(
  Estadocivil = as.factor(c("solteiro","casado","solteiro","casado","casado","solteiro")),
  renda = c(1000, 2000, 3500, 3000, 4000, 5000),
  Compra = c(100, 213, 250, 345, 345, 330)
)

# Ver estrutura dos dados
print(dados02)
str(dados02)

# =======================================================
# 4. Particionar dados (Treino 70% / Teste 30%)
# =======================================================
set.seed(123)
indice_treino <- createDataPartition(dados02$Compra, p = 0.7, list = FALSE)
treino <- dados02[indice_treino, ]
teste  <- dados02[-indice_treino, ]

cat("Treino:", nrow(treino), " | Teste:", nrow(teste), "\n")

# =======================================================
# 5. Criar o modelo de árvore de regressão
# =======================================================
arvore003 <- rpart(
  formula = Compra ~ Estadocivil + renda,
  data = treino,
  method = "anova",  # regressão
  parms = list(split = "gini"),
  control = rpart.control(
    cp = 0.0001,
    minsplit = 1,
    minbucket = 1,
    maxdepth = 3,
    xval = 10
  )
)

# Mostrar resumo da árvore
print(arvore003)

# =======================================================
# 6. Plotar a árvore
# =======================================================
rpart.plot(
  arvore003,
  type = 3,
  extra = 101,   # mostra o valor previsto
  under = TRUE,
  faclen = 0,
  fallen.leaves = TRUE,
  main = "Árvore de Decisão - Regressão (Compra ~ EstadoCivil + Renda)"
)

# =======================================================
# 7. Fazer previsões no conjunto de teste
# =======================================================
pred_teste <- predict(arvore003, newdata = teste)

# Comparar valores reais x previstos
comparacao <- data.frame(
  Real = teste$Compra,
  Previsto = round(pred_teste, 2)
)
print(comparacao)

# =======================================================
# 8. Avaliar desempenho (R² e RMSE)
# =======================================================
R2 <- cor(comparacao$Real, comparacao$Previsto)^2
RMSE <- sqrt(mean((comparacao$Real - comparacao$Previsto)^2))

cat("\nR² =", round(R2, 4), "\n")
cat("RMSE =", round(RMSE, 4), "\n")

# =======================================================
# 9. Fazer novas previsões
# =======================================================
novos <- data.frame(
  Estadocivil = as.factor(c("solteiro", "solteiro", "casado", "casado")),
  renda = c(3200, 4850, 2500, 4100)
)

# Prever valores para os novos dados
pred_novos <- predict(arvore003, newdata = novos)

# Adicionar a previsão ao dataframe
novos$Compra_Prevista <- round(pred_novos, 2)

print(novos)

# =======================================================
# Fim
# =======================================================
