##############################################################
# LIMPEZA INICIAL
##############################################################
rm(list = ls())

##############################################################
# BIBLIOTECAS
##############################################################
library(ISLR)
library(ISLR2)
library(rpart)
library(rpart.plot)
library(caret)

##############################################################
# 1. DATASET IRIS
##############################################################

# Dados
dados001 <- iris
names(dados001)

# Partição dos dados (70% treino / 30% teste)
set.seed(123)
n <- nrow(dados001)
particao <- sample(seq_len(n), size = 0.7 * n)
treino001 <- dados001[particao, ]
teste001 <- dados001[-particao, ]

# Árvore completa
arvore001 <- rpart(Species ~ .,
                   data = treino001,
                   method = "class",
                   control = rpart.control(cp = 0.000,
                                           minsplit = 2,
                                           minbucket = 1,
                                           maxdepth = 20,
                                           xval = 10))

# Exibir tabela de complexidade
printcp(arvore001)
plotcp(arvore001)

# Selecionar cp ótimo
cp_otimo <- arvore001$cptable[which.min(arvore001$cptable[, "xerror"]), "CP"]

# Podar a árvore
arvore001_podada <- prune(arvore001, cp = cp_otimo)

# Visualizar árvores
rpart.plot(arvore001, main = "Árvore Completa - IRIS")
rpart.plot(arvore001_podada, main = "Árvore Podada - IRIS (CP ótimo)")

# Modelo completo
pred_classes <- predict(arvore001, teste001, type = "class")
resumo_arvore001 <- confusionMatrix(pred_classes, teste001$Species)

# Modelo podado
pred_classes_p <- predict(arvore001_podada, teste001, type = "class")
resumo_arvore001_p <- confusionMatrix(pred_classes_p, teste001$Species)


##############################################################
# 2. DATASET COLLEGE
##############################################################

# Dados
dados002 <- ISLR::College
names(dados002)

# Partição (70% treino / 30% teste)
set.seed(123)
n <- nrow(dados002)
particao <- sample(seq_len(n), size = 0.7 * n)
treino002 <- dados002[particao, ]
teste002 <- dados002[-particao, ]

# Árvore completa
arvore002 <- rpart(Private ~ ., 
                   data = treino002,
                   method = "class",
                   control = rpart.control(cp = 0.0001,
                                           minsplit = 2,
                                           minbucket = 1,
                                           maxdepth = 20,
                                           xval = 10))

# Exibir tabela de complexidade
printcp(arvore002)
plotcp(arvore002)

# Selecionar cp ótimo
cp_otimo2 <- arvore002$cptable[which.min(arvore002$cptable[, "xerror"]), "CP"]

# Podar a árvore
arvore002_podada <- prune(arvore002, cp = cp_otimo2)

# Visualizar
rpart.plot(arvore002, main = "Árvore Completa - College")
rpart.plot(arvore002_podada, main = "Árvore Podada - College")

# Modelo completo
pred_college <- predict(arvore002, teste002, type = "class")
resumo_college <- confusionMatrix(pred_college, teste002$Private)

# Modelo podado
pred_college_p <- predict(arvore002_podada, teste002, type = "class")
resumo_college_p <- confusionMatrix(pred_college_p, teste002$Private)


##############################################################
# 3. DATASET OJ
##############################################################

# Dados
dados003 <- ISLR2::OJ
names(dados003)

# Partição (70% treino / 30% teste)
set.seed(123)
n <- nrow(dados003)
particao <- sample(seq_len(n), size = 0.7 * n)
treino003 <- dados003[particao, ]
teste003 <- dados003[-particao, ]

# Árvore completa
arvore003 <- rpart(Purchase ~ ., 
                   data = treino003,
                   method = "class",
                   control = rpart.control(cp = 0.0001,
                                           minsplit = 2,
                                           minbucket = 1,
                                           maxdepth = 20,
                                           xval = 10))

# Exibir tabela de complexidade
printcp(arvore003)
plotcp(arvore003)

# Selecionar cp ótimo
cp_otimo3 <- arvore003$cptable[which.min(arvore003$cptable[, "xerror"]), "CP"]

# Podar a árvore
arvore003_podada <- prune(arvore003, cp = cp_otimo3)

# Visualizar
rpart.plot(arvore003, main = "Árvore Completa - OJ")
rpart.plot(arvore003_podada, main = "Árvore Podada - OJ")

# Modelo completo
pred_oj <- predict(arvore003, teste003, type = "class")
resumo_oj <- confusionMatrix(pred_oj, teste003$Purchase)

# Modelo podado
pred_oj_p <- predict(arvore003_podada, teste003, type = "class")
resumo_oj_p <- confusionMatrix(pred_oj_p, teste003$Purchase)


##############################################################
# COMPARAÇÃO DE DESEMPENHO
##############################################################

cat("\n========== COMPARAÇÃO DE DESEMPENHO ==========\n")

# ---- IRIS ----
cat("\n===== IRIS =====\n")
cat("Acurácia - Árvore Completa:", resumo_arvore001$overall[1], "\n")
cat("Acurácia - Árvore Podada:", resumo_arvore001_p$overall[1], "\n")
if (resumo_arvore001_p$overall[1] > resumo_arvore001$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE PODADA\n")
} else if (resumo_arvore001_p$overall[1] < resumo_arvore001$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE COMPLETA\n")
} else {
  cat("→ Mesma acurácia entre os modelos\n")
}

# ---- COLLEGE ----
cat("\n===== COLLEGE =====\n")
cat("Acurácia - Árvore Completa:", resumo_college$overall[1], "\n")
cat("Acurácia - Árvore Podada:", resumo_college_p$overall[1], "\n")
if (resumo_college_p$overall[1] > resumo_college$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE PODADA\n")
} else if (resumo_college_p$overall[1] < resumo_college$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE COMPLETA\n")
} else {
  cat("→ Mesma acurácia entre os modelos\n")
}

# ---- OJ ----
cat("\n===== OJ =====\n")
cat("Acurácia - Árvore Completa:", resumo_oj$overall[1], "\n")
cat("Acurácia - Árvore Podada:", resumo_oj_p$overall[1], "\n")
if (resumo_oj_p$overall[1] > resumo_oj$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE PODADA\n")
} else if (resumo_oj_p$overall[1] < resumo_oj$overall[1]) {
  cat("→ Melhor modelo: ÁRVORE COMPLETA\n")
} else {
  cat("→ Mesma acurácia entre os modelos\n")
}

##############################################################
# FIM DO SCRIPT - ARIEL LADISLAU REISES
##############################################################

