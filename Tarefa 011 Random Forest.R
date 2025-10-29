# Ariel Reises
# ---------------------------------------------------------------------
# EXPLICAÇÃO DA VARIÁVEL "default" DO DATASET ISLR2::Default
# ---------------------------------------------------------------------
# A variável "default" indica se um cliente não conseguiu pagar sua dívida
# (ou seja, entrou em "default" = inadimplência). Ela pode assumir dois valores:
#   - "Yes": o cliente deu calote / não pagou.
#   - "No": o cliente está adimplente.
# Nosso objetivo é prever "default" com base nas variáveis:
#   income (renda anual), balance (saldo médio devedor) e student (se é estudante).
# ---------------------------------------------------------------------

# Limpar ambiente
rm(list = ls())

# Bibliotecas
library(ISLR2)
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

# Semente para reprodutibilidade
set.seed(123)

# ---------------------------------------------------------------------
# Carregar dados
# ---------------------------------------------------------------------

dados_default <- ISLR2::Default

# Ver estrutura
str(dados_default)

# Transformar variável-alvo em fator
dados_default$default <- as.factor(dados_default$default)

# Remover possíveis NAs (boa prática)
dados_default <- na.omit(dados_default)

# ---------------------------------------------------------------------
# Divisão treino/teste (70% treino, 30% teste)
# ---------------------------------------------------------------------

set.seed(123)
particao <- sample(1:nrow(dados_default), size = 0.7 * nrow(dados_default), replace = FALSE)
treino <- dados_default[particao, ]
teste  <- dados_default[-particao, ]

# ---------------------------------------------------------------------
# Modelo base Random Forest
# ---------------------------------------------------------------------
# O Random Forest é escolhido porque:
#  - Trabalha bem com variáveis contínuas e categóricas.
#  - Reduz o overfitting em relação a árvores únicas.
#  - Permite medir a importância de cada variável.
# ---------------------------------------------------------------------

modelo_base <- randomForest(
  default ~ ., 
  data = treino,
  ntree = 500,  # número de árvores
  mtry = floor(sqrt(ncol(treino) - 1)),  # regra padrão (raiz quadrada do nº de variáveis preditoras)
  importance = TRUE
)

print(modelo_base)
varImpPlot(modelo_base)

# O gráfico de importância mostra quais variáveis mais contribuem para prever “default”.
# Normalmente, “balance” (saldo médio devedor) é a mais relevante, seguida por “income”.

# ---------------------------------------------------------------------
# Avaliação inicial
# ---------------------------------------------------------------------

pred_base <- predict(modelo_base, newdata = teste)
matriz_base <- confusionMatrix(pred_base, teste$default)
print(matriz_base)
matriz_base$overall["Accuracy"]

# ---------------------------------------------------------------------
# Teste de diferentes combinações de mtry e ntree
# ---------------------------------------------------------------------
# Vamos buscar a melhor combinação de:
#   - mtry: nº de variáveis testadas por divisão
#   - ntree: nº de árvores na floresta
# Nosso objetivo é:
#   - Minimizar o erro OOB (Out-of-Bag)
#   - Maximizar a acurácia em teste
#   - E, entre combinações semelhantes, escolher o modelo mais simples
#     (menor número de árvores e menor mtry)
# ---------------------------------------------------------------------

mtry_usados <- 1:(ncol(treino) - 1)
ntrees <- c(50, 100, 250, 500, 1000, 1500)

oob <- numeric(length(ntrees) * length(mtry_usados))
acuracia <- numeric(length(ntrees) * length(mtry_usados))
vetorntrees <- numeric(length(ntrees) * length(mtry_usados))
vetormtry <- numeric(length(ntrees) * length(mtry_usados))

n <- 1
for (i in seq_along(ntrees)) {
  for (m in seq_along(mtry_usados)) {
    vetorntrees[n] <- ntrees[i]
    vetormtry[n] <- mtry_usados[m]
    
    modelo_tmp <- randomForest(
      default ~ .,
      data = treino,
      ntree = ntrees[i],
      mtry = mtry_usados[m],
      importance = FALSE
    )
    
    # Erro OOB final
    oob[n] <- tail(modelo_tmp$err.rate[, "OOB"], 1)
    
    # Avaliação em teste
    pred_tmp <- predict(modelo_tmp, newdata = teste)
    matriz_tmp <- confusionMatrix(pred_tmp, teste$default)
    acuracia[n] <- matriz_tmp$overall["Accuracy"]
    
    n <- n + 1
  }
}

# ---------------------------------------------------------------------
# DataFrame com resultados
# ---------------------------------------------------------------------

info_rf <- data.frame(
  NTREES = vetorntrees,
  MTRY = vetormtry,
  OOB = oob,
  ACURACIA = acuracia
)

# Ordenar priorizando menor erro OOB e, em caso de empate, menor ntree e mtry
info_rf <- info_rf[order(info_rf$OOB, info_rf$NTREES, info_rf$MTRY), ]
head(info_rf, 10)

# ---------------------------------------------------------------------
# Visualização
# ---------------------------------------------------------------------

ggplot(info_rf, aes(x = MTRY, y = ACURACIA, color = as.factor(NTREES))) +
  geom_line() + geom_point() +
  labs(title = "Acurácia vs MTRY por Número de Árvores (Dataset Default)",
       x = "MTRY (nº de variáveis por split)",
       y = "Acurácia",
       color = "Nº de Árvores") +
  theme_minimal()

ggplot(info_rf, aes(x = NTREES, y = OOB, color = as.factor(MTRY))) +
  geom_line() + geom_point() +
  labs(title = "Erro OOB vs NTREE (Dataset Default)",
       x = "Número de Árvores (ntree)",
       y = "Erro OOB",
       color = "MTRY") +
  theme_minimal()

# ---------------------------------------------------------------------
# Escolher o melhor modelo (menor ntree e mtry com boa acurácia)
# ---------------------------------------------------------------------
# Aqui escolhemos o modelo com:
#   - Menor erro OOB (menor erro de validação interna do Random Forest)
#   - Menor número de árvores possível (menor custo computacional)
#   - mtry pequeno, o que ajuda a reduzir correlação entre árvores
# ---------------------------------------------------------------------

melhor <- info_rf[order(info_rf$OOB, info_rf$NTREES, info_rf$MTRY), ][1, ]
melhor

ntree_melhor <- melhor$NTREES
mtry_melhor <- melhor$MTRY

# ---------------------------------------------------------------------
# Modelo Final com melhor configuração
# ---------------------------------------------------------------------
# Este é o modelo considerado “ótimo” dentro dos critérios definidos:
# Ele equilibra:
#   - Boa acurácia em teste
#   - Baixo erro OOB
#   - Simplicidade (menos árvores e mtry menor)
# Isso indica que o modelo já estabilizou (aumentar ntree não melhora o desempenho)
# ---------------------------------------------------------------------

modelo_final <- randomForest(
  default ~ ., 
  data = treino,
  ntree = ntree_melhor,
  mtry = mtry_melhor,
  importance = TRUE
)

print(modelo_final)

# ---------------------------------------------------------------------
# Avaliação Final
# ---------------------------------------------------------------------

pred_final <- predict(modelo_final, newdata = teste)
matriz_final <- confusionMatrix(pred_final, teste$default)
print(matriz_final)
matriz_final$overall["Accuracy"]

# Interpretação:
#   - A acurácia indica o percentual de previsões corretas.
#   - O modelo tende a ser muito preciso ao prever “No” (não inadimplente),
#     pois a maioria dos clientes está nessa categoria.
#   - Porém, a variável “balance” é o principal fator para prever “Yes”.

# ---------------------------------------------------------------------
# Visualizar ponto de estabilização do erro OOB
# ---------------------------------------------------------------------
# O ponto de estabilização ocorre quando o erro OOB para de cair significativamente,
# indicando que aumentar o número de árvores não melhora mais o desempenho.
# ---------------------------------------------------------------------

plot(modelo_final, main = "Evolução do Erro OOB - Modelo Final")

# ---------------------------------------------------------------------
# Conclusão
# ---------------------------------------------------------------------
# A variável "default" é fortemente explicada por "balance" e "income".
# Clientes com saldos devedores altos e renda menor têm muito mais chance de entrar em default.
# O modelo Random Forest escolhido é o melhor porque:
#   - Minimiza o erro OOB (erro interno do ensemble)
#   - Maximiza a acurácia em dados de teste
#   - Usa o menor número possível de árvores, tornando-o eficiente
#   - Mostra estabilidade do erro (indicando que não há overfitting)
# ---------------------------------------------------------------------
