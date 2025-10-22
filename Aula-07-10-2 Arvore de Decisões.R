install.packages("rpart.plot")


# Carregando as bibliotecas
library(rpart)
library(rpart.plot)

# ----------------------------
# 1. Criando o dataframe base
# ----------------------------
dados <- data.frame(
  Estadocivil = as.factor(c("solteiro", "casado", "solteiro", "casado", "solteiro")),
  renda = c(1000, 2000, 3500, 4000, 5000),
  Compra = c(0, 1, 0, 1, 1)
)

# Convertendo variável resposta para fator (0/1 → Não/Sim)
dados$Compra <- factor(dados$Compra, levels = c(0, 1), labels = c("Nao", "Sim"))

# Visualizar os dados
print(dados)

# ----------------------------
# 2. Criando a árvore de decisão
# ----------------------------
arvore001 <- rpart(
  formula = Compra ~ Estadocivil + renda,
  data = dados,
  method = "class",
  parms = list(split = "gini"),  # ou "information" para entropia
  control = rpart.control(
    cp = 0.0,       # permite crescimento total da árvore
    minsplit = 2,
    minbucket = 1,
    maxdepth = 4,
    xval = 10
  )
)

# Visualizar a árvore no console
print(arvore001)

# ----------------------------
# 3. Plotar a árvore
# ----------------------------
rpart.plot(
  arvore001,
  type = 3,               # mostra divisões e previsões
  extra = 104,            # mostra % e probabilidade
  under = TRUE,
  faclen = 0,
  fallen.leaves = TRUE,
  main = "Árvore (não podada)"
)

# ----------------------------
# 4. Criando novo dataframe para predição
# ----------------------------
novos <- data.frame(
  Estadocivil = factor(c("solteiro", "solteiro", "casado"),
                       levels = levels(dados$Estadocivil)),
  renda = c(3000, 6000, 2500)
)

print(novos)

# ----------------------------
# 5. Fazendo predições
# ----------------------------
# Probabilidade de "Sim"
pred_novos_probabilidade <- predict(arvore001, novos, type = "prob")[, "Sim"]

# Classe prevista ("Sim" ou "Nao")
pred_novos_classes <- predict(arvore001, novos, type = "class")

# Adicionando predições ao dataframe
novos$Prob_Sim <- pred_novos_probabilidade
novos$Compra_Prevista <- pred_novos_classes

# Criando novo dataframe
novos$Compra<-pred_novos_classes

# Exibindo resultados
print(novos)

