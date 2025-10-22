# Ariel Ladislau Reises

# =========================================================
# Exercício 4 – Doença Cardíaca (Heart Disease)
# =========================================================

# =========================================================
# 1. Configuração e Importação de Dados
# =========================================================

# Carregando a base de dados
dados_coracao <- read.csv("heart_disease_uci.csv")

# Visualizando as primeiras linhas
head(dados_coracao)

# Verificando estrutura das variáveis
str(dados_coracao)

# Resumo estatístico
summary(dados_coracao)


# =========================================================
# 2. Limpeza e Tratamento de Dados
# =========================================================

# Verificando valores ausentes
colSums(is.na(dados_coracao))

# Removendo linhas com NA (caso existam)
dados_coracao <- na.omit(dados_coracao)

# Conferindo novamente
colSums(is.na(dados_coracao))


# =========================================================
# 3. Renomear Variáveis para Português
# =========================================================

names(dados_coracao) <- c("Idade", "Sexo", "Dor_peito", "Pressao_repouso", 
                          "Colesterol", "Glicemia_jejum", "ECG_repouso", 
                          "Freq_cardiaca_max", "Angina_exercicio", "Oldpeak", 
                          "Inclinação_ST", "Vasos_coloridos", "Thalassemia", "Doenca")

# Conferindo nomes
head(dados_coracao)


# =========================================================
# 4. Análise Exploratória
# =========================================================

# Distribuição da variável dependente
table(dados_coracao$Doenca)
prop.table(table(dados_coracao$Doenca))  # proporção de pacientes com doença

# Histogramas das variáveis numéricas
hist(dados_coracao$Idade, main="Distribuição de Idade", xlab="Idade", col="lightblue")
hist(dados_coracao$Pressao_repouso, main="Distribuição da Pressão Arterial", xlab="Pressão em repouso", col="lightgreen")
hist(dados_coracao$Colesterol, main="Distribuição do Colesterol", xlab="Colesterol", col="lightpink")

# Boxplot da frequência cardíaca máxima
boxplot(dados_coracao$Freq_cardiaca_max ~ dados_coracao$Doenca,
        main="Freq. Cardíaca Máx. x Doença Cardíaca",
        xlab="Doença Cardíaca (0=Ausente,1=Presente)",
        ylab="Frequência Cardíaca Máxima", col=c("lightblue","salmon"))

# Correlação entre variáveis numéricas
cor(dados_coracao[,c("Idade","Pressao_repouso","Colesterol","Freq_cardiaca_max","Oldpeak")])


# =========================================================
# 5. Modelagem: Regressão Logística
# =========================================================

# Variável dependente: Doenca (0/1)
# Modelo logístico usando todas as variáveis
modelo_logistico <- glm(Doenca ~ Idade + Sexo + Dor_peito + Pressao_repouso + 
                          Colesterol + Glicemia_jejum + ECG_repouso + Freq_cardiaca_max + 
                          Angina_exercicio + Oldpeak + Inclinação_ST + Vasos_coloridos + Thalassemia,
                        data=dados_coracao, family=binomial)

# Resumo do modelo
summary(modelo_logistico)

# Explicação:
# - Coeficientes positivos: aumentam a probabilidade de doença
# - Coeficientes negativos: diminuem a probabilidade
# - p-value: significância estatística
# - Deviance e AIC: qualidade do ajuste


# =========================================================
# 6. Avaliação do Modelo
# =========================================================

# Predição das probabilidades
probabilidades <- predict(modelo_logistico, type="response")

# Definir ponto de corte (0.5) para classificação
predicoes <- ifelse(probabilidades > 0.5, 1, 0)

# Matriz de confusão
table(Predito=predicoes, Real=dados_coracao$Doenca)

# Taxa de acurácia
mean(predicoes == dados_coracao$Doenca)

# Curva ROC e AUC
library(pROC)
roc_obj <- roc(dados_coracao$Doenca, probabilidades)
plot(roc_obj, main="Curva ROC")
auc(roc_obj)


# =========================================================
# 7. Previsão para Novos Pacientes
# =========================================================

novos_pacientes <- data.frame(
  Idade = c(55, 40),
  Sexo = c(1, 0),
  Dor_peito = c(2, 1),
  Pressao_repouso = c(140, 120),
  Colesterol = c(250, 200),
  Glicemia_jejum = c(0, 1),
  ECG_repouso = c(0, 1),
  Freq_cardiaca_max = c(160, 150),
  Angina_exercicio = c(0, 1),
  Oldpeak = c(1.2, 0.5),
  Inclinação_ST = c(2, 1),
  Vasos_coloridos = c(0, 1),
  Thalassemia = c(2, 3)
)

# Previsão das probabilidades de doença cardíaca
predict(modelo_logistico, newdata=novos_pacientes, type="response")
