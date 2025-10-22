# Ariel Ladislau Reises

# =========================================================
# Exercícios de Regressão Linear Múltipla
# =========================================================

# =========================================================
# 0. Pacotes necessários
# =========================================================
# install.packages("ISLR")
# install.packages("ISLR2")
library(ISLR)
library(ISLR2)

# =========================================================
# Função auxiliar: criar modelo e mostrar resumo
# =========================================================
criar_modelo_linear <- function(base, formula, nome_var_dep) {
  cat("=======================================================\n")
  cat("Modelo de Regressão Linear Múltipla - Variável dependente:", nome_var_dep, "\n")
  cat("=======================================================\n")
  
  modelo <- lm(formula, data=base)
  print(summary(modelo))
  
  # Visualização dos resíduos
  par(mfrow=c(1,2))
  plot(modelo$fitted.values, modelo$residuals,
       xlab="Valores Ajustados", ylab="Resíduos",
       main=paste("Resíduos vs Ajustados -", nome_var_dep))
  abline(h=0, col="red")
  
  hist(modelo$residuals,
       main=paste("Histograma dos Resíduos -", nome_var_dep),
       col="lightblue", border="black")
  
  par(mfrow=c(1,1))
  return(modelo)
}

# =========================================================
# c) ISLR::Credit - variável dependente: balance
# =========================================================
data_credit <- Credit

modelo_credit <- criar_modelo_linear(data_credit,
                                     balance ~ ., 
                                     "balance (saldo do cartão de crédito)")

# =========================================================
# d) ISLR::Hitters - variável dependente: salary
# =========================================================
data_hitters <- Hitters

# Remover NAs (salário ausente)
data_hitters <- na.omit(data_hitters)

modelo_hitters <- criar_modelo_linear(data_hitters,
                                      salary ~ ., 
                                      "salary (salário dos jogadores de baseball)")

# =========================================================
# e) ISLR2::Wage - variável dependente: wage
# =========================================================
data_wage <- Wage

modelo_wage <- criar_modelo_linear(data_wage,
                                   wage ~ ., 
                                   "wage (salário)")

# =========================================================
# f) ISLR2::College - variável dependente: outstate
# =========================================================
data_college <- College

modelo_college <- criar_modelo_linear(data_college,
                                      outstate ~ ., 
                                      "outstate (mensalidade de alunos de fora do estado)")

# =========================================================
# g) ISLR2::OJ - variável dependente: PriceMM
# =========================================================
data_oj <- OJ

modelo_oj <- criar_modelo_linear(data_oj,
                                 PriceMM ~ ., 
                                 "PriceMM")
