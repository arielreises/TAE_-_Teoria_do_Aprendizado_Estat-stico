# Gerando dados simples
set.seed(42)

x <- 1:100
y <- 3 * 4 + rnorm(100, sd = 10)

# Criando modelos: linear e polinomial grau 10 (overfitting)
modelo_simples <- lm(y ~ x)
modelo_complexo_02 <- lm(y ~ poly(x, 2))
modelo_complexo_10 <- lm(y ~ poly(x, 10))
modelo_complexo_27 <- lm(y ~ poly(x, 27))

# Plotando os modelos
plot(x, y, main = "Overfitting vs Ajuste Simples")

lines(x, predict(modelo_simples), col = "blue", lwd = 2)
lines(x, predict(modelo_complexo_02), col = "green", lwd = 2)
lines(x, predict(modelo_complexo_10), col = "red", lwd = 2)
lines(x, predict(modelo_complexo_27), col = "brown", lwd = 2)

legend("bottomright",
       legend = c("Linear", 
                  "Polinomial grau 2", 
                  "Polinomial grau 10", 
                  "Polinomial grau 27"),
       col = c("blue", "green", "red", "brown"),
       lwd = 1,
       cex = 0.5)

# Coeficientes dos modelos
modelo_simples$coefficients
modelo_complexo_02$coefficients
modelo_complexo_10$coefficients
modelo_complexo_27$coefficients

# Resumo do modelo simples
summary(modelo_simples)
summary(modelo_complexo_02)
summary(modelo_complexo_10)
summary(modelo_complexo_27)
