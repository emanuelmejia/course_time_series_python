set.seed(10)

# Crearemos dos serie estacionarias en tendencia
n = 100

# Y = 0.1 + 0.2t + w_t
y <- 0.1 + (0.2 * 1:n) + rnorm(n)
y <- ts(y)

# X = 0.3 - 0.1t + w_t
x <- 0.3 - (0.1 * 1:n) + rnorm(n)
x <- ts(x)

# Gráfica de ambas series
plot.ts(data.frame(y, x), col = "coral")

# Una forma de poner los ACF de ambas series
acf(data.frame(y, x))

# Modelo de regresión de Y en X
reg_directa <- lm(y ~ x)
summary(reg_directa)

# Modelo incluyendo la tendencia
modelo_tend <- lm(y ~ x + time(y))
summary(modelo_tend)

# Crearemos dos series estacionarias en diferencias
y2 <- ts(cumsum(rnorm(100)))
x2 <- ts(cumsum(rnorm(100)))
plot.ts(data.frame(y2, x2), col = "blue")

# Regresión de Y en X
reg_directa2 <-lm(y2~x2)
summary(reg_directa2)

# Regresión de las diferencias
modelo_dif <-lm(diff(y2)~diff(x2))
summary(modelo_dif)
