# Challenge 0X
# Jorge Abraham Campos Soto

library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("/Users/braamsy./Documents/ST/L0-Review/Review_Estadística/Scripts")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)
var(uschange$Consumption)

# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
cov(uschange[,2:6])

# Elabora una Matriz de correlación de variables numéricas
cor(uschange[,3:6])


# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
cor(uschange[,2:6]) %>% corrplot(method = "square")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <- lm(uschange$Consumption~uschange$Unemployment)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

plot(x = uschange$Unemployment,
     y = uschange$Consumption,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Vino VS Muertes",     # Título del gráfico
     xlab = "Desempleo", # Nombre del eje x
     ylab = "Consumo")                    # Nombre del eje y

abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)

coefs <- coef(reg)
abline(a = coefs[1], b = coefs[2], col = "orangered1", lwd = 4)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_consum <- reg$fitted.values
uschange$res_consum <- reg$residuals

# Regresión de residuales
reg_res_consum <- lm(uschange$res_consum~uschange$pred_consum) # "~" Regresión de los residuales con base en predicciones
reg_res_consum
summary(reg_res_consum)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

library(ggrepel)

uschange %>%
  ggplot(aes(x = pred_consum, y = res_consum)) +
  geom_point(alpha = 0.6, color = "#001F82") +
  geom_abline(intercept = coef(reg_res_consum)[1], slope = coef(reg_res_consum)[2], color = "#0099F8", size = 1.5) +
  geom_text_repel(aes(label = Date), size = 2.5, max.overlaps = 15) +
  labs(
    title = "Predicciones VS Residuales",
    subtitle = "Gráfico de dispersión",
    x = "Predicción Consumo",
    y = "Residuales"
  ) +
  theme_minimal()


# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- lm(uschange$Consumption~uschange$Income + uschange$Unemployment) 
reg_mult

# Analiza los resultados utilizando la función summary
summary(reg_mult)


# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_m_consum <- reg_mult$fitted.values
uschange$res_m_consum  <- reg_mult$residuals

regm_res_consum <- lm(uschange$res_m_consum~uschange$pred_m_consum) # "~" Regresión de los residuales con base en predicciones
regm_res_consum
summary(regm_res_consum)


# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_m_consum, y = uschange$res_m_consum)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(regm_res_consum)[1], slope = coef(regm_res_consum)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Consumo ~ Ingreso + Desempleo",
       x = "Predicción Consumo por Ingreso y Tasa de desempleo",
       y = "Residuales") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696")
  )


# Realiza una comparación de ambos modelos 
# Utilizando criterios de información

library(broom)

glance(reg)
glance(reg_mult)


# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?

# Tras haber realizado el análisis de ambos modelos, decido que el modelo
# de regresión múltiple es mejor y es el que elegiría.
# Cuando observamos la r-squared adjusted, la regresión múltiple muestra
# un parámetro mayor, (0.345 vs 0.274 (del modelo de una variable)).
# De igual manera, ambas pruebas muestran un p-value adecuado pero
# la prueba AIC y BIC son menores para el modelo multivariado
# lo cuál también es deseable.