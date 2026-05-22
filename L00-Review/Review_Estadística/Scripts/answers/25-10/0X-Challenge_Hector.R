library(tidyverse)
library(dplyr)


# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/Hector Carvajal/Documents/FORECAST_AMAT/Review_Estadística/Review_Estadística")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("C:/Users/hgcarvaj/Downloads/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

colSums(is.na(uschange))


uschange %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Calcula la varianza individual de la variable Consumo (Consumption)
var(uschange$Consumption)
var(uschange$Income)
var(uschange$Production)
var(uschange$Savings)
var(uschange$Unemployment)


uschange %>%
  summarise(across(where(is.numeric), var))

# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
cov(uschange[,2:6])

uschange %>%
  select(where(is.numeric)) %>%
  cov()
# Elabora una Matriz de correlación de variables numéricas
cor(uschange[,2:6])

uschange %>%
  select(where(is.numeric)) %>%
  cor()

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
cor(uschange[,2:6]) %>% corrplot(method = "square",      
                                 addCoef.col = "black", # mostrar valores de correlación
                                 tl.cex = 0.8,          # tamaño texto etiquetas
                                 number.cex = 0.7 )

cor_mat <- cor(uschange[,2:6])

heatmap(cor_mat, scale = "none")


library(ggplot2)
library(reshape2)
# Convertir a formato largo para ggplot
cor_long <- melt(cor_mat)

ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mapa de Calor con Valores de Correlación", x = "", y = "")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <- lm(uschange$Consumption~uschange$Production)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

plot(x = uschange$Production,
y = uschange$Consumption,                    # Coordenadas
col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
pch = 18,                            # Tipo de punto que se va a utilizar
main = "Consumo vs Produccion",     # Título del gráfico
xlab = "producción industrial", # Nombre del eje x
ylab = "consumo")                    # Nombre del eje y

abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)
coefs <- coef(reg)
# Pintemos ahora una línea con base en estos coeficientes
abline(a = coefs[1], b = coefs[2], col = "orangered1", lwd = 4)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_cons <- reg$fitted.values
uschange$res_cons <- reg$residuals

reg_res_cons<- lm(uschange$res_cons~uschange$pred_cons) # "~" Regresión de los residuales con base en predicciones
reg_res_cons
summary(reg_res_cons)

# modelo original no muestra un patrón sistemático en los errores — 
# los errores parecen aleatorios, lo cual indica que el modelo está bien ajustado en ese sentido.

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

uschange %>%
  ggplot(aes(x = pred_cons, y = res_cons)) +
  geom_point(alpha = 0.6, color = "#001F82") +
  geom_abline(
    intercept = coef(reg_res_cons)[1],
    slope = coef(reg_res_cons)[2],
    color = "#0099F8",
    size = 1.5
  ) +
  labs(
    title = "Predicciones vs Residuales",
    subtitle = "Gráfico de dispersión para el modelo de Consumo",
    x = "Predicción Consumo",
    y = "Residuales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8", size = 17, face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696", size = 10, face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696")
  )


# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <-  lm(
  uschange$Consumption ~ uschange$Income + uschange$Production + uschange$Savings +uschange$Unemployment
)
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_mult <- predict(reg_mult)
uschange$res_mult  <- residuals(reg_mult)

reg_res_mult <- lm(res_mult ~ pred_mult, data = uschange)
summary(reg_res_mult)


# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

library(ggplot2)

ggplot(uschange, aes(x = pred_mult, y = res_mult)) +
  geom_point(alpha = 0.6, color = "#001F82") +
  geom_abline(
    intercept = coef(reg_res_mult)[1],
    slope = coef(reg_res_mult)[2],
    color = "#0099F8",
    size = 1.5
  ) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  labs(
    title = "Predicciones vs Residuales (Modelo Múltiple)",
    subtitle = "Evaluación de independencia de errores",
    x = "Predicciones del modelo múltiple",
    y = "Residuales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8", size = 17, face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696", size = 10, face = "bold"),
    axis.text = element_text(color = "#969696", size = 10)
  )


# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)

glance(reg)
glance(reg_mult)

# Tras el análisis propuesto

#Analisis:
#- R²:El modelo múltiple explica 76.8% de la variación en el consumo, frente a solo 28% 
# en el modelo simple.
#-Sigma (error estándar residual):El error medio de las predicciones bajó de 0.54 a 0.31 lo que idica qeu las predicciones del modelo múltiple 
# son más precisas.
#-F-statistic:ambos son significativos, pero el múltiple tiene una mayor potencia explicativa.
# se ratidica con el p value
#-AIC / BIC:Ambos criterios bajan drásticamente el mejor ajuste global de da  en el modelo múltiple (menor es mejor).
#¿cuál de los dos modelos elegirías?

# modelo multivariable

# ¿Por qué?

#por los puntos mencionados en el analisis