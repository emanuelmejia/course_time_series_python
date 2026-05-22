library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L0-Review/Review_Estadística/Scripts")

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
cor(uschange[,2:6])

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
cor(uschange[,2:6]) %>% corrplot(method = "square")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"
reg <- lm(uschange$Consumption~uschange$Income) 
reg 
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión
coefs <- coef(reg)
uschange %>% ggplot(aes(x = uschange$Income, y = uschange$Consumption)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = mean(uschange$Consumption), slope = 0, color = "lightgrey", size = 1)+
  geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Ingresos VS Consumo",
       subtitle = "Análisis de Regresión Lineal Simple",
       x = "Ingresos (% de cambio trimestral)",
       y = "Consumo (% de cambio trimestral)") +
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

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
uschange$pred_consum <- reg$fitted.values
uschange$res_consum <- reg$residuals
reg_res_consum <- lm(uschange$res_consum~uschange$pred_consum) 
reg_res_consum
summary(reg_res_consum)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión
uschange %>% ggplot(aes(x = uschange$pred_consum, y = uschange$res_consum)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consum)[1], slope = coef(reg_res_consum)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción Consumo",
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


# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)
reg_mult <- lm(uschange$Consumption~uschange$Income + uschange$Unemployment) # "~" Regresión de Consumption cardio con base en consumo Income
reg_mult
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
uschange$pred_consum_mult <- reg_mult$fitted.values
uschange$res_consum_mult <- reg_mult$residuals
reg_res_mult <- lm(uschange$res_consum_mult~uschange$pred_consum_mult) # "~" Regresión de los residuales con base en predicciones
reg_res_mult
summary(reg_res_mult)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple
uschange %>% ggplot(aes(x = pred_consum_mult, y = res_consum_mult)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_mult)[1], slope = coef(reg_res_mult)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Consumo ~ Ingreso + Desempleo",
       x = "Predicción Consumo",
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
