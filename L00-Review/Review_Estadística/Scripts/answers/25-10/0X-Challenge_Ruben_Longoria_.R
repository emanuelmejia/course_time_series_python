library(tidyverse)
rm(list=ls())

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/ruben/Documents/AMAT/Semana2")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read_csv("data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)

# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)
sd(uschange$Consumption)


# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
uschange_2 <- uschange %>% 
  na.omit()%>%
  select(!Date)

var(uschange_2)

# Elabora una Matriz de correlación de variables numéricas
cor(uschange_2)


# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
library(psych)

corPlot(uschange_2)




# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <- lm(Consumption~Production,data=uschange)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

uschange %>%
ggplot(aes(x=Production,y=Consumption))+
  geom_point(color = "#1f78b4", alpha = 0.7, size = 2.5)+
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color = "#08306b", size = 1.2) +
  geom_abline(intercept = mean(uschange$Consumption), slope = 0, color = "darkgreen", linetype = "dashed", size = 0.7) +
  labs(
    title = "Relación Consumo y Producción",
    subtitle = "Ajuste de regresión lineal",
    y = "Consumo",
    x = "Producción"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
  )
  

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred<-predict(reg)
uschange$residuals<-residuals(reg)


# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

reg2 <- lm(residuals~pred,data=uschange)

uschange %>%
  ggplot(aes(x=pred,y=residuals))+
  geom_point(color = "#1f78b4", alpha = 0.7, size = 2.5)+
  geom_abline(intercept = reg2$coefficients[1], slope = reg2$coefficients[2], color = "#08306b", size = 1.2) +
  labs(
    title = "residuales VS predicciones",
    subtitle = "Ajuste de regresión lineal",
    y = "Residuales",
    x = "Producciones"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#deebf7", color = NA),
    panel.grid.major = element_line(color = "#c6dbef"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = "#08306b"),
    plot.subtitle = element_text(color = "#2171b5")
  )





# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- lm(Consumption~Production+Unemployment,data=uschange)

# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_mult<-predict(reg_mult)
uschange$residuals_mult<-residuals(reg_mult)

reg_res_mult <- lm(residuals_mult~pred_mult,data=uschange)
reg_res_mult$coefficients

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>%
  ggplot(aes(x=pred_mult,y=residuals_mult))+
  geom_point(color = "#1f78b4", alpha = 0.7, size = 2.5)+
  geom_abline(intercept = reg_res_mult$coefficients[1], slope = reg_res_mult$coefficients[2], color = "#08306b", size = 1.2) +
  labs(
    title = "Residuales VS predicciones",
    subtitle = "Ajuste de regresión lineal",
    y = "Residuales",
    x = "Producciones"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#0B2239"),
    plot.subtitle = element_text(size = 13, color = "#2F4F4F"),
    axis.title = element_text(face = "bold", color = "#0B2239"),
    panel.grid.major = element_line(color = "#D9E1E8"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )




# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)
glance(reg)
glance(reg_mult)



# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?# ¿Por qué?

# El modelo seleccionado por ambos creiterios AIC/BIC sería el multiple
# a pesar de que son significativos solo al 1-0.001 nuestra una mejor R2, sin embargo
# habría que analizar un mejor modelo ya que no este nivel de R2 no es tan alto




