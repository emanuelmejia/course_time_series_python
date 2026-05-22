library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)

setwd("C:/Users/u1660033/OneDrive - Clorox Services Company/Desktop/LM/Series de tiempo/ST/L0-Review/Review_Estadística/Scripts")

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

reg <- lm(uschange$Consumption~uschange$Production)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

uschange %>% ggplot(aes(x = uschange$Production, y = uschange$Consumption)) + geom_point(alpha = 0.6, color = "blue") + 
  geom_abline(intercept =  mean(uschange$Consumption), slope = 0, color = "#0099F8", size = 1.5)+
  geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2], color = "orange", size = 1.5)+
  labs(title = "Consumo vs Produccion",
       subtitle = "Gráfico de dispersión",
       x = "Produccion",
       y = "Consumo") +
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

uschange$pred_simple <- reg$fitted.values
uschange$res_simple <- reg$residuals
regRES <- lm(uschange$res_simple ~uschange$pred_simple)
# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

uschange %>% ggplot(aes(x = uschange$pred_simple, y = uschange$res_simple)) + geom_point(alpha = 0.6, color = "gray") + 
    geom_abline(intercept = coef(regRES)[1], slope = coef(regRES)[2], color = "green", size = 1.5)+
  labs(title = "Residuales vs Produccion",
       subtitle = "Gráfico de dispersión",
       x = "Produccion",
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

reg_mult <- lm(uschange$Consumption~uschange$Production+uschange$Unemployment)
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_multiple <- reg_mult$fitted.values
uschange$res_multiple <- reg_mult$residuals
regRESmul <- lm(uschange$res_multiple ~uschange$pred_multiple)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_multiple, y = uschange$res_multiple)) + geom_point(alpha = 0.6, color = "gray") + 
  geom_abline(intercept = coef(regRESmul)[1], slope = coef(regRESmul)[2], color = "green", size = 1.5)+
  labs(title = "Consumo vs Produccion",
       subtitle = "Gráfico de dispersión",
       x = "Produccion",
       y = "Consumo") +
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
# TU CÓDIGO AQUÍ

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
#eligo la de reg_mult
# ¿Por qué?
#r2 es mas grande en reg_mult y tambien AIC y BIC son datos menores lo que indica que el coeficiente es mejor
#Adicional en el grafico se muestra una mayor acumulacion de puntos en cero lo cual se traduce con una menor variacion de error
# hay que tener cuidado cuando AIC y BIC son diferentes (en BIC al ser un set de datos mas grande se vuelve  confiable y por ende es el mas utilizado)
#