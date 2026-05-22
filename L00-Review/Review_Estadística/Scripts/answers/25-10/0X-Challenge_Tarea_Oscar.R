library(tidyverse)

# Get working directory para saber en dónde estamos
getwd("C:/Users/Downloads/Review_Estadística/Review_Estadística/Scripts")

# Set working directory (Pon el tuyo)
setwd("C:/Users/Downloads/Review_Estadística/Review_Estadística/Scripts")

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
corrplot(cor(uschange[,2:6]),method = "square")

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

coefs <- coef(reg)

plot(x = uschange$Production,
     y = uschange$Consumption,
     col = c("orangered1"),            
     pch = 18,                            
     main = "Consumo VS Predicción",     
     xlab = "Producción", 
     ylab = "Consumo")   
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)
abline(a = coef(reg)[1], b = coef(reg)[2], col = "orangered1", lwd = 2)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_consumption <- reg$fitted.values
uschange$res_consumption <- reg$residuals
reg_res_consumptio <- lm(uschange$res_consumption~uschange$pred_consumption)
               
summary(reg_res_consumptio)  
# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

library(ggplot2)
?ggplot
ggplot(data = uschange,aes(x = uschange$pred_consumption, y = uschange$res_consumption)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consumptio)[1], slope = coef(reg_res_consumptio)[2], color = "#0099F8", size = 1.5)+
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

reg_mult <- lm(uschange$Consumption ~ uschange$Production + uschange$Income)
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_consumption_mult <- reg_mult$fitted.values
uschange$res_consumption_mult <- reg_mult$residuals

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

ggplot(data = uschange,aes(x = uschange$pred_consumption_mult, y = uschange$res_consumption_mult)) + geom_point(alpha = 0.6, color = "#001F82") + 
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

# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)
glance(reg)
glance(reg_mult)

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?

#Eligiría el modelo de regreseión multiple para explicar la variable consumo
#Al comparar los criterios de información, los del modelo multiple son menores al del modelo simple
#Además la r^2 y r^2 ajustada son mayores al compararlo con el modelo de regresión simple