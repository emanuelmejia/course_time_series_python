library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("D:/_Cursos/FORECASTING/ST/L0-Review/Review_Estadística/data")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")
view(uschange)
# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)
# TU CÓDIGO AQUÍ
var(uschange$Consumption)
# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
# TU CÓDIGO AQUÍ
cov(uschange[,2:6])
# Elabora una Matriz de correlación de variables numéricas
# TU CÓDIGO AQUÍ
cor(uschange[,2:6])
# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
# TU CÓDIGO AQUÍ
?corrplot
cor(uschange[,2:6])%>%corrplot(method="color")
# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <- lm(uschange$Production~uschange$Consumption)# TU CÓDIGO AQUÍ
reg
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

# TU CÓDIGO AQUÍ
plot(x=uschange$Production,
     y=uschange$Consumption,
     col=c("green"),
     main="Producción vs Consumo",
     xlab="Producción Anual",
     ylab="Consumo Anual")
coefs2 <- coef(reg)

abline(a=mean(uschange$Consumption), b=0, col="red",lwd=3)
abline(a=coefs2[1],b=coefs2[2],col="blue",lwd=5)
# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# # TU CÓDIGO AQUÍ
uschange$pred_Prod <- reg$fitted.values
uschange$res_Prod <- reg$residuals
view(uschange)
reg_res_pred <- lm(uschange$res_Prod~uschange$pred_Prod)
reg_res_pred
summary(reg_res_pred)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

# TU CÓDIGO AQUÍ
plot(x=uschange$res_Prod,
     y=uschange$pred_Prod,
     color=c("red"),
     pch=12,
     main="Residuales vs Predicciones",
     xlab="Residuales",
     ylab="Predicciones")
coef3 <- coef(reg_res_pred)
coef3
abline(a = coef3[1], b = coef3[2], col = "orangered1", lwd = 4)
# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <-lm(uschange$Consumption~uschange$Unemployment+uschange$Savings) # TU CÓDIGO AQUÍ
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
# TU CÓDIGO AQUÍ
uschange$pred_con <- reg_mult$fitted.values
uschange$res_con <- reg_mult$residuals

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple
# TU CÓDIGO AQUÍ
uschange %>% ggplot(aes(x = uschange$pred_con, y = uschange$res_con)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_mult)[1], slope = coef(reg_mult)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Consumo ~ Desempleo + Ahorro ",
       x = "Predicción Consumo por Desempleo y Ahorro",
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

# TU CÓDIGO AQUÍ
glance(reg)
glance(reg_mult)
# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?
# 
# # EN MI CASO UTILICE AMBOS MODELOS PARA VER DOS CASOS COMPLETAMNETE DIFERENTES:
# EN EL PRIMER MODELO ELEGI 2 VARIABLES QUE TENÍAN CORRECLACIÓN POSITIVA QUE
# ERAN EL CONSUMO Y LA PRODUCCIÓN, DE LOS CUALES EL MODELO LINEAL AYUDO A TENER
# UNA APROXIMANCIÓN EXCELENTE, YA QUE LOS RESIDULES ESTÁBAN MUY CERCA DEL CERO
# Y AL MOMENTO DE EL VER EL ESTADÍSTICO, PODEMOS VER QUE EL P VALUE ERA PRACTICAMENTE 0
# 
# EN EL 2DO CASO UTILICE VARIABLES QUE TENÍAN UNA CORRELACIÓN NEGATIVA, COMO FUE
# EL CASO DEL CONSUMO CON EL AHORRO Y EL DESEMPLEO, LO QUE SIGNIFICA QUE SI UNO SUBE, EL
# OTRO BAJA, DE IGUAL MANERA EN ESTE MODELO EL COMPORTAMIENTO FUE MUY SIMILAR,
# YA QUE LOS RESIDUALES ESTABAN MUY CERCA DEL CERO Y EL P VALUE PRACTICAMENTE TAMBIÉN
# 
# CREO QUE AMBOS MODELOS SON MUY UTILES Y EN CUANTO A PREFERENCIAS ME QUEDÓ CON EL 
# MODELO MULTIVARIADO QUE NOS PERMITE EXPLORAR UN POCO MÁS CON LOS DATOS DE LAS BASES Y
# JUGAR CON LOS MODELOS.
# 
# COMO HINT: EN ESTA CLASE NO ESTUVE, ESPERO QUE LAS CONCLUSIONES SEAN CORRECTAS
# ASI COMO CUALQUIER COMENTARIO O COMPLEMENTO ES SIEMPRE BIENVENIDO
# 