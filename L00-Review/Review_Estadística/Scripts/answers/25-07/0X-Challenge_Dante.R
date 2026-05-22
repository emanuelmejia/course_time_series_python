cat("\014")
rm(list = ls())
graphics.off()
library(tidyverse)
# Get working directory para saber en dónde estamos
getwd()
# Set working directory (Pon el tuyo)
setwd("C:/Users/Dante/OneDrive/Documentos/Preparacion/R/Regresion/Scripts")
# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")

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
cor(uschange[,2:6]) %>% corrplot(method = "square")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"
reg <- lm(uschange$Consumption~uschange$Income)# TU CÓDIGO AQUÍ
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 
coef <- coef(reg)

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Hagamos una gráfica de dispersión
plot(x = uschange$Income,
     y = uschange$Consumption,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Nivel de consumo segun ingresos",     # Título del gráfico
     xlab = "Ingresos", # Nombre del eje x
     ylab = "consumo")                    # Nombre del eje y
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión, guardando los coeficientes en una variable
# TU CÓDIGO AQUÍ
abline(a=mean(uschange$Consumption),b=0,col="blue",lwd=2)
abline(a=coef[1], b=coef[2], col="orangered1", lwd=2)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# TU CÓDIGO AQUÍ
uschange$pred <- reg$fitted.values
uschange$res <-  reg$residuals
reg_res <- lm(uschange$res~uschange$pred)
summary(reg_res)
coef_reg_res <- coef(reg_res)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión
# TU CÓDIGO AQUÍ
plot(x = uschange$pred,
     y = uschange$res,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Prediccion consumo vs Residuales",     # Título del gráfico
     xlab = "Prediccion de consumo", # Nombre del eje x
     ylab = "Residuales")                    # Nombre del eje y
abline(a=coef_reg_res[1], b=coef_reg_res[2], col = "darkorange", lwd=2)


######################################################################################################
# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- lm(uschange$Consumption~uschange$Production+uschange$Income)# TU CÓDIGO AQUÍ
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
# TU CÓDIGO AQUÍ
uschange$pred_mult <- reg_mult$fitted.values
uschange$res_mult <- reg_mult$residuals
reg_res_mult <- lm(uschange$res_mult~uschange$pred_mult)
coef_reg_res_mult <- coef(reg_res_mult)
# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple
# TU CÓDIGO AQUÍ
plot(x = uschange$pred_mult,
     y = uschange$res_mult,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Prediccion consumo vs Residuales (mult)",     # Título del gráfico
     xlab = "Prediccion de consumo", # Nombre del eje x
     ylab = "Residuales")                    # Nombre del eje y
abline(a=coef_reg_res_mult[1], b=coef_reg_res_mult[2], col = "darkorange", lwd=2)

# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
# TU CÓDIGO AQUÍ
library(broom)
glance(reg)
glance(reg_mult)
# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías? ; ¿Por qué?
##### RESPUESTA #####

### Regresion lineal simple ###
#Para el modelo de regresion simple utilize consumo como variable de respuesta e ingresos como
#variable predictora donde se obtuvo un R-ajus de 0.143 con un P value <0.001 por tanto 
#rechazamos hipotesis nula y decimos que existe una relacion lineal entre consumo e ingresos

### Regresion lineal multiple ###
#Para el modelo de regresion multiple agregue la variable de produccion ya que tiene una alta 
#correacion con el consumo con este modelo se obtuvo un R-ajus 0.336, observamos un claro 
#incremento respecto al modelo lineal el p valor es muy pequeño lo que indica que existe 
#relacion entre las variables predictoras y de respuesta

#### ELECCION ####

###Sin duda en este caso el modelo indicado seria el de regresion multiple dado que el 
### R-ajus es mayor y los criterios A|C , B|C disminuyen al agregar la variable, lo que indica
###que el modelo no esta sobreajustado 

