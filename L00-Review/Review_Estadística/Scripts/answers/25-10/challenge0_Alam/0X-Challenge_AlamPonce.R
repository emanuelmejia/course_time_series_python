library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("F:/Downloads/Review_Estadística")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("./data/US_change_full.csv")

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

reg <- lm(uschange$Consumption~uschange$Savings)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

#Para este ejercicio se escogió la variable Savings como variable explicativa.
#En el resultado anterior podemos observar que es una variable muy significativa aún en el nivel más bajo de 0 (los tres asteríscos).
#En términos generales, el modelo dice que conforme la variable Saving aumenta, la variable Consumption disminuya.


# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

# Hagamos una gráfica de dispersión
plot(x = uschange$Savings,
     y = uschange$Consumption,                    
     col = c("orangered1"),             
     pch = 18,                          
     main = "Consumo VS Ahorros",     
     xlab = "Dinero consumido", 
     ylab = "Ahorros")                 

# abline pinta sobre el gráfico actual una recta del tipo y = a + bx

# Pintemos el MODELO NULO, es decir la media de y
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)

# Línea de regresión
coefs <- coef(reg)
abline(a = coefs[1], b = coefs[2], col = "orangered1", lwd = 4)


# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_cons <- reg$fitted.values
uschange$res_cons <- reg$residuals

reg_res_cons <- lm(uschange$res_cons~uschange$pred_cons) # "~" Regresión de los residuales con base en predicciones
reg_res_cons
summary(reg_res_cons)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

uschange %>% ggplot(aes(x = uschange$pred_cons, y = uschange$res_cons)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_cons)[1], slope = coef(reg_res_cons)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción Consumos",
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


#El intercepto y el coeficiente de las predicciones es prácticamente 0,
#junto con que el p-value es igual a 1 esto quiere decir que las predicciones y los residuales no están relacionados.

# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- lm(uschange$Consumption~uschange$Savings + uschange$Income) 
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

#Para la regresión multiple se tomaron las variables Savings (la misma que el modelo pasado) y la variable Income. 
#En el resultado anterior podemos observar ambas variables son muy significativas aún en el nivel más bajo de 0 (los tres asteríscos).
#En términos generales, el modelo dice que si fijamos la variable Income, conforme la variable Saving aumenta, 
#la variable Consumption disminuya; mientras que al fijar la variable Savings, conforme la variable Income aumenta, 
#la variable Consumption aumenta.

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_cons <- reg_mult$fitted.values
uschange$res_cons <- reg_mult$residuals

reg_res_cons <- lm(uschange$res_cons~uschange$pred_cons) # "~" Regresión de los residuales con base en predicciones
reg_res_cons
summary(reg_res_cons)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_cons, y = uschange$res_cons)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_cons)[1], slope = coef(reg_res_cons)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción Consumos",
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


#El intercepto y el coeficiente de las predicciones es prácticamente 0, 
#junto con que el p-value es igual a 1 esto quiere decir que las predicciones y los residuales no están relacionados.

# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)

glance(reg)
glance(reg_mult)

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?

#Podemos observar que ambos modelos obtienen resultados muy diferentes, explicaremos que sucede en la R^2,
# el AIC y BIC para determinar cual es el mejor modelo.

#En el modelo de una variable se obtiene una R^2 de 0.0659, mientras que en el de dos variables de 0.737,
# lo que quiere decir que el modelo multivariable explica casi 10 veces más que el de una variable.

#En el modelo de una variable se obtiene un AIC de 375 y un BIC de 385, mientras que en el de dos variables 
# se obtiene un AIC de 126 y un BIC de 139, lo que quiere decir que el modelo multivariable tiene un mejor balance
# respecto a la complejidad y menor penalización sobre esta.

#En conclusión, por lo mencionado anteriormente, el modelo multivariable es mejor y el que elegiría


