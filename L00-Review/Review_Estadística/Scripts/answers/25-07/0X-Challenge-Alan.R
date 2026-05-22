library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
# setwd("TU CÓDIGO AQUÍ")
# CONTRL SHIFT H - Fijar directorio de trabajo 
# copiamos el código de la consola 
setwd("C:/Users/Windows 11/OneDrive/Documentos/AMAT/AMAT Forecasting Series de Tiempo R/ST/L0-Review/Review_Estadística/Scripts")


# Carga el archivo "US_change_full.csv" dentro de la variable uschange
# uschange <- read.csv("TU CÓDIGO AQUÍ")
uschange <- read.csv("../data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)

# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)
# TU CÓDIGO AQUÍ
uschange$Consumption %>% var()

# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
# TU CÓDIGO AQUÍ
uschange[,2:6] %>% cov()
cov(uschange[,2:6])

# Elabora una Matriz de correlación de variables numéricas
# TU CÓDIGO AQUÍ
uschange[,2:6] %>% cor()
cor(uschange[,2:6])

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
# TU CÓDIGO AQUÍ
corrplot(cor(uschange[,2:6]),method = "square")
cor(uschange[,2:6]) %>% corrplot(method = "square")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg0X <- lm(uschange$Consumption~uschange$Unemployment) # TU CÓDIGO AQUÍ
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg0X)

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

# TU CÓDIGO AQUÍ
plot(x = uschange$Unemployment,
     y = uschange$Consumption,                    # Coordenadas
     col = c("green"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Comsumo~Desempleo",     # Título del gráfico
     xlab = "Desempleo", # Nombre del eje x
     ylab = "Consumo")                    # Nombre del eje y
geom_abline(intercept = coef(reg0X)[1], slope = coef(reg0X)[2], color = "#0099F8", size = 1.5)
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)

coef(reg0X)

uschange %>% ggplot(aes(x = uschange$Unemployment, y = uschange$Consumption)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg0X)[1], slope = coef(reg0X)[2], color = "red", size = 1.5)+
labs(title = "Comsumo~Desempleo",
     subtitle = "Gráfico de dispersión",
     x = "Desempleo",
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
# TU CÓDIGO AQUÍ

coeficientes <- coef(reg0X)

uschange$pred_consum <- reg0X$fitted.values
uschange$resi_consum <- reg0X$residuals

View(uschange)

reg_resi_consum <- lm(uschange$resi_consum~uschange$pred_consum)
reg_resi_consum
reg_resi_consum %>% summary()

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

# TU CÓDIGO AQUÍ
uschange %>% ggplot(aes(x = uschange$pred_consum, y = uschange$resi_consum)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_resi_consum)[1], slope = coef(reg_resi_consum)[2], color = "red", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicciones Consumo",
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

reg_mult <- lm(uschange$Consumption~ uschange$Unemployment + uschange$Production) # TU CÓDIGO AQUÍ
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
# TU CÓDIGO AQUÍ
uschange$pred_mul_consum <- reg_mult$fitted.values
uschange$resi_mul_consum <- reg_mult$residuals

View(uschange)

reg_mul_resi_consum <- lm(uschange$resi_mul_consum~uschange$pred_mul_consum)
reg_mul_resi_consum
reg_mul_resi_consum %>% summary()


# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple
# TU CÓDIGO AQUÍ
uschange %>% ggplot(aes(x = uschange$pred_mul_consum, y = uschange$resi_mul_consum)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_mul_resi_consum)[1], slope = coef(reg_mul_resi_consum)[2], color = "red", size = 1.5)+
  labs(title = "Predicciones VS Residuales Regresión Múltiple",
       subtitle = "Gráfico de dispersión",
       x = "Predicciones Consumo",
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
view(glance(reg0X))
view(glance(reg_mult))

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?

# Elegiríamos el modelo de Regresión Múltiple que tiene como variable independiente al Consumo
# y tenemos de predictores al Desempleo y a la Producción ya que es mejor en todos los indicadores y criterios de información:
# La R cuadrada ajustada crece con la múltiple 
# El estadístico es menor en la regresión múltiple
# El p.value es menor en la regresión múltiple 
# Los crieterios de información AIC y BIC son menores con la regresión múltiple, por lo que es mejor ese modelo


