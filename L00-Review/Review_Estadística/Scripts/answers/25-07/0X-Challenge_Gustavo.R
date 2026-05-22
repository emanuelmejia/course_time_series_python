library(tidyverse)

# Get working directory para saber en dónde estamos
#getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/Gustavo/Downloads/ST/L0-Review/Review_Estadística/Scripts")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
#La base se trata de datos económicos de manera trimestral desde el año 1970 hasta 2019 
#sobre los conceptos de: Consumo, Ingresos, Producción, Ahorro y Desempleo

# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)
#Se muestra un registro de 198 datos para cada columna, mientras que en el resumen de cada una,
#los datos más dispersos se encuentran en Ahorros (Savings) con un min = -56.472 y max = 41.608


# Calcula la varianza individual de la variable Consumo (Consumption)

var(uschange$Consumption)
#Nos da como resultado 0.4068692

# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas

cov(uschange[,2:6])

# Elabora una Matriz de correlación de variables numéricas

cor(uschange[,2:6])

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre

library(corrplot)

# Graficamos los datos numéricos
cor(uschange[,2:6]) %>% corrplot(method = "square")
#Se puede observar que existen correlaciones entre varias variables,
#por ejemplo, existe una correlación positiva entre las variables Consumo y Producción,
#existe una correlación negativa entre las variables Production y Unemployement,
#además, se observa que existe una nula correlación, por ejemplo; entre Producción y Ahorros

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

#Se eligió como variable explicativa a Producción
reg <- lm(uschange$Consumption~uschange$Production)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

#Se obtuvo a Bo=0.62998, B1=0.22186, por lo que el modelo de regresión lineal simple sería;
# Consumption = 0.62998+0.22186(Production)+Error
#Dada la Hipotesis Nula B1=0, y H1≠0 se observa que pvalue=1.11e-15 aprox pvalue=0
#por lo que, a un nivel de significancia alfa= 0.05, entonces se observa que;
#pvalue<alfa, es decir, me da evidencia para rechazar H0


# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

# Gráfica de dispersión
plot(x = uschange$Production,
     y = uschange$Consumption,                    # Coordenadas
     col = c("orangered1"),               # Color
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Producción VS Consumo",     # Título del gráfico
     xlab = "Producción", # Nombre del eje x
     ylab = "Consumo")                    # Nombre del eje y

# Pintemos el MODELO NULO, es decir la media de y
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)

#línea de regresión usando los coeficientes
# Podemos extraer los coeficientes
coefs <- coef(reg)
# Pintemos ahora una línea con base en estos coeficientes
abline(a = coefs[1], b = coefs[2], col = "green", lwd = 4)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

# Guardemos las predicciones y residuales dentro del DF
uschange$pred_consu <- reg$fitted.values
uschange$res_consu <- reg$residuals

# Regresión de residuales
reg_res_consu <- lm(uschange$res_consu~uschange$pred_consu) # "~" Regresión de los residuales con base en predicciones
reg_res_consu
summary(reg_res_consu)


# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

# Gráfico de residuales usando ggplot y línea de regresión
uschange %>% ggplot(aes(x = uschange$pred_consu, y =uschange$res_consu)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consu)[1], slope = coef(reg_res_consu)[2], color = "#0099F8", size = 1.5) +  
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


############     Regresión múltiple    #########################

# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)
#Las dos variables explicativas que se eligieron son: "Production" e "Income"

reg_mult <- lm(uschange$Consumption~uschange$Production + uschange$Income)
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

#Se obtuvo que B0=0.51068, B1=0.19250 y B2=0.18428, por lo que el modelo de regresión
#lineal múltiple es Consumption=0.51068 + 0.19250X1 + 0.18428X2 + error
#además se observa que tanto la variable "Production" e "Income" son significativas, 
#ya que ambos tienen un pvalue cercano a cero y con una R cuadrada ajustada del 0.336,
#mostrando así que las dos variables explicativas elegidas explican más del 30% de los 
#datos de "Consumption"


# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_consu2 <- reg_mult$fitted.values
uschange$res_consu2 <- reg_mult$residuals

# Regresión de residuales
reg_res_consu2 <- lm(uschange$res_consu2~uschange$pred_consu2) # "~" Regresión de los residuales con base en predicciones
reg_res_consu2
summary(reg_res_consu2)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

# Gráfico de residuales contra predicciones
uschange %>% ggplot(aes(x = uschange$pred_consu2, y = uschange$res_consu2)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consu2)[1], slope = coef(reg_res_consu2)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Consumption ~ Production + Income",
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
#Se elegiría el segundo modelo, el de regresión lineal múltiple:
#Consumption=0.51068 + 0.19250X1 + 0.18428X2 + error
#Ya que comparado con el modelo lineal simple;
#muestra un valor más elevado tanto en la R2 y R2 ajustada, mostrando así una mejor 
#explicación de "Consumption" a través de las variables "Production" e "Income"
#Además, en los criterios de información (AIC y BIC), muestran valores más bajos
#AIC=308 y BIC=321 comparados con los de la regresión lineal simple (AIC=324 y BIC=334)



