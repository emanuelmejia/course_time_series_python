library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("~/Series de tiempo/ST/ST/L0-Review/Review_Estadística/Scripts") #utilice el ctrl+shift+h

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)
var(uschange$Consumption) # 0.4068692

# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
cov(uschange[,2:6]) 

#

# Elabora una Matriz de correlación de variables numéricas
cor(uschange[,2:6])
###################################Comentarios###################################
## podemos ver que las variables 'Savings' e 'Income'
## están correlacionadas positivamente,
## ,también las variables 'consumo' 
## y 'producción' aunque no tan fuerte.
## las variables desempleo y producción 
## tienen una correlación negativa fuerte.
## y las variables consumo y desempleo tienen una correlación 
##negativa no muy fuerte pero significativa.
###################################Comentarios###################################

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)
cor(uschange[,2:6]) %>% corrplot(method = "square")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <-lm(uschange$Consumption~uschange$Unemployment)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 
###################################Comentarios###################################

## Tratamos de predecir el consumo a traves del desempleo
## cuando el desempleo es 0, el consumo predicho es aproximadamente mi estimador puntual 0.743
## cuando disminuye el desempleo aumenta el consumo segun la Beta1
## por cada aumento de una unidad en desempleo, el consumo disminuye en promedio en 0.914 
## el desempleo es un predictor estadísticamente significativo del consumo ya que el p.valor es muy muy pequeño
## obtenemos una r2 muy similar a la r2 ajustada porque solo hay una variable, entonces no penaliza más la r ajustada
## podriamos decir que  desempleo explica aproximadamente el 27.7% de lo que varia el consumo.

###################################Comentarios###################################

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

plot(x = uschange$Consumption,
     y = uschange$Unemployment,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "consumo vs desempleo",     # Título del gráfico
     xlab = "consumo", # Nombre del eje x
     ylab = "Desempleo")                    # Nombre del eje y
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)
# Podemos extraer los coeficientes
coefs <- coef(reg)
coefs
# Pintemos ahora una línea con base en estos coeficientes
abline(a = coefs[1], b = coefs[2], col = "grey", lwd = 4)


# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_consumo <- reg$fitted.values
uschange$res_consumo <- reg$residuals

# Regresión de residuales
reg_res_consumo <- lm(uschange$res_consumo ~uschange$pred_consumo) # "~" Regresión de los residuales con base en predicciones
reg_res_consumo
summary(reg_res_consumo)

graphics.off()
# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

uschange %>% ggplot(aes(x = uschange$pred_consumo, y = uschange$res_consumo)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consumo)[1], slope = coef(reg_res_consumo)[2], color = "#0099F8", size = 1.5)+
  geom_text(
    label= uschange$Date,
    nudge_x = 0, nudge_y = 15,
    check_overlap = T
  ) +
  geom_label(
    data = uschange %>% filter(pred_consumo < 1), # Filtramos datos
    aes(label = Date,
        x = pred_consumo,
        y = res_consumo),
    nudge_x = 0, nudge_y = 16,
    label.size = 0.3,
    label.padding = unit(0.15, "lines"),
    fill="lightblue") +
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción consumo",
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

reg_mult <- lm(uschange$Consumption~uschange$Unemployment + uschange$Income) 
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)
###################################Comentarios###################################

##podemos ver en la r2 ajustada que es la que penaliza por mas variables
##que cuando agregamos los ingresos podemos explicar más variabilidad del consumo que con el MRLS
##ambas variables son significativas y en este caso para los ingresos se puede ver 
##una relación positiva con el consumo

###################################Comentarios###################################

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

uschange$pred_consumo_mult <- reg_mult$fitted.values
uschange$res_consumo_mult <- reg_mult$residuals
reg_res_consumo_mult <- lm(uschange$res_consumo_mult ~uschange$pred_consumo_mult) # "~" Regresión de los residuales con base en predicciones
reg_res_consumo_mult
summary(reg_res_consumo_mult)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_consumo_mult, y = uschange$res_consumo_mult)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consumo_mult)[1], slope = coef(reg_res_consumo_mult)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "consumo ~ desempleo + ingresos",
       x = "Predicción consumo",
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

###################################Comentarios###################################
# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?

## Segun los resultados, yo elegiría el modelo de regresión multiple ya que puede explicar más variabilidad del target
## y vemos que no se esta sobreajustando ya que los criterios de información son menores que en el MRLS
## tiene menor error estandar y el residual es menor por lo que podemos decir que el error es mas pequeño.

###################################Comentarios###################################