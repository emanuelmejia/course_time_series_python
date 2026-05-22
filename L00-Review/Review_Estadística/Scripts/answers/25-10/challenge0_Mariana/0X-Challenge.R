library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/G18406/Documents/Curso_TS/Review_Estadística/Scripts")

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
# X : Variable explicativa (production)
# Y guardarla dentro de una variable llamada "reg"
reg <- lm(uschange$Consumption~uschange$Production)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (production) (eje x)
plot(x = uschange$Production,
     y = uschange$Consumption,                    # Coordenadas
     col = c("#63B8FF"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Producción VS Consumo",     # Título del gráfico
     xlab = "Producción", # Nombre del eje x
     ylab = "Consumo")                    # Nombre del eje y
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión
#Modelo nulo
abline(a = mean(uschange$Consumption), b = 0, col = "#708090", lwd = 2)
#línea de regresión
coefs <- coef(reg)
abline(a = coefs[1], b = coefs[2], col = "#8B3A62", lwd = 2)


# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
uschange$pred_cons <- reg$fitted.values
uschange$res_cons <- reg$residuals

# Regresión de residuales
reg_res_cons <- lm(uschange$res_cons~uschange$pred_cons) # "~" Regresión de los residuales con base en predicciones
reg_res_cons
summary(reg_res_cons)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión
uschange %>% ggplot(aes(x = uschange$pred_cons, y = uschange$res_cons)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_cons)[1], slope = coef(reg_res_cons)[2], color = "#00868B", size = 1.2)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción consumo",
       y = "Residuales") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#303030", size = 13, face = "italic"),
    axis.title = element_text(color = "#303030",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#303030", size = 10),
    axis.line = element_line(color = "#303030")
  )

# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (producción / desempleo)

reg_mult <-lm(uschange$Consumption~uschange$Production + uschange$Unemployment) 
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
uschange$pred_cons_mult <-reg_mult$fitted.values
uschange$res_cons_mult <- reg_mult$residuals

# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
reg_res_mult <- lm(uschange$res_cons_mult~uschange$pred_cons_mult) # "~" Regresión de los residuales con base en predicciones
reg_res_mult
summary(reg_res_mult)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_cons_mult, y = uschange$res_cons_mult)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_mult)[1], slope = coef(reg_res_mult)[2], color = "#00868B", size = 1.2)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción consumo",
       y = "Residuales modelo múltiple") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 15,
                              face = "bold"),
    plot.subtitle = element_text(color = "#303030", size = 13, face = "italic"),
    axis.title = element_text(color = "#303030",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#303030", size = 10),
    axis.line = element_line(color = "#303030")
  )

# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)
glance(reg)
glance(reg_mult)

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?
#Elegiría el modelo mútiple porque tiene una mayor R² y R² ajustada, mientras
#que que el AIC y BIC son menores en este modelo, que es lo que estamos buscando.
