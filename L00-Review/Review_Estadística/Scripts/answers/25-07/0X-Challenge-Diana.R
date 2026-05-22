library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/crica/Documents/Files DKRT/ST/L0-Review/Review_Estadística/Scripts")

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

reg <-lm(uschange$Consumption~uschange$Income)
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 
# Cada que aumenta el ingreso o mientras el ingreso sea mayor, el consumo aumenta 0.2718, el ingreso explica de manera importante el consumo

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
plot(x = uschange$Income,
     y = uschange$Consumption,                    
     col = c("orangered1"),              
     pch = 18,                            
     main = "Consumo VS Ingresos",     
     xlab = "Ingresos",
     ylab = "consumo")                  

# Incorpora una línea que indique el modelo nulo (promedio de consumo)
abline(a = mean(uschange$Consumption), b = 0, col = "blue", lwd = 2)
# Y la línea de regresión

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

uschange$pred_consumo <- reg$fitted.values
uschange$res_consumo <- reg$residuals
reg_res_consumo <- lm(uschange$res_consumo~uschange$pred_consumo) 
reg_res_consumo
summary(reg_res_consumo)

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

uschange %>% ggplot(aes(x = uschange$pred_consumo, y = uschange$res_consumo)) + geom_point(alpha = 0.6, color = "#001F82") + 
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

uschange %>% ggplot(aes(x = uschange$pred_consumo, y = uschange$res_consumo)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_consumo)[1], slope = coef(reg_res_consumo)[2], color = "#0099F8", size = 1.5)+
  geom_text(
    label= uschange$Date,
    nudge_x = 0, nudge_y = 15,
    check_overlap = T
  ) +
  geom_label(
    data = uschange %>% filter(pred_consumo < 1),
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

reg_mult <- lm(uschange$Consumption~ uschange$Unemployment+uschange$Income) 
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple
uschange$pred_consumo2 <- reg_mult$fitted.values
uschange$res_consumo2 <- reg_mult$residuals

reg_mult_res_consumo2 <- lm(uschange$res_consumo2~uschange$pred_consumo) 
reg_mult_res_consumo2
summary(reg_mult_res_consumo2)

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

uschange %>% ggplot(aes(x = uschange$pred_consumo2, y = uschange$res_consumo2)) + geom_point(alpha = 0.6, color = "#001F82") + 
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

# Me quedaría con el módelo 2 dado que tanto el desempleo como el ingreso pueden explicar de manera importante el consumo y al meter el desempleo el modelo parece que mejora
# Al existir mayor desempleo el consumo bajaría aunque los ingresos aumenten dado que podría existir cierta preocupación por el tema del desempleo