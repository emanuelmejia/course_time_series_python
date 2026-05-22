# Tidiverse incluye varios paquetes útiles para datos
# ggplot2 para visualización
# dplyr para manipulación de datos
# tidyr para organizar datos
# readr para importar datos 
# cpurrr para programación funcional 
# tibble para marcos de datos mejorados. 
library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Carga y Descarga Básica de Archivos -------------------------------------

# Set working directory
# Se guardarán en esta liga todos los archivos que se trabajen

#### OJO!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### PONER AQUÍ ABAJO LA UBICACIÓN EN SU COMPUTADORA
#### EN DONDE SE ENCUENTRA LA CARPETA DE ESTE SCRIPT
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L0-Review/Review_Estadística/Scripts")

# Puede seleccionarse manualmente usando el Shortcut CTRL+SHIFT+H
# Pero entonces se tendrá que hacer cada vez que se abra el archivo

vino <- read.csv("../data/vino.csv")

# Mostrar los primeros 10 datos
head(vino,10)
# ¿Qué tipo de objeto es?
class(vino)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(vino)

# Remover NAs
vino <- vino %>% na.omit() # Mucho cuidado con los NA
head(vino,10)
summary(vino)

# Varianza individual
var(vino$alcohol)
# Matriz de Varianzas/Covarianzas
cov(vino[,3:6])
# Matriz de correlaciones
cor(vino[,3:6])


library(corrplot) # librería de gráficos de correlaciones

# Graficamos los datos numéricos
cor(vino[,3:6]) %>% corrplot(method = "square")

# Hagamos una gráfica de dispersión
plot(x = vino$alcohol,
     y = vino$muertes,                    # Coordenadas
     col = c("orangered1"),               # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                            # Tipo de punto que se va a utilizar
     main = "Vino VS Muertes",     # Título del gráfico
     xlab = "Alcohol consumido en vino per cápita (L)", # Nombre del eje x
     ylab = "Muertes por cada 100,000 hab")                    # Nombre del eje y

# abline pinta sobre el gráfico actual una recta del tipo y = a + bx

# Pintemos el MODELO NULO, es decir la media de y
abline(a = mean(vino$muertes), b = 0, col = "blue", lwd = 2)

# Intentos dse encontrar una recta que se ajuste a los datos
abline(a = 700, b = 50, col = "gray50", lwd = 1)
abline(a = 1000, b = -50, col = "gray50", lwd = 1)


# Ajusta un modelo de regresión lineal (simple)

?lm # Ajusta un modelo lineal Y~X

# Y : Variable respuesta
# X : Variable explicativa

# Busco encontrar coeficientes b0 y b1 en Y = b0 + b1*X
# Que mejor se ajusten a mis datos

reg <- lm(vino$muertes~vino$alcohol) # "~" Lo que está en muertes en función de lo que está en alcohol
reg # Intercept es b0 y pendiente (trae el título original) es b1

# summary a una regresión para ver más datos 
# verificar asteriscos de coeficientes (basados en p-value)
summary(reg)

# Podemos extraer los coeficientes
coefs <- coef(reg)
# Pintemos ahora una línea con base en estos coeficientes
abline(a = coefs[1], b = coefs[2], col = "orangered1", lwd = 4)

View(vino)
# Guardemos las predicciones y residuales dentro del DF
vino$pred_muer <- reg$fitted.values
vino$res_muer <- reg$residuals

# Regresión de residuales
reg_res_muer <- lm(vino$res_muer~vino$pred_muer) # "~" Regresión de los residuales con base en predicciones
reg_res_muer
summary(reg_res_muer)

# Gráfico de residuales usando ggplot!
vino %>% ggplot(aes(x = vino$pred_muer, y = vino$res_muer)) + 
  geom_point(alpha = 0.6, color = "#001F82") +
  labs(title = "Predicciones VS Residuales",
       subtitle = "Gráfico de dispersión",
       x = "Predicción Muertes",
       y = "Residuales") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", 
                                 size = 13, 
                                 face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", 
                             size = 10),
    axis.line = element_line(color = "#969696")
  )

# Hagamos nuevamente el gráfico con la línea de regresión
vino %>% ggplot(aes(x = vino$pred_muer, y = vino$res_muer)) + 
  geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_muer)[1], 
              slope = coef(reg_res_muer)[2], 
              color = "#0099F8", size = 1.5)+
  geom_text(
    label= vino$pais,
    nudge_x = 0, nudge_y = 15,
    check_overlap = T
  ) +
  geom_label(
    data = vino %>% filter(pred_muer < 800), # Filtramos datos
    aes(label = pais,
        x = pred_muer,
        y = res_muer),
    nudge_x = 0, nudge_y = 16,
    label.size = 0.3,
    label.padding = unit(0.15, "lines"),
    fill="lightblue") +
  labs(title = "Predicciones VS Residuales",
     subtitle = "Gráfico de dispersión",
     x = "Predicción Muertes",
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

vino %>% ggplot(aes(x = vino$alcohol, y = vino$cardio)) + 
  geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_base)[1], 
              slope = coef(reg_base)[2], 
              color = "#0099F8", size = 1.5) +
  labs(title = "Alcohol VS Muertes Cardio",
       subtitle = "Gráfico de dispersión",
       x = "Alcohol consumido en vino per cápita (L)",
       y = "Muertes por cardiopatía (c/100K hab)") +
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

# Modelo base incluyendo solo una variable
reg_base <- lm(vino$cardio~vino$alcohol) # "~" Regresión de muertes cardio con base en consumo alcohol
reg_base
summary(reg_base)

# Agreguemos una variable adicional
reg_control <- lm(vino$cardio~vino$alcohol + vino$muertes) 
reg_control
summary(reg_control)

# Modelo saturado
reg_full <- lm(
  vino$cardio ~ vino$alcohol + vino$muertes + vino$hepatic
  )
reg_full
summary(reg_full)

# Comparación por medio de criterios de información (selección de variables)
library(broom)
glance(reg_base)
glance(reg_control)
glance(reg_full)

# ¿Y si ajustamos a quitar la variable sin significancia estadística?
reg_ajuste <- lm(
  vino$cardio ~ vino$muertes + vino$hepatic
)
reg_ajuste
summary(reg_ajuste)

# Comparemos estos últimos dos
glance(reg_full)
glance(reg_ajuste)

# Revisión de residuales
vino$pred_car <- reg_ajuste$fitted.values
vino$res_car <- reg_ajuste$residuals

reg_res_car <- lm(vino$res_car~vino$pred_car) # "~" Regresión de los residuales con base en predicciones
reg_res_car
summary(reg_res_car)

# Gráfico de residuales contra predicciones
vino %>% ggplot(aes(x = vino$pred_car, y = vino$res_car)) + geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_car)[1], slope = coef(reg_res_car)[2], color = "#0099F8", size = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Muertes Cardio ~ Muertes + Muertes Hepatic",
       x = "Predicción Muertes por Cardiopatía",
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

### OJO CON INTERPRETACIONES!!!!