library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("C:/Users/uriel/Documents/Curso TS")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read_csv("Review_Estadística/data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Calcula la varianza individual de la variable Consumo (Consumption)

uschange %>% pull(Consumption) %>% var()



# Elabora una Matriz de Varianzas/Covarianzas de variables numéricas
num_vars <- uschange %>% select( where(is.numeric))

cov(num_vars)



# Elabora una Matriz de correlación de variables numéricas
corr_mat <- cor(num_vars)

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
library(corrplot)

corrplot(corr_mat,method = "number")

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"
library(broom)
reg <- lm(Consumption ~ Unemployment , data = select(uschange, -Date))

# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

coefs  <- tidy(reg) %>% pull(estimate)
reg %>% broom::glance()

AIC(reg)
# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

uschange %>% ggplot(aes(y = Consumption, x = Unemployment )) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean(uschange$Consumption), color = "red", alpha = 0.5)+
  geom_abline(intercept = coefs[1], slope = coefs[2], color = "blue", alpha = 0.6)+
  labs(x = "Desempleo", y = "Consumo", title = "Gráfico de dispersión Consumo vs. Desempleo")
# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones


results <- uschange %>% mutate( 
  .preds = reg$fitted.values,
  .residuals = reg$residuals
  )

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

results %>% arrange(.preds) %>%  ggplot(aes(x = .preds, y = .residuals))+
  geom_point( alpha = 0.5) 
  # geom_line(alpha = 0.4)


# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- lm(Consumption ~ . , data = select(uschange, -Date))
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

tidy(reg_mult)
glance(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

results_mult <- uschange %>% mutate(
  .preds = reg_mult$fitted.values,
  .residuals = reg_mult$residuals
)


# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple
results_mult %>% arrange(.preds) %>%  ggplot(aes(x = .preds, y = .residuals))+
  geom_point( alpha = 0.5) 




# Realiza una comparación de ambos modelos 
# Utilizando criterios de información
library(broom)

results_all_models <- reg %>% 
  glance() %>% 
  mutate(model = "SLR") %>% 
  bind_rows(
    reg_mult %>% 
      glance() %>% 
      mutate(model = "MLR")
    ) 

results_all_models

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?


# De manera general el modelo multiple muestra mejores resultados, 
# tanto en criterios de informacion como BIC y AIC, como en R2 ajustada y varianza.
# Aún así ambos modelos son significativos. 
