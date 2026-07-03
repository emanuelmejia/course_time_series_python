library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(feasts)       # Para analizar series de tiempo
library(tsibble)      # Librería para crear y manejar datos temporales
library(vars)         # Estimación de modelos VAR 
library(forecast)     # Análisis de pronósticos
library(tseries)      # Análisis de series de tiempo

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L10-Modelos_VAR/Scripts")

## Importar datos
googbit <- read_csv("../data/google_bitcoin.csv")
googbit <- rename(googbit,  Fecha = Date)

# Gráficos iniciales
googbit %>% ggplot() +
  geom_line(aes(x= Fecha, y= bitcoin)) +
  theme_minimal() +
  labs(title = "Precio Bitcoin")

googbit %>% ggplot() +
  geom_line(aes(x= Fecha, y= google)) +
  theme_minimal() +
  labs(title = "Búsquedas de palabras relacionadas con 'Bitcoin'",
       caption = "Reporte - Google Trends")

# Análisis ACF - PACF
googbit %>% as_tsibble(index=Fecha) %>% 
  gg_tsdisplay(bitcoin, plot_type="partial") +labs(subtitle = "Precio Bitcoin")

googbit %>% as_tsibble(index=Fecha) %>% 
  gg_tsdisplay(google, plot_type="partial") +labs(subtitle = "Búsqueda en google palabras relacionadas a 'Bitcoin'")

# Revisando estacionariedad de cada serie
adf.test(googbit$bitcoin, alternative = "stationary")
adf.test(googbit$google, alternative = "stationary")
# Revisando estacionariedad de las diferencias
adf.test(difference(googbit$bitcoin)[-1], alternative = "stationary")
adf.test(difference(googbit$google)[-1], alternative = "stationary")

# Crear columnas de las diferencias
googbit["dif_bitcoin"] <- difference(googbit$bitcoin) 
googbit["dif_google"] <- difference(googbit$google) 

## Borrar el NA inicial creado por la diferencia
googbit_dif <- googbit %>%
  filter(Fecha >= "2020-01-12") %>%
  dplyr::select(dif_bitcoin,dif_google)

# Criterios para estimar la cantidad de lags
# Akaike information criterion (AIC) - Logverosimilitud con penalización
# Schwarz criterion (SC) - BIC en multivariados - Logverosimilitud con mayor penalización
# Final Prediction Error (FPE) criterion - Basado en RSS y penalización por parámetros
# Hannan–Quinn information criterion (HQC) - Logverosimilitud con penalización mayor a AIC pero menor a BIC
VARselect(googbit_dif, lag.max = 4, type="none")

# Creando un modelo VAR de 3 lags por serie
var_diff = VAR(as.ts(googbit_dif), p = 3, type = "none")
summary(var_diff)

# Verificando prueba de hipótesis de causalidad de Granger
causality(var_diff,cause="dif_google")$Granger
causality(var_diff,cause="dif_bitcoin")$Granger

## Eigenvalores para corroborar estabilidad
roots(var_diff)

# Prueba de No autocorrelación Serial en los residuales
# H0: No autocorrelación en el modelo multivariado
var_diff_test<- serial.test(var_diff, lags.pt = 12)
var_diff_test

# Elaboración de pronósticos
steps = 15
forecast(var_diff, h=steps) %>%
  autoplot() +
  xlab("Semanas")

# ¿Y cómo podemos retornarlos manualmente?
# Seleccionemos únicamente las columnas de interés en el tibble original
originales <- googbit %>% 
  dplyr::select(Fecha, bitcoin, google)
# Agreguemos una columna indicando que son los valores históricos
originales["valores"] <- "Históricos"

# Observemos los últimos valores
tail(originales)
last_bit <- tail(originales$bitcoin, n=1)
last_goo <- tail(originales$google, n=1)

# Guardemos los forecasts de las diferencias de cada variable por separado
bit_dif_pred <- forecast(var_diff, h=steps)$forecast$dif_bitcoin$mean
goo_dif_pred <- forecast(var_diff, h=steps)$forecast$dif_google$mean

# Guardemos el último valor de cada serie
# Y los forecasts de las diferencias a continuación
bit_val <- c(last_bit,bit_dif_pred)
goo_val <- c(last_goo,goo_dif_pred)

# Elaboremos un tibble de los pronósticos
# Conteniendo las mismas columnas de los históricos
pronosticos <- tibble(
  Fecha = as.Date("2022-05-29") + 7 * 0:15,
  bitcoin = cumsum(bit_val),
  google = cumsum(goo_val),
  valores = "Pronósticos"
)

# Juntamos ambos tibbles por columnas
googbit_final <- bind_rows(originales, pronosticos)

# Ver gráficamente los resultados
googbit_final %>% ggplot() +
  geom_line(aes(x= Fecha, y= bitcoin, colour = valores)) +
  theme_minimal() +
  labs(title = "Precio Bitcoin",
       subtitle = "Pronósticos modelo VAR")

googbit_final %>% ggplot() +
  geom_line(aes(x= Fecha, y= google, colour = valores)) +
  theme_minimal() +
  labs(title = "Búsquedas de palabras relacionadas con 'Bitcoin'",
       subtitle = "Pronósticos modelo VAR",
       caption = "Reporte - Google Trends")
