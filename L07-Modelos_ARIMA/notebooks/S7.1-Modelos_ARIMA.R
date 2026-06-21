library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(tsibble)      # Librería para crear y manejar datos temporales
library(lubridate)    # Librería para trabajar con fechas/tiempos
library(feasts)       # Para analizar series de tiempo
library(fable)        # Provee modelos comunes de series de tiempo
library(plyr)

# Función que aplique tema a nuestros gráficos
ggtema <- function(graf = NULL) {
  graph <- graf +
    # theme_classic() +
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
  
  return(graph)
}

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L07-Modelos_ARIMA/Scripts")
set.seed(10)

## MODELO INTEGRADO I(1)

w <- rnorm(1000) # Normal estándar

# Crear caminata aleatoria
walk <- cumsum(w)

ts_i1 <- diff(walk)

par(mfrow = c(3,2))

plot(walk,type = 'l', col = 'cornflowerblue', main='Caminata Aleatoria', xlab = 't', ylab='x_t')
plot(ts_i1,type = 'l', col = 'cornflowerblue', main='Modelo Diferenciado', xlab = 't', ylab='x_t')

acf(walk, col = 'cornflowerblue', main='ACF Caminata Aleatoria')
acf(ts_i1, col = 'cornflowerblue', main='ACF Modelo Diferenciado')

pacf(walk, col = 'cornflowerblue', main='PACF Caminata Aleatoria')
pacf(ts_i1, col = 'cornflowerblue', main='PACF Modelo Diferenciado')

## MODELOS ARIMA

# Crear una simulación ARI(4,1)
ts_ari <- -15 + arima.sim(
  model=list(order=c(4,1,0), ar=c(0.2,0.2,0.2,0.2)), n=1000)

# Observar las gráficas del modelo ARI(4,1), ACF y PACF
par(mfrow = c(2,2))
plot(ts_ari,type = 'l', col = 'coral', main='ARI(4,1)', xlab = 't', ylab='x_t')
acf(ts_ari, col = 'coral', main='ACF ARI(4,1)')
pacf(ts_ari, col = 'coral', main='PACF ARI(4,1)')

# Crear una simulación IMA(1,4)
ts_ima <- -15 + arima.sim(
  model=list(order=c(0,1,4), ma=c(0.2,0.2,0.2,0.2)), n=1000)

# Observar las gráficas del modelo IMA(1,4), ACF y PACF
par(mfrow = c(2,2))
plot(ts_ima,type = 'l', col = 'coral', main='IMA(1,4)', xlab = 't', ylab='x_t')
acf(ts_ima, col = 'coral', main='ACF IMA(1,4)')
pacf(ts_ima, col = 'coral', main='PACF IMA(1,4)')

# Crear una simulación ARIMA(4,1,4)
ts_arima <- -15 + arima.sim(
  model=list(order=c(4,1,4), 
             ar = c(0.2,0.2,0.2,0.2), 
             ma=c(0.2,0.2,0.2,0.2)), 
  n=1000)

# Observar las gráficas del modelo ARIMA(4,1,4), ACF y PACF
par(mfrow = c(2,2))
plot(ts_arima,type = 'l', col = 'coral', main='ARIMA(4,1,4)', xlab = 't', ylab='x_t')
acf(ts_arima, col = 'coral', main='ACF ARIMA(4,1,4)')
pacf(ts_arima, col = 'coral', main='PACF ARIMA(4,1,4)')

# Datos reales
exch <- read.table("../data/pounds_nz.dat", header = T)
exch.ts <- ts(exch, st = 1991, fr = 4) 
exch.tsib <- as_tsibble(exch.ts) %>%
  dplyr::rename(
    "fecha" = "index",
    "xRate" = "value"
  )

# Forzar un modelo ARIMA(p,d,q) a los datos
arima_model <- exch.tsib %>%
  model(ARIMA(
    xRate ~ 1 + 
      pdq(p=0:10, d=0:2, q=0:10) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(arima_model)

# Ver gráficamente predicciones
plot_arima <- augment(arima_model)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = xRate, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Tipo de Cambio Libras - NZD",
    subtitle = "+ Forecast Auto ARIMA (ARMA?)",
    x = "Fecha",
    y = "Tipo de Cambio"
  ) +
  autolayer(forecast(arima_model, h=20), colour="blue")
plot_arima <- ggtema(plot_arima)

plot_arima

# Y si intentamos forzar un ARIMA con d=1?
arima_dif <- exch.tsib %>%
  model(ARIMA(
    xRate ~ 1 + 
      pdq(p=0:10, d = 1, q=0:10) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(arima_dif)

# Ver gráficamente predicciones
plot_arima_dif <- augment(arima_dif)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = xRate, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Tipo de Cambio Libras - NZD",
    subtitle = "+ Forecast ARIMA d=1",
    x = "Fecha",
    y = "Tipo de Cambio"
  ) +
  autolayer(forecast(arima_dif, h=20), colour="blue")
plot_arima_dif <- ggtema(plot_arima_dif)

plot_arima_dif

# Análisis de residuales
arima_dif %>% gg_tsresiduals()

# Gráfico de dispersión residuales
augment(arima_dif)%>%
  ggplot(aes(x=.fitted,y=.innov))+
  geom_point()+
  geom_smooth(se=FALSE)

# Prueba formal
augment(arima_dif) %>%
  features(.innov, ljung_box, dof = 4, lag = 24)

# Análisis de criterios bondad ajuste
glance(arima_model) %>%
  dplyr::select(AIC, AICc, BIC)

glance(arima_dif) %>%
  dplyr::select(AIC, AICc, BIC)

### ¿Y qué sucede nuevamente con la serie no estacionaria?

# Cargar los Datos
co2_ts <- as_tsibble(co2)  %>%
  dplyr::rename(
    "fecha" = "index",
    "ppm" = "value"
  )

co2_ts <- fill_gaps(co2_ts, .full=TRUE)

# Gráficas Iniciales
co2_ts %>% gg_tsdisplay(ppm, plot_type="partial") +
  labs(subtitle = "CO2 en la Atmósfera (PPM)")

# Forzar un modelo ARIMA a los datos CO2
arima_co2 <- co2_ts %>%
  model(ARIMA(
    ppm ~ 1 + 
      pdq(p=0:10, d=0:2, q=0:10) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(arima_co2)

# Ver gráficamente predicciones
plot_arima_co2 <- augment(arima_co2)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = ppm, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "+ Forecast ARIMA(p,d,q)",
    x = "Fecha",
    y = "PPM Promedio"
  ) +
  autolayer(forecast(arima_co2, h=365), colour="blue")
plot_arima_co2 <- ggtema(plot_arima_co2)

plot_arima_co2


# Y si agregamos un componente estacional?
sarima_model <- co2_ts %>%
  model(ARIMA(ppm ~ 1 + 
                pdq(0:3, 0:1, 0:3) + 
                PDQ(0:3, 0:1, 0:3)))

report(sarima_model)

# Ver gráficamente predicciones
plot_sarima_co2 <- augment(sarima_model)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = ppm, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "+ Forecast SARIMA AUTO",
    x = "Fecha",
    y = "PPM Promedio"
  ) +
  autolayer(forecast(sarima_model, h=365), colour="blue")
plot_sarima_co2 <- ggtema(plot_sarima_co2)

plot_sarima_co2

# Parámetros adicionales útiles para encontrar modelos:
# ic: Criterio de información
# https://otexts.com/fpp3/arima-r.html
# stepwise: No prueba TODOS los estados sino a partir de 4 estados iniciales
# greedy: Se mueve de estado en cuanto encuentra un modelo mejor
sarima_full <- co2_ts %>%
  model(ARIMA(ppm ~ 1 + 
                pdq(0:3, 0:1, 0:3) + 
                PDQ(0:3, 0:1, 0:3),
              ic="bic", 
              stepwise=F, 
              greedy=F))

report(sarima_full)

# Ver gráficamente predicciones
plot_sarima_full <- augment(sarima_full)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = ppm, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "+ Forecast SARIMA AUTO (Búsqueda completa)",
    x = "Fecha",
    y = "PPM Promedio"
  ) +
  autolayer(forecast(sarima_full, h=365), colour="blue")
plot_sarima_full <- ggtema(plot_sarima_full)

plot_sarima_full

# Ajustando parámetros manualmente
sarima_manual <- co2_ts %>%
  model(ARIMA(ppm ~ 1 + pdq(1, 1, 3) + PDQ(2, 1, 2), 
              ic="bic"))

report(sarima_manual)

# Ver gráficamente predicciones
plot_sarima_manual <- augment(sarima_manual)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = ppm, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "+ Forecast SARIMA MANUAL",
    x = "Fecha",
    y = "PPM Promedio"
  ) +
  autolayer(forecast(sarima_manual, h=365), colour="blue")
plot_sarima_manual <- ggtema(plot_sarima_manual)
plot_sarima_manual

# Comprobemos contra Información Actual!
co2_actual <-  read.csv('https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.csv', header = TRUE, sep = ",", skip = 35) %>%
  filter(
    average != -999.99 
  ) %>%
  mutate(
    fecha = lubridate::make_date(year, month, day),
    ppm = average
  ) %>%
  select(
    ppm, fecha
  ) %>%
  filter(
    fecha >= as.Date("1997-01-01") 
  ) %>%
  as_tsibble(index = fecha)

# Data wrangling
co2_actual <- fill_gaps(co2_actual, .full=TRUE)
co2_actual_mes <- 
  co2_actual[which(year(co2_actual$fecha) > 1996), ] %>%
  mutate(fecha = yearmonth(fecha))
co2_actual_mes <- 
  aggregate(co2_actual_mes$ppm, list(co2_actual_mes$fecha), FUN=mean) %>%
  dplyr::rename(fecha = Group.1, ppm = x) %>%
  as_tsibble(index=fecha)

# Predicción contra REAL!
forecast_v_real <- autoplot(co2_ts, .vars = ppm) +
  autolayer(co2_actual, .vars = ppm, colour="black") + 
  autolayer(forecast(sarima_manual, h=350), colour="blue", alpha = 0.35) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "Real VS Proyección",
    x = "Mes",
    y = "PPM Promedio"
  ) 
forecast_v_real <- ggtema(forecast_v_real)

forecast_v_real
