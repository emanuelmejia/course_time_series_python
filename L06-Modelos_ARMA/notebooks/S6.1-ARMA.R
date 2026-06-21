library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(tsibble)      # Librería para crear y manejar datos temporales
library(feasts)       # Para analizar series de tiempo
library(fable)        # Provee modelos comunes de series de tiempo

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

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L06-Modelos_ARMA/Scripts")
set.seed(10)

## MODELO AUTORREGRESIVO

# Crear una simulación AR(4)
ts_ar <- -15 + arima.sim(model=list(ar=c(0.2,0.2,0.2,0.2)), n=1000)

# Observar las gráficas del modelo AR(4), ACF y PACF
par(mfrow = c(2,2))
plot(ts_ar,type = 'l', col = 'coral', main='AR(4)', xlab = 't', ylab='x_t')
acf(ts_ar, col = 'coral', main='ACF AR(4)')
pacf(ts_ar, col = 'coral', main='PACF AR(4)')

# Ahora con datos reales
exch <- read.table("../data/pounds_nz.dat", header = T)
exch.ts <- ts(exch, st = 1991, fr = 4) 
exch.tsib <- as_tsibble(exch.ts) %>%
  dplyr::rename(
    "fecha" = "index",
    "xRate" = "value"
  )

# Veamos la descomposición
exch.decom <- decompose(exch.ts) # ¿Y si la hacemos multiplicativa?
plot(exch.decom)

# Podemos hacer los mismos gráficos que aprendimos sobre las subseries
tend_exch<-exch.decom$trend
par(mfrow = c(1,1))
plot(tend_exch)

# Resumen de los valores para cada estación
boxplot(exch.ts~cycle(exch.ts))

# Ver gráficos iniciales
exch.tsib %>% gg_tsdisplay(xRate, plot_type="partial") +
  labs(subtitle = "Tipo de Cambio Libras - NZD")

# Forzar un modelo AR(p) a los datos
ar_model <- exch.tsib %>%
  model(ARIMA(
    xRate ~ 1 + pdq(p=1, d=0, q=0) + PDQ(P=0, D=0, Q=0)
  ))

report(ar_model)

# Ver gráficamente predicciones
plot_ar <- augment(ar_model) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = xRate, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Tipo de Cambio Libras - NZD",
    subtitle = "+ Forecast AR(p)",
    x = "Fecha",
    y = "Tipo de Cambio"
  ) +
  autolayer(forecast(ar_model, h=20), colour="blue")
plot_ar <- ggtema(plot_ar)

plot_ar

# Análisis de residuales
ar_model %>% gg_tsresiduals()

# Gráfico de dispersión residuales
augment(ar_model)%>%
  ggplot(aes(x=.fitted,y=.innov))+
  geom_point()+
  geom_smooth(se=FALSE)

# Prueba formal de AC residuales
# df-Predictores
# lag- lags a analizar
augment(ar_model) %>%
  features(.innov, ljung_box, dof = 1, lag = 8)

# Análisis de criterios bondad ajuste
glance(ar_model) %>%
  dplyr::select(AIC, AICc, BIC)

### MODELO PROMEDIO MÓVIL

# Crear una simulación MA(4)
ts_ma <- -15 + arima.sim(model=list(ma=rep(0.2,4)), n=1000)

# Observar las gráficas del modelo MA(3), ACF y PACF
par(mfrow = c(2,2))
plot(ts_ma,type = 'l', col = 'deeppink', main='MA(4)', xlab = 't', ylab='x_t')
acf(ts_ma, col = 'deeppink', main='ACF MA(4)')
pacf(ts_ma, col = 'deeppink', main='PACF MA(4)')

# Forzar un modelo MA(q) a los datos
ma_model <- exch.tsib %>%
  model(ARIMA(
    xRate ~ 1 + 
      pdq(p=0, d=0, q=0:5) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(ma_model)

# Ver gráficamente predicciones
plot_ma <- augment(ma_model)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = xRate, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Tipo de Cambio Libras - NZD",
    subtitle = "+ Forecast MA(q)",
    x = "Fecha",
    y = "Tipo de Cambio"
  ) +
  autolayer(forecast(ma_model, h=20), colour="blue")
plot_ma <- ggtema(plot_ma)

plot_ma

# Análisis de residuales
ma_model %>% gg_tsresiduals()

# Gráfico de dispersión residuales
augment(ma_model)%>%
  ggplot(aes(x=.fitted,y=.innov))+
  geom_point()+
  geom_smooth(se=FALSE)

# Prueba formal
augment(ma_model) %>%
  features(.innov, ljung_box)

# Análisis de criterios bondad ajuste
glance(ma_model) %>%
  dplyr::select(AIC, AICc, BIC)

### MODELO ARMA

# Crear una simulación ARMA(2,2)
ts_arma <- -15 + arima.sim(model=list(ar=rep(0.2,2), ma=rep(0.2,2)), n=1000)

# Observar las gráficas del modelo ARMA(3), ACF y PACF
par(mfrow = c(2,2))
plot(ts_arma,type = 'l', col = 'cornflowerblue', main='ARMA(2,2)', xlab = 't', ylab='x_t')
acf(ts_arma, col = 'cornflowerblue', main='ARMA(2,2)')
pacf(ts_arma, col = 'cornflowerblue', main='ARMA(2,2)')

# Forzar un modelo ARMA(p,q) a los datos
arma_model <- exch.tsib %>%
  model(ARIMA(
    xRate ~ 1 + 
      pdq(p=0:5, d=0, q=0:5) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(arma_model)

# Ver gráficamente predicciones
plot_arma <- augment(arma_model)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = xRate, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Tipo de Cambio Libras - NZD",
    subtitle = "+ Forecast ARMA(p,q)",
    x = "Fecha",
    y = "Tipo de Cambio"
  ) +
  autolayer(forecast(arma_model, h=20), colour="blue")
plot_arma <- ggtema(plot_arma)

plot_arma

# Análisis de residuales

arma_model %>% gg_tsresiduals()

# Gráfico de dispersión residuales
augment(arma_model)%>%
  ggplot(aes(x=.fitted,y=.innov))+
  geom_point()+
  geom_smooth(se=FALSE)

# Prueba formal
augment(arma_model) %>%
  features(.innov, ljung_box)

# Análisis de criterios bondad ajuste
glance(arma_model) %>%
  dplyr::select(AIC, AICc, BIC)


### COMPARACIONES SIMULACIONES

# Gráfica de los 3 modelos
par(mfrow = c(2,2))
plot(ts_ar,type = 'l', col = 'cornflowerblue', main='AR(4)', xlab = 't', ylab='x_t')
plot(ts_ma,type = 'l', col = 'cornflowerblue', main='MA(4)', xlab = 't', ylab='x_t')
plot(ts_arma,type = 'l', col = 'cornflowerblue', main='ARMA(2,2)', xlab = 't', ylab='x_t')

# Gráfica de los 3 ACF
par(mfrow = c(2,2))
acf(ts_ar, col = 'cornflowerblue', main='ACF AR(4)')
acf(ts_ma, col = 'cornflowerblue', main='ACF MA(4)')
acf(ts_arma, col = 'cornflowerblue', main='ACF ARMA(2,2)')

# Gráfica de los 3 PACF
par(mfrow = c(2,2))
pacf(ts_ar, col = 'cornflowerblue', main='PACF AR(4)')
pacf(ts_ma, col = 'cornflowerblue', main='PACF MA(4)')
pacf(ts_arma, col = 'cornflowerblue', main='PACF ARMA(2,2)')

### ¿Y qué sucede con una serie no estacionaria?

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

# Forzar un modelo ARMA a los datos CO2
arma_co2 <- co2_ts %>%
  model(ARIMA(
    ppm ~ 1 + 
      pdq(p=0:10, d=0, q=0:10) + 
      PDQ(P=0, D=0, Q=0)
  ))

report(arma_co2)

# Ver gráficamente predicciones
plot_co2 <- augment(arma_co2)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = ppm, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "CO2 en la Atmósfera (PPM)",
    subtitle = "+ Forecast ARMA(p,q)",
    x = "Fecha",
    y = "PPM Promedio"
  ) +
  autolayer(forecast(arma_co2, h=365), colour="blue")
plot_co2 <- ggtema(plot_co2)

plot_co2
