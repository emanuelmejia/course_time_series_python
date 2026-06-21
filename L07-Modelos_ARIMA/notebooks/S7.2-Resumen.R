library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(tsibble)      # Librería para crear y manejar datos temporales
library(fable)        # Provee modelos comunes de series de tiempo
library(gridExtra)    # Acomodo de Gráficos
library(feasts)       # Para analizar series de tiempo

# Función para graficar
ggforecast <- function(model = NULL, 
                       forecastVar = NULL, 
                       model_name = "Modelo y predicciones",
                       forecastData = NULL) {
  graph <- augment(model)%>%
    ggplot(aes(x = fecha)) +
    geom_line(aes(y = forecastVar, colour = "Datos")) +
    geom_line(aes(y = .fitted, colour = "Ajuste")) +
    labs(
      title = paste("Forecast Consumo"),
      subtitle = model_name,
      x = "Fecha",
      y = as.character(forecastVar)
    ) +
    autolayer(forecast(model, new_data = forecastData), colour="blue") + 
    theme_classic() +
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

# Poner el propio directorio
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L07-Modelos_ARIMA/Scripts")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("../data/US_change_full.csv")

# Mostrar los primeros 10 datos
head(uschange,10)
tail(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Convertir en time series
usch.ts <- ts(uschange[,2:6], start = 1970, freq=4)

# Convertir en tsibble y modificar nombres
usch.tsib <- as_tsibble(usch.ts, pivot_longer = FALSE) %>%
  dplyr::rename(
    "fecha" = "index",
    "Consumo" = "Consumption",
    "Ingresos" = "Income",
    "Produccion" = "Production",
    "Ahorro" = "Savings",
    "Desempleo" = "Unemployment"
  )

# Crear datos de entrenamiento
usch.train <- usch.tsib %>% filter_index(~ "2014-Q4")

# Crear datos de prueba
usch.test <- usch.tsib %>% filter_index("2015"~.)

# Gráficos Iniciales
# Series incluidas
plot(usch.ts)

# Serie de Consumo
usch.tsib %>% gg_tsdisplay(Consumo, plot_type="partial") +
  labs(subtitle = "Variación en niveles de Consumo USA 1970-2019")

# Descomposición
plot(decompose(usch.ts[,1], type = "mult"))

# Estacionalidad
boxplot(usch.ts[,1]~cycle(usch.ts[,1]))

#### MODELOS
modelos <- usch.train %>%
  model(
    ar = ARIMA(Consumo ~ 1 + pdq(p=0:5, d=0, q=0) + PDQ(P=0, D=0, Q=0)),
    ma = ARIMA(Consumo ~ 1 + pdq(p=0, d=0, q=0:5) + PDQ(P=0, D=0, Q=0)),
    arma = ARIMA(Consumo ~ 1 + pdq(p=0:5, d=0, q=0:5) + PDQ(P=0, D=0, Q=0)),
    arima = ARIMA(Consumo ~ 1 + pdq(p=0:5, d=0:2, q=0:5) + PDQ(P=0, D=0, Q=0)),
    sarima = ARIMA(Consumo ~ 1 + pdq(p=0:5, d=0:2, q=0:5) + PDQ(P=0:5, D=0:2, Q=0:5)),
    sarimax = ARIMA(Consumo ~ 1 + pdq(p=0:5, d=0:2, q=0:5) + PDQ(P=0:5, D=0:2, Q=0:5) + Desempleo),
    sarimax_manual = ARIMA(Consumo ~ 1 + pdq(p=1, d=0, q=3) + PDQ(P=1, D=0, Q=1) + Desempleo + Produccion)
  )

# Reportes Iniciales
report(modelos[,"ar"])
report(modelos[,"ma"])
report(modelos[,"arma"])
report(modelos[,"arima"])
report(modelos[,"sarima"])
report(modelos[,"sarimax"])
report(modelos[,"sarimax_manual"])

# Reportes de coeficientes
coef(modelos[,"ar"])
coef(modelos[,"ma"])
coef(modelos[,"arma"])
coef(modelos[,"arima"])
coef(modelos[,"sarima"])
coef(modelos[,"sarimax"])
coef(modelos[,"sarimax_manual"])

# Ver gráficamente predicciones
plot_ar <- ggforecast(modelos[,"ar"], usch.train$Consumo, "Modelo AR", usch.test)
plot_ma <- ggforecast(modelos[,"ma"], usch.train$Consumo, "Modelo MA", usch.test)
plot_arima <- ggforecast(modelos[,"arima"], usch.train$Consumo, "Modelo ARIMA", usch.test)
plot_sarima <- ggforecast(modelos[,"sarima"], usch.train$Consumo, "Modelo SARIMA", usch.test)
plot_sarimax <- ggforecast(modelos[,"sarimax"], usch.train$Consumo, "Modelo SARIMAX", usch.test)
plot_sar_manual <- ggforecast(modelos[,"sarimax_manual"], usch.train$Consumo, "Modelo SARIMAX - Manual", usch.test)

# Acomodo en enrejado
grid.arrange(plot_ar, plot_ma,
             plot_arima, plot_sarima, 
             plot_sarimax, plot_sar_manual,
             nrow=3)

# Análisis de residuales
modelos[,"ar"] %>% gg_tsresiduals()
modelos[,"ma"] %>% gg_tsresiduals()
modelos[,"arima"] %>% gg_tsresiduals()
modelos[,"sarima"] %>% gg_tsresiduals()
modelos[,"sarimax"] %>% gg_tsresiduals()
modelos[,"sarimax_manual"] %>% gg_tsresiduals()

# Métricas de Precisión y Ajuste
modelForecasts <- forecast(modelos, new_data = usch.test)

# Precisión
acc <- accuracy(modelForecasts, usch.tsib) %>%
  dplyr::select(.model, ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE)

# Bondad de Ajuste
crit <- glance(modelos) %>%
  dplyr::select(.model, AIC, AICc, BIC)

# Tabla completa
metrics <- merge(acc, crit, by = ".model", all = T)
metrics[,2:10] <- round(metrics[2:10],2)
metrics

# Gráfico con dataset test
plot_sar_manual +
  autolayer(usch.test, .vars = Consumo, colour="black")

