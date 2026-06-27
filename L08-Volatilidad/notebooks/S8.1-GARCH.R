library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(tsibble)      # Librería para crear y manejar datos temporales
library(fable)        # Provee modelos comunes de series de tiempo
library(feasts)       # Para analizar series de tiempo
library(MASS)         # Librería con datasets para aplicaciones estadísticas
library(tseries)      # Librería con modelos de series de tiempo (GARCH)
library(lmtest)       # Pruebas para modelos

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L08-Volatilidad/Scripts")

# Simulación GARCH
set.seed(10)

# Parámetros
alpha0 <- 0.1
alpha1 <- 0.4
beta1 <- 0.2

# Vectores iniciales
w <- rnorm(10000)
E <- rep(0, 10000)
h <- rep(0,10000)

# Simulación GARCH
for (t in 2:10000){
  h[t] <- alpha0 + alpha1 * (E[t-1]^2) + beta1 * h[t-1]
  E[t] <- w[t] * sqrt(h[t])
}

# Verificando autocorrelación de componentes E
plot(E,type = 'l')
plot(E^2,type = 'l')
acf(E)
acf(E^2)

# Ajustando un modelo GARCH
E.garch <- garch(E, trace = FALSE)
E.garch
confint(E.garch)

# DATOS REALES
# Cargamos datos del índice SP
data(SP500)
plot(SP500, type = 'l')
acf(SP500)

# Análisis de los valores ajustados a la media al cuadrado
SP500_aju <- (SP500 - mean(SP500))^2
plot(SP500_aju, type = 'l')
acf(SP500_aju)
pacf(SP500_aju)

# Ajustando un modelo GARCH
SP.garch <- garch(SP500, trace = F)
SP.garch
confint(SP.garch)

# Análisis de Residuales tras modelo GARCH
SP.res <- SP.garch$res[-1]
plot(SP.res, type = 'l')
plot(SP.res^2, type = 'l')
acf(SP.res)
acf(SP.res^2)

# Análisis completo
# Carga de datos
stemp <- read.table("../data/stemp.dat")

stemp <- stemp %>% pivot_longer(
  cols= starts_with("V"),
  values_to="temperatura"
)

# Observando la serie
stemp.ts <- ts(stemp$temperatura, start = 1850, freq = 12)
plot(stemp.ts)

# Convertimos a tsibble
stemp.tsib <- as_tsibble(stemp.ts) %>%
  dplyr::rename(
    "fecha" = "index",
    "Temperatura" = "value"
  )

# Selección de un modelo SARIMA
stemp.model <- stemp.tsib %>%
  model(ARIMA(
    Temperatura ~ 1 + 
      pdq(p=0:2, d=0:1, q=0:2) + 
      PDQ(P=0:2, D=0:1, Q=0:2),
    stepwise = TRUE,  # Uso de metodología stepwise para encontrar modelo ágilmente https://www.jstatsoft.org/article/view/v027i03/v27i03.pdf
    greedy = TRUE, # Moverse a la siguiente mejor opción inmediatamente
    ic = "bic" # Criterio para selección de modelo
  ))
report(stemp.model)
coef(stemp.model)

# Gráfico con Forecast
steps <- 180
plot_stemp <- augment(stemp.model)%>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = Temperatura, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste")) +
  labs(
    title = "Variación en temperatura Hemisferio Sur",
    subtitle = "+ Forecast SARIMA(p,d,q)(P,D,Q)",
    x = "Fecha",
    y = "Temperatura"
  ) +
  autolayer(forecast(stemp.model, h=steps), colour="blue")
plot_stemp

# Revisión de Residuales
stemp.model %>% gg_tsresiduals()

# Correlogramas de residuales
stemp.res <- augment(stemp.model)[".innov"]
acf(stemp.res)
acf(stemp.res^2)

# Ajuste de un modelo GARCH
stemp.garch <- garch(stemp.res, trace = F)
stemp.garch
t(confint(stemp.garch))
coeftest(stemp.garch)

# Revisión de Residuales de modelo GARCH
stemp.garch.res <- resid(stemp.garch)[-1]
plot(stemp.garch.res, type = 'l')
plot(stemp.garch.res^2, type = 'l')
acf(stemp.garch.res)
acf(stemp.garch.res^2)

# ¿Cómo utilizarlo? Simulaciones

# Funcion Garch sumando el forecast puntual
garch.sim <- function(puntos=1000,
                      a0 = 0.1,
                      a1 = 0.4,
                      b1 = 0.2,
                      vecFore = 0){
  w = rnorm(puntos)
  E <- rep(0, puntos)
  h <- rep(0,puntos)
  
  for (t in 2:puntos){
    h[t] <- a0 + a1 * (E[t-1]^2) + b1 * h[t-1]
    E[t] <- w[t] * sqrt(h[t])
  }
  return(E + vecFore)
}

# Parámetros para GARCH
numpuntos <- steps
a0.stem <- coef(stemp.garch)[1]
a1.stem <- coef(stemp.garch)[2]
b1.stem <- coef(stemp.garch)[3]
foreStem <- forecast(stemp.model, h=steps)$.mean

# Crear un DF con índice que replique 100 veces la función
sim_garch <- data.frame(
  t=seq(from=1, to=numpuntos,by=1),
  replicate(
    garch.sim(
      numpuntos, 
      a0.stem, 
      a1.stem, 
      b1.stem, 
      foreStem),
    n=100)
)

# Convertir en DF de columna
sim_garch <- sim_garch %>% pivot_longer(
  cols=starts_with("x"),
  names_to="sim",
  values_to="value"
)

# Gráfica de las 100 simulaciones
ggplot(sim_garch,aes(x=t,y=value, col=sim))+
  geom_line(alpha = 0.3)+
  labs(title = "Predicciones GARCH",
       subtitle = "100 Simulaciones",
       x = "t",
       y = "xt") +
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
    axis.line = element_line(color = "#969696"),
    legend.position="none"
  ) 
