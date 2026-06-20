library(tidyverse)
library(forecast)
library(knitr)
library(gridExtra)
library(tsibble)
library(fable)
library(feasts)

# Vamos a hacer una función que aplique tema a nuestros gráficos
ggtema <- function(graf = NULL) {
  graph <- graf +
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

# Directorio donde se encuentra este cuaderno
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L05-Regresión/Scripts")

# Leer datos
pasUS <- read.csv("../data/pasajeros.csv")
head(pasUS)

# Convertir columna total en valores numéricos
# Transformación entre 1M (mostrará millones)
pasUS$TOTAL <- parse_number(pasUS$TOTAL)/1000000

# Visualizar ST
plot(pasUS$TOTAL,
     type = 'l',
     ylab = 'Pasajeros (millones)')

# Convertir en serie de tiempo
pasUS.st <- ts(pasUS[,3], 
             start = c(2010,1), 
             end=c(2019,12), 
             frequency = 12)

# Lambda Automática para transformación BoxCox
lambdaPas <- BoxCox.lambda(pasUS.st)
lambdaPas

# Nuevo Objeto!! TSIBBLE
pas.tsib <- as_tsibble(pasUS.st) %>%
  mutate(Total = value,
         BoxCox = BoxCox(Total, lambdaPas)) %>%
  dplyr::select(index, Total, BoxCox)

# Gráfico Original
plotOrig <- pas.tsib %>%
  ggplot(aes(x = index, y = Total)) +
  geom_line()+
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = "Nacionales e Internacionales",
       y = "Personas (Milliones)", x = "Tiempo")
# Aplicamos función de tema
plotOrig <- ggtema(plotOrig)

# Gráfico Transformado (BoxCox)
plotTrans <- pas.tsib %>%
  ggplot(aes(x = index, y = BoxCox)) +
  geom_line()+
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = paste("Transformación BoxCox (lambda = ", 
                        round(lambdaPas,4),
                        ")", sep = ""
                        ),
       y = "BoxCox", x = "Tiempo")
# Aplicamos función de tema
plotTrans <- ggtema(plotTrans)

# Ver ambos gráficos
grid.arrange(plotOrig, 
             plotTrans, 
             nrow=2)

### Modelos de regresión

# Modelo tendencia lineal
reg_lineal <- pas.tsib %>% 
  model(trend_model = TSLM(Total ~ trend()))
reg_lineal %>% report()

# Modelo tendencia cuadrática
reg_cuad <- pas.tsib %>% 
  model(trend_model = TSLM(Total~ trend() + I(trend()^2)))
reg_cuad %>% report()

# Modelo tendencia lineal + estacionalidad
reg_lin_est <- pas.tsib %>% 
  model(trend_model = TSLM(Total ~ trend() + season()))
reg_lin_est %>% report()

# Modelo tendencia cuadrática + estacionalidad
reg_cuad_est <- pas.tsib %>% 
  model(trend_model = TSLM(Total~ trend() + I(trend()^2) + season()))
reg_cuad_est %>% report()

### GRÁFICOS

# Gráfica de predicciones con modelo tendencia lineal
plt_lineal <- augment(reg_lineal)%>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = Total, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste"), alpha = 0.7) +
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = "Predicciones modelo tendencia lineal",
       y = "Personas (Milliones)", x = "Tiempo")
plt_lineal <- ggtema(plt_lineal)

# Gráfica de predicciones con modelo tendencia cuadrática
plt_cuad <- augment(reg_cuad)%>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = Total, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste"), alpha = 0.7) +
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = "Predicciones modelo tendencia cuadrática",
       y = "Personas (Milliones)", x = "Tiempo")
plt_cuad <- ggtema(plt_cuad)

# Gráfica de predicciones con modelo tendencia lineal y estacionalidad
plt_lin_est <-augment(reg_lin_est)%>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = Total, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste"), alpha = 0.7) +
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = "Predicciones modelo tendencia lineal + estacionalidad",
       y = "Personas (Milliones)", x = "Tiempo")
plt_lin_est <- ggtema(plt_lin_est)

# Gráfica de predicciones con modelo tendencia cuadrática y estacionalidad
plt_cuad_est <- augment(reg_cuad_est)%>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = Total, colour = "Datos")) +
  geom_line(aes(y = .fitted, colour = "Ajuste"), alpha = 0.7) +
  labs(title = "Pasajeros en vuelos desde USA para 2010-2019",
       subtitle = "Predicciones modelo tendencia cuadrática + estacionalidad",
       y = "Personas (Milliones)", x = "Tiempo")
plt_cuad_est <- ggtema(plt_cuad_est)

grid.arrange(plt_lineal,plt_cuad,
             plt_lin_est,plt_cuad_est, 
             nrow = 2, 
             ncol = 2)

### Criterios de los modelos

# Modelo tendencia lineal
glance(reg_lineal) %>%
  dplyr::select(r_squared, adj_r_squared, CV, AIC, BIC)

# Modelo tendencia cuadrática
glance(reg_cuad) %>%
  dplyr::select(r_squared, adj_r_squared, CV, AIC, BIC)

# Modelo tendencia lineal + estacionalidad
glance(reg_lin_est) %>%
  dplyr::select(r_squared, adj_r_squared, CV, AIC, BIC)

# Modelo tendencia cuadrática + estacionalidad
glance(reg_cuad_est) %>%
  dplyr::select(r_squared, adj_r_squared, CV, AIC, BIC)

### GRÁFICOS DE RESIDUALES

# Gráficos básicos de cada modelo
reg_lineal %>% gg_tsresiduals()
reg_cuad %>% gg_tsresiduals()
reg_lin_est %>% gg_tsresiduals()
reg_cuad_est %>% gg_tsresiduals()

# Gráfico de dispersión de modelo seleccionado
augment(reg_cuad_est)%>%
  ggplot(aes(x=.fitted,y=.innov))+
  geom_point()+
  geom_smooth(se=FALSE)

# Gráfico de cajas por mes de modelo seleccionado
augment(reg_cuad_est)%>%
  mutate(month=month(index,label=TRUE))%>%
  ggplot(aes(x=month,y=.innov)) +
  geom_boxplot()

# Prueba formal
augment(reg_cuad_est) %>%
  features(.innov, ljung_box, dof = 14, lag = 24)

### Predicción con modelo seleccionado

# Tsibble de datos futuros
predFutur <- new_data(pas.tsib, n = 36)

# Gráfico incluyendo los datos originales y forecast
autoplot(
  forecast(reg_cuad_est, new_data = predFutur),
  pas.tsib
)

