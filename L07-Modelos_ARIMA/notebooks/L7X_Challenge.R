library(tidyverse)    # Varias librerías para manejo y limpieza de datos
library(tsibble)      # Librería para crear y manejar datos temporales
library(fable)        # Provee modelos comunes de series de tiempo
library(gridExtra)    # Acomodo de Gráficos
library(feasts)       # Para analizar series de tiempo

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L7-Modelos_ARIMA/Scripts")

## Lectura de archivo
CBE <- read.table("../data/cbe.dat", header = T)
CBE.ts <- ts(CBE, start = 1958, freq=12)
CBE.tsib <- as_tsibble(CBE.ts, pivot_longer = FALSE) %>%
  dplyr::rename(
    "fecha" = "index"
  )

CBE.train <- CBE.tsib %>% filter_index(~ "1988-12")
CBE.test <- CBE.tsib %>% filter_index("1989-01"~.)

# Elaborar un análisis de la serie de tiempo de Chocolate
# Ajustar un modelo apropiado (SARIMA? SARIMAX?)