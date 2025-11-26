#**************************************************************************************#
#**************************************************************************************#
#
#                         Trabajo Final del curso de Demografía
#                                         2026-1 
#                               Facultad de Ciencias UNAM
#                        Tablas de mortalidad México 2010, 2020
#
#                                   Descomponsición de Arriaga
#         Creado por:               Carlos Espinoza - Kasandra Herrejon
#         Fecha de creación:        04-11-2025
#         Actualizado por:          Carlos Espinoza
#         Fecha de actualización:   24-11-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 carlos_es_mar@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#


#Preámbulo ----

##Limpieza de graficas ----
graphics.off()

## Limpieza de memoria----
rm(list =ls() )

##Cargo de paquetes y funciones----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)
## Carga de tablas de datos----

# Preprocesamiento Tablas de Vida

lt <- fread("data/lt_mex.csv")

# Confirmar columnas requeridas
if(!all(c("year","age","mx") %in% names(lt))){
  stop("El archivo debe tener columnas: year, age, mx")
}

# Asegurar orden por edad
setorder(lt, year, age)

# Calculo de funciones

calc_life_table <- function(mx){
  
  qx <- mx / (1 + 0.5*mx)
  qx[qx > 1] <- 1
  
  lx <- numeric(length(mx)); Lx <- lx; dx <- lx; Tx <- lx
  lx[1] <- 100000
  
  for(i in seq_along(mx)){
    dx[i] <- lx[i] * qx[i]
    Lx[i] <- lx[i] - 0.5 * dx[i]
    if(i < length(mx)) lx[i+1] <- lx[i] - dx[i]
  }
  
  Tx[length(mx)] <- Lx[length(mx)]
  for(i in (length(mx)-1):1){
    Tx[i] <- Tx[i+1] + Lx[i]
  }
  
  ex <- Tx / lx
  return(list(ex=ex, lx=lx, Lx=Lx))
  
# Evaluación por año
years_to_eval <- c(2010, 2019, 2021)
lt <- lt[year %in% years_to_eval]

# Calcular tablas de vida por año
lt_full <- lt[, {
  tb <- calc_life_table(mx)
  .(age = age, mx = mx, lx = tb$lx, Lx = tb$Lx, ex = tb$ex)
}, by = year]

# Comparación de datos
arriaga_decomp <- function(yr1, yr2){
  d1 <- lt_full[year == yr1]
  d2 <- lt_full[year == yr2]
  
  # Fórmula clásica Arriaga
  Cx <- (d2$mx - d1$mx) * (d1$Lx / d1$lx[1]) 
  
  contrib <- 0.5 * Cx
  
  data.frame(
    age = d1$age,
    contrib = contrib,
    period = paste0(yr1,"→",yr2)
  )
}

# 2010 vs 2019 y 2019 vs 2021

A_10_19 <- arriaga_decomp(2010, 2019)
A_19_21 <- arriaga_decomp(2019, 2021)

# Juntar
A <- rbind(A_10_19, A_19_21)

# Ordenar de mayor contribución a menor
A_sorted <- A %>% arrange(period, desc(contrib))

# Graficas

ggplot(A, aes(x = reorder(age, contrib), y = contrib, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Descomposición de Arriaga",
    x = "Edad",
    y = "Aportación en años a Δ e₀"
  ) +
  theme_minimal(base_size = 14)

# Creación de tablas 

fwrite(A_10_19, "data/arriaga_2010_2019.csv")
fwrite(A_19_21, "data/arriaga_2019_2021.csv")
