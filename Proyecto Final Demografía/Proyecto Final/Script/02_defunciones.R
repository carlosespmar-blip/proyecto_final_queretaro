#**************************************************************************************#
#**************************************************************************************#
#
#                         Trabajo Final del curso de Demografía
#                                         2026-1 
#                               Facultad de Ciencias UNAM
#                        Tablas de mortalidad México 2010, 2020
#
#                                   Defunciones
#         Creado por:               Carlos Espinoza - Kasandra Herrejon
#         Fecha de creación:        04-11-2025
#         Actualizado por:          Carlos Espinoza
#         Fecha de actualización:   24-11-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 carlos_es_mar@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tablas de datos ----
def_pro <- fread("data/def_pro.csv") %>% 
  .[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]


## calculo del promedio para el ano de referencia
def_pro[ , year_new := ifelse( year %in% 2009:2011, 
                               2010,
                               ifelse( year %in% 2018:2019,
                                       2019,
                                       year ) ) ]

# datos preparados de defunciones
def <- 
  def_pro[ , 
           .( deaths = mean( deaths ) ),
           .( year = year_new, sex, age ) ] 

# Guardar tabla de DEF ----
write.csv(def, "data/def", row.names = F)

#-------- FIN --------*
