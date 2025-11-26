#**************************************************************************************#
#**************************************************************************************#
#
#                         Trabajo Final del curso de Demografía
#                                         2026-1 
#                               Facultad de Ciencias UNAM
#                        Tablas de mortalidad México 2010, 2020
#
#                                   Años Persona Vividos
#         Creado por:               Carlos Espinoza - Kasandra Herrejon
#         Fecha de creación:        04-11-2025
#         Actualizado por:          Carlos Espinoza
#         Fecha de actualización:   24-11-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 carlos_es_mar@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

#Preámbulo----

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

## Carga de tablas de datos ----
censos_pro <- fread("data/censos_pro.csv")


# Cálculo de APV 2010 ----
# Cálculo de años persona vividos (población a mitad de año)
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex, N)]
apv2010[ , year :=2010]

ggplot(apv2010, aes(x = factor(age), y = ifelse(sex == "male", -N/1000000, N/1000000), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2010",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (Millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

  # Cálculo de APV 2019 ----
  # Cálculo de años persona vividos (población a mitad de año)
  N <- expo(censos_pro[year==2010] %>% .$pop, 
            censos_pro[year==2020] %>% .$pop, 
            t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)
  
  apv2019 <- censos_pro[year==2020, .(age, sex, N)]
  apv2019[, year:=2019]
  
  ggplot(apv2019, aes(x = factor(age), y = ifelse(sex == "male", -N/1000000, N/1000000), fill = sex)) +
    geom_col(width = 0.7, alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "M"),
      breaks = scales::pretty_breaks(n = 8)
    ) +
    scale_fill_manual(
      values = c("male" = "#1f77b4", "female" = "#d62728"),
      labels = c("male" = "Hombres", "female" = "Mujeres")
    ) +
    labs(
      title = "Pirámide Poblacional 2019",
      subtitle = "Distribución por edad y sexo",
      x = "Grupo de edad",
      y = "Población mitad de año (Millones)",
      fill = "Sexo",
      caption = "Fuente: INEGI"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank()
    )
  
  # Cálculo de APV 2020 ----
  # Cálculo de años persona vividos (población a mitad de año)
  N <- expo(censos_pro[year==2010] %>% .$pop, 
            censos_pro[year==2020] %>% .$pop, 
            t_0 = "2010-06-25", t_T = "2020-03-15", t = 2020.5)
  
  apv2020 <- censos_pro[year==2010, .(age, sex, N)]
  apv2020[ , year :=2010]
  
  ggplot(apv2020, aes(x = factor(age), y = ifelse(sex == "male", -N/1000000, N/1000000), fill = sex)) +
    geom_col(width = 0.7, alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "M"),
      breaks = scales::pretty_breaks(n = 8)
    ) +
    scale_fill_manual(
      values = c("male" = "#1f77b4", "female" = "#d62728"),
      labels = c("male" = "Hombres", "female" = "Mujeres")
    ) +
    labs(
      title = "Pirámide Poblacional 2020",
      subtitle = "Distribución por edad y sexo",
      x = "Grupo de edad",
      y = "Población mitad de año (Millones)",
      fill = "Sexo",
      caption = "Fuente: INEGI"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank()
    )
  
  # Cálculo de APV 2021 ----
  # Cálculo de años persona vividos (población a mitad de año)
  N <- expo(censos_pro[year==2010] %>% .$pop, 
            censos_pro[year==2020] %>% .$pop, 
            t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)
  
  apv2021 <- censos_pro[year==2020, .(age, sex, N)]
  apv2021[, year:=2021]
  
  ggplot(apv2010, aes(x = factor(age), y = ifelse(sex == "male", -N/1000000, N/1000000), fill = sex)) +
    geom_col(width = 0.7, alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "M"),
      breaks = scales::pretty_breaks(n = 8)
    ) +
    scale_fill_manual(
      values = c("male" = "#1f77b4", "female" = "#d62728"),
      labels = c("male" = "Hombres", "female" = "Mujeres")
    ) +
    labs(
      title = "Pirámide Poblacional 2021",
      subtitle = "Distribución por edad y sexo",
      x = "Grupo de edad",
      y = "Población mitad de año (Millones)",
      fill = "Sexo",
      caption = "Fuente: INEGI"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_blank()
    )
  
  #Consolidar tablas 2010 y 2020----
  apv <- rbind(apv2010,apv2019,apv2021)
  
  #Grafica
  ggplot(data = apv, aes(x = age, y = ifelse(sex == "male", -N/100000, N/100000), fill = sex)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    facet_wrap(~year, ncol = 2) +
    coord_flip() +
    scale_fill_manual(values = c("female" = "#B19CD9", "male" = "#0050B8")) +  
    scale_y_continuous(labels = function(x) paste0(abs(x), "M"),
                       breaks = seq(-1.5, 1.5, by = .4),
                       limits = c(-1.5, 1.5)) +
    labs(title = "Pirámide Poblacional: 2010, 2019 y 2021",
         x = "Grupo de Edad",
         y = "Población (Millones)",
         fill = "Sexo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          legend.position = "bottom")

#Guardar tabla de APV
write.csv(apv, "data/apv.csv")
