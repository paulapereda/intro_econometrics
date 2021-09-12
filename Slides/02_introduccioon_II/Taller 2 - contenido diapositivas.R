########################################################
############### Taller 2 - Econometría I ###############
############### Repaso e interpretaciones ##############
########################################################

# Se corre una vez si no tengo los paquetes instalados

install.packages("dplyr")
install.packages("readr")
install.packages("wooldridge")

# Parte I: tidy

library(dplyr)
library(readr)

# Leo los datos
datos <- read_csv("http://www.lock5stat.com/datasets/HappyPlanetIndex.csv")

# Veo qué tiene los datos 

glimpse(datos)

# arrange: ordeno variables según una variable

orden <- datos %>% 
  arrange(Region)

head(orden)

orden <- datos %>% 
  arrange(-Region)

head(orden)

# select: escogiendo variables

feliz_chico <- datos %>%
  select(Country, Region, Happiness)

# filter: me enfoco en pocos casos

feliz2 <- datos %>%
  filter(Region == 2)

feliz3 <- datos %>%
  filter(Happiness > 7)

# mutate: creo nuevas variables

feliz <- datos %>%
  mutate(TotalGDP = GDPperCapita*Population)

# rename: cambio el nombre de alguna variable

datos_es <- datos %>%
  rename(pais = Country,
         felicidad = Happiness)

# group_by & summarise: resúmenes agrupados

resumen <- datos %>%
  group_by(Region) %>%
  summarise(AverageHappy = mean(Happiness))

