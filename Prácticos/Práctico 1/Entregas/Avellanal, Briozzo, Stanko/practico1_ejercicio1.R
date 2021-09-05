library(tidyverse)

#Problema 1

#Creamos la tabla de datos

gastos_familia_df <- data.frame(
  "Familia" = 1:12, 
  "gasto_mensual_vivienda" = c(300, 550,350, 1100, 640, 480, 450, 700, 670, 600, 1900, 5200))

#1.a

gasto_mensual_promedio = mean(gastos_familia_df$gasto_mensual_vivienda)
gasto_mensual_promedio 

gasto_mensual_mediana = median(gastos_familia_df$gasto_mensual_vivienda)
gasto_mensual_mediana 

#1.b

gasto_mensual_varianza = var(gastos_familia_df$gasto_mensual_vivienda)
gasto_mensual_varianza

gasto_mensual_desviacion = sd(gastos_familia_df$gasto_mensual_vivienda)
gasto_mensual_desviacion

gasto_mensual_ri = IQR(gastos_familia_df$gasto_mensual_vivienda)
gasto_mensual_ri

#1.c

gastos_familia_df2 <- gastos_familia_df %>%
  mutate(gasto_mensual_vivienda = ifelse(gastos_familia_df$Familia== 12, gastos_familia_df$gasto_mensual_vivienda *(1+0.5) , gastos_familia_df$gasto_mensual_vivienda))

gasto_mensual_promedio2 = mean(gastos_familia_df2$gasto_mensual_vivienda)
gasto_mensual_promedio2 

gasto_mensual_mediana2 = median(gastos_familia_df2$gasto_mensual_vivienda)
gasto_mensual_mediana2

gasto_mensual_varianza2 = var(gastos_familia_df2$gasto_mensual_vivienda)
gasto_mensual_varianza2

gasto_mensual_desviacion2 = sd(gastos_familia_df2$gasto_mensual_vivienda)
gasto_mensual_desviacion2

gasto_mensual_ri2 = IQR(gastos_familia_df2$gasto_mensual_vivienda)
gasto_mensual_ri2

#1.d

#En primer lugar, la media es el valor resultante al sumar todos los datos de una muestra y dividir entre el numero total de datos.
#Por otro lado, la mediana es el numero medio de un conjunto de datos, una vez que estan ordenados de menor a mayor.
#Podemos notar que cuando contamos con un valor que difiere demasiado del resto de valores (valor atipico),la media y la mediana difieren cada vez mas.
#Problema 

