
#defino el gasto mensual de las viviendas como un vector de valores
familia <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
gasto_mensual_vivienda <- c(300, 550, 350, 1100, 640, 480, 450, 700, 670, 600, 1900, 5200)

#calculo la media de la variable con la funcion "mean"
mean(gasto_mensual_vivienda)

#calculo la mediana de la variable con la funcion "median"
median(gasto_mensual_vivienda)

#calculo la desviacion estandar de la variable con la funcion "sd"
sd(gasto_mensual_vivienda)

#Calculo la varianza de la variable con la funcion "variance", compruebo que sea igual al desvio estandar cuadrado
variance(gasto_mensual_vivienda)
(sd(gasto_mensual_vivienda))^2

#calculo los valores de los cuartiles. Dado que el rango intercuartilico es x(0.75) - x(0.25), 
#hago la resta con los valores hallados
quantile (gasto_mensual_vivienda)

x75 <- 800
x25 <- 472.5

rango_intercuartílico <- (x75 - x25)

#Suponga que la familia número 12 aumenta su gasto mensual en un 50%, pero los de todas las demás familias permanecen.
5200 + 5200*0.5 == 7800
nuevo_gasto_mensual_vivienda <- c(300, 550, 350, 1100, 640, 480, 450, 700, 670, 600, 1900, 7800)

#calculo la media de la nueva variable con la funcion "mean"
mean(nuevo_gasto_mensual_vivienda)

#calculo la mediana de la nueva variable con la funcion "median"
median(nuevo_gasto_mensual_vivienda)

#calculo la desviacion estandar de la nueva variable con la funcion "sd"
sd(nuevo_gasto_mensual_vivienda)

#Calculo la varianza de la nueva variable con la funcion "variance", compruebo que sea igual al desvio estandar cuadrado
variance(nuevo_gasto_mensual_vivienda)
(sd(nuevo_gasto_mensual_vivienda))^2

#calculo los valores de los cuartiles. Dado que el rango intercuartilico es x(0.75) - x(0.25), 
#hago la resta con los valores hallados
quantile (nuevo_gasto_mensual_vivienda)
x75 <- 800
x25 <- 472.5

rango_intercuartílico <- (x75 - x25)

#obs el rango intercuartilico no cambia a pesar del cambio en el consumo de la familia 12
#el cambio en la media es de 216.66667

mean(nuevo_gasto_mensual_vivienda) - mean(gasto_mensual_vivienda)

#No hay cambios en las medianas
median(nuevo_gasto_mensual_vivienda) - median(gasto_mensual_vivienda)

