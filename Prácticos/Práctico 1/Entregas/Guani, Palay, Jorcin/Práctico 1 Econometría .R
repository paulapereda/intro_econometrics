############## PRACTICO 1 ECONOMETRIA ##############
#Integrantes: Rosario Palay, Valentina Jorcin y Erika Guani


#Se crea la base de datos que contiene el gasto mensual en viviendas para 12 familias
library(dplyr)
familia <- c(1:12)
gasto_mensual <- c(300,550,350,1100,640,480,450,700,670,600,1900,5200)
gasto_vivienda <- data.frame(familia,gasto_mensual)

#1)
summary(gasto_mensual)

#1a)
#Gasto mensual promedio
promedio <- mean(gasto_vivienda$gasto_mensual)
#Gasto mensual mediano
mediana <- median(gasto_vivienda$gasto_mensual)

#1b) 
#Rango intercuartílico
rango_intercuartilico <- IQR(gasto_vivienda$gasto_mensual)

#Desvío estándar
desvio_estandar <- sd(gasto_vivienda$gasto_mensual)

#Varianza
varianza_1 <- var(gasto_vivienda$gasto_mensual)
varianza_2 <- desvio_estandar**2

#1c) Se modifica el gasto mensual de la familia 12
gasto_vivienda_2 <- gasto_vivienda %>% 
  mutate(gasto_mensual = if_else(familia == 12, 5200*1.5, gasto_mensual ))

#Se calcula nuevamente los puntos anteriores con la modificacion en la familia 12

promedio_2 <- mean(gasto_vivienda_2$gasto_mensual)
mediana_2 <- median(gasto_vivienda_2$gasto_mensual)
rango_intercuartilico_2 <- IQR(gasto_vivienda_2$gasto_mensual)
desvio_estandar_2 <- sd(gasto_vivienda_2$gasto_mensual)
varianza_3 <- var(gasto_vivienda_2$gasto_mensual)

#1d)
  # Comentario sobre la media y la mediana

#Por un lado la media se calcula sumando todos los valores y dividiendo la suma
#entre el número total de valores. Es decir que la media del gasto mensual nos dice
#cuanto es el gasto mensual de cada familia si las 12 familias gastaran lo mismo. 
#En cambio, la mediana es el valor que ocupa el lugar central de todos los datos
#cuando éstos están ordenados de menor a mayor, es decir que deja la misma cantidad
#de valores a un lado que a otro, deja un 50% de observaciones a su izquierda y un 
#50% a su derecha.Por lo que la mitad de los números son superiores 
#a la mediana y la mitad de los números tienen valores menores que la mediana.

#En este caso, al cambiar el gasto mensual de la familia 12, el promedio se modifico,
#paso de 1078,333 a 1295,mientras que la mediana permanecio incambiada, esta es 620.
#Teniendo en cuenta las definiciones de media y mediana y como se calcula cada una, siempre 
#que se modifique el gasto de alguna familia el promedio cambia.Como el gasto aumento, 
#el promedio aumento. En cambio, como la mediana es el gasto que acumula el 50% de los datos, 
#y la familia 12 tenia el gsato mas elevado, al aumentar su gasto la mediana no se modifica, 
#no cambia el gasto que acumula el 50% de los datos porque el gasto de la familia 12 era superior
#a ese valor. 

# Si los datos son simetricos, la mediana es igual a la media. Si hay asimetria positiva
#el promedio es mayor a la mediana mientras que si hay asimetria negativa, el promedio es 
#menor a la mediana. Este es un caso de asimetria positiva. 


