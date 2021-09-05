#             PRACTICO 1 ECONOMETRIA I
#  Azadian, Bordaberry, Etchegorry, Odizzio
 
#             Problema 1 
#1A)

#Defino los datos como una matriz con titulos.
gastos_mensual <- c(300,550,350,1100,640,480,450,700,670,600,1900,5200)
fam <- c(1,2,3,4,5,6,7,8,9,10,11,12)

gastos_matrix <- cbind(fam, gastos_mensual)
colnames(gastos_matrix) <- c("Familia","Gasto Mensual")

#Promedio
mean(gastos_matrix[,2])

#Mediano
median(gastos_matrix[,2])
q2 <- median(gastos_matrix[,2])

#1B) 
#Varianza
var(gastos_matrix[,2])

#Cuartiles
quantile(gastos_matrix[,2], probs = c(.25, .5, .75))

#Rango intercuartilico.
IQR(gastos_matrix[,2])

#otra forma
#q1 <- quantile(gastos_matrix, probs = c(.25))
#q3 <- quantile(gastos_matrix, probs = c(.75))
#q3 - q1


#1C)

#con Dataframes, mutate y tidyverse
library(tidyverse)
gastos_df <- data.frame(fam,gastos_mensual)
gastos_df <- mutate(gastos_df,gastos_mensual = ifelse(fam == 12, gastos_mensual*1.5, gastos_mensual))

#Otra Forma más rustica con matrices
#gastos_mensual <- ifelse(fam == 12, gastos_mensual*1.5, gastos_mensual)
#gastos_matrix <- cbind(fam, gastos_mensual)
#colnames(gastos_matrix) <- c("Familia","Gasto Mensual")

#Promedio con 50% en fam 12
mean(gastos_df[,2])

#Mediano con 50% en fam 12
median(gastos_df[,2])

#Varianza con 50% en fam 12
var(gastos_df[,2])

#Cuartiles con 50% en fam 12
quantile(gastos_df[,2], probs = c(.25, .5, .75))

#Rango intercuartilico con 50% en fam 12
IQR(gastos_df[,2])


#1D
#La media y la mediana difieren cuando los datos estan sesgados (skewed)





#         PROBLEMA 3

#  E(GPA/SAT) = 0.7+ 0.001 SAT

#3A) Encuentre la esperanza de GPA cuando SAT = 800 y E(GPA/SAT=1400) . Comente la diferencia

E_GPA_SAT800 <- 0.7+0.001*800
E_GPA_SAT1400 <- 0.7+0.001*1400
E_GPA_SAT800
E_GPA_SAT1400

#Al aumentar el SAT, aumenta el GPA, pero menos que proporcional, y 0.7 
#del GPA es independiente del resultado del SAT (por alguna razon esta dado)

#3B) 
#Propiedad : E[E(Y/X)] = E(Y)
#E[E(GPA/SAT)] = E(GPA)

E_GPA_SAT1100 <- 0.7+0.001*1100
E_GPA_SAT1100

