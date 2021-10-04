# install.packages("strucchange")
# install.packaes("wooldridge")
# install.packages("stargazer)
# install.packages("car")
library(car)
library(MASS)
library(dplyr)
library(stargazer)
library(wooldridge)
library(strucchange)

##############################################################################################
########################################## Taller 6 ##########################################
##############################################################################################

## EJERCICIO 0

# (A) Fake data

# Se usa una prueba de Chow para probar si los coeficientes en dos modelos de regresión 
# diferentes  en diferentes conjuntos de datos son iguales.
# 
# Esta prueba se usa generalmente en el campo de la econometría con datos de series de tiempo 
# para determinar si hay una ruptura estructural en los datos en algún momento.
# 
# Este tutorial proporciona un ejemplo paso a paso de cómo realizar una prueba de Chow en R.

# (i) Paso 1: crear los datos
# Primero, crearemos algunos datos falsos:

# crear datos
data <- data.frame (x = c (1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 10, 10,
                           11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 20),
                    y = c (3, 5, 6, 10, 13, 15, 17, 14, 20, 23, 25, 27, 30, 30, 31,
                           33, 32, 32, 30, 32, 34, 34, 37, 35, 34, 36, 34, 37, 38, 36))

# ver las primeras seis filas de los datos
head(datos)

# (ii) Paso 2: Visualice los datos
# A continuación, crearemos un diagrama de dispersión simple para visualizar los datos:

# creo diagrama dispersión

plot(data$x, data$y, type = "p", col = "blue", lwd = 3)

# En la gráfica de dispersión podemos ver que el patrón en los datos parece cambiar en x = 10.
# Por lo tanto, podemos realizar la prueba de Chow para determinar si hay un punto de ruptura 
# estructural en los datos en x = 10.
# 
# (iii) Realice la prueba de Chow
# Podemos utilizar el sctest función de la strucchange paquete para realizar una prueba 
# de Chow:


# realizar prueba de Chow 
sctest(data$y ~ datos$x, type = "Chow", point = 10)

# Desde el resultado de la prueba podemos ver:
#   
# - Estadística de la prueba F : 110,14
# - valor p: <.0000
# 
# Dado que el valor p es menor que .05, podemos rechazar la hipótesis nula de la prueba. Esto 
# significa que tenemos suficiente evidencia para decir que existe un punto de ruptura estructural 
# en los datos.
# 
# En otras palabras, dos líneas de regresión pueden ajustarse al patrón en los datos de manera más
# efectiva que una sola línea de regresión.

# (B) Datos verdaderos: wage 1 de Wooldridge

# A partir de la importancia de los test de cambios estructurales en los resultados de los modelos 
# obtenidos, la siguiente entrada muestra un ejemplo de cómo realizar un test de cambio estructural 
# de Chow en R. En este ejemplo, se utiliza la base de dato “wage1” sobre salarios, disponible en 
# el texto de econometría de Wooldridge. De forma específica, se desea verificar si existen 
# diferencias entre los retornos de mujeres y hombres.
# 
# Paso 1: Importar las bases de datos y verificar las variables disponibles
# 
# Dado que el test considera conocido el punto de quiebre estructural, es usual un análisis 
# descriptivo formal de las bases, que permita consolidar en el analista, una idea del punto de 
# quiebre estructural. En este ejemplo con datos transversales, suponemos este punto lo marcan 
# diferencias de genero entre hombres y mujeres. 

data("wage1")
attach(wage1)

str(wage1)


# Paso 2: Estimar los modelos para la muestra completa y para las muestras entre las cuales se 
# desea testear el quiebre estructural.
# 
# En esta paso necesitamos estimar tres modelos, el primero corresponde al modelo no restringido
# (para la muestra completa, modelNR); posteriormente, se estima el modelo para hombres y para 
# mujeres, utilizando indexación de nuestra base de datos, wage1[female==1,]. 
# 
# Los resultados de estos modelos se muestran en una tabla de regresión utilizando la librería 
# stargazer.

modelNR <- lm(lwage ~ 1 + educ + exper + tenure, data = wage1)
modeln1 <- lm(lwage ~ 1 + educ + exper + tenure, data = wage1[female==1,])
modeln2 <- lm(lwage ~ 1 + educ + exper + tenure, data = wage1[female==0,])

# Resultados
stargazer(modelNR, modeln1, modeln2, type = "text", title = "Results", align = TRUE)

## Results
## ===========================================================================================
##                                               Dependent variable:                         
##                     -----------------------------------------------------------------------
##                                                      lwage                                
##                               (1)                     (2)                     (3)         
## -------------------------------------------------------------------------------------------
## educ                       0.092***                0.080***                0.096***       
##                             (0.007)                 (0.010)                 (0.009)       
##                                                                                            
## exper                       0.004**                  0.002                 0.008***       
##                             (0.002)                 (0.002)                 (0.002)       
##                                                                                            
## tenure                     0.022***                 0.010*                 0.018***       
##                             (0.003)                 (0.005)                 (0.004)       
##                                                                                            
## Constant                   0.284***                 0.356**                 0.322**       
##                             (0.104)                 (0.141)                 (0.139)       
##                                                                                           
## -------------------------------------------------------------------------------------------
## Observations                  526                     252                     274         
## R2                           0.316                   0.212                   0.365        
## Adjusted R2                  0.312                   0.202                   0.358        
## Residual Std. Error    0.441 (df = 522)        0.397 (df = 248)        0.428 (df = 270)   
## F Statistic         80.391*** (df = 3; 522) 22.233*** (df = 3; 248) 51.836*** (df = 3; 270)
## =======================================================
## Note:                                                           *p<0.1; **p<0.05; ***p<0.01

# Paso 3: Obtener el estadístico F del test y el estadístico de tabla
# 
# A partir de las suma del cuadrado de los residuos obtenidos en los modelos anteriores 
# sum(residuals(modelNR)^2), y la obtención de los grados de libertad del test k y n1n2k, se 
# puede obtener el estadístico calculado asociado al test, para finalmente compararlo con el 
# valor del estadístico de tabla a un determinado nivel de significancia.



SRCnr <- sum(residuals(modelNR)^2)
SRCn1 <- sum(residuals(modeln1)^2)
SRCn2 <- sum(residuals(modeln2)^2)

# H0 b1=b2=b
k <- length(coefficients(modeln1))
n1n2k <- sum(female == 1) + sum(female == 0) - k

Fc <- ((SRCnr - (SRCn1 + SRCn2)) * n1n2k)/((SRCn1 + SRCn2) * k)
Ft <- qf(0.05, k, n1n2k, lower.tail = FALSE)

Fc
Ft

## EJERCICIO 1 

# Utilice los datos de producción (Q), Capital (K) y trabajo (L) del archivo datos taller_6.csv 
# que se encuentra en la página del curso. Utilizando R. 
# 
# Se pide:
# 
# a) Estime una función de producción Cobb-Douglas.
# b) Pruebe la hipótesis de rendimientos constantes a escala.
# c) Realice la prueba de cambio de régimen de Chow. Asuma que usted quiere estimar si existe 
# cambio de régimen luego de la segunda guerra mundial.
# d) Realice las pruebas de estabilidad de parámetros CUSUM y CUSUM cuadrado. Comente.

## EJERCICIO 2

# (ver .pdf)

## EJERCICIO 3

# (ver .pdf)


