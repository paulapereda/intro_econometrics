# (EJERCICIO 1) El gerente de una empresa desea investigar si el gasto en publicidad realmente incrementa 
# las ventas.
# Para ello recolecta datos de la variable y (número de unidades vendidas) y de las variables x1 (gastos
# de publicidad en pesos) y x2 (precios de venta).

anio <- c("1999", "2000", "2001", "2002", "2003")
y <- c(0.5, 1, 2, 3.5, 5)
x1 <- c(0.6, 0.65, 0.8, 1.5)
x2 <- c(10, 6, 5, 4, 3)

# Se pide:
## (a) Justifique en términos económicos porque el modelo incluye una constante.
## (b) Escriba el modelo en forma matricial. Defina las siguientes matrices: Y, b, X, X’,
# (X’X) y X’Y.

y <- c(0.5, 1, 2, 3.5, 5)
x1 <- c(0.6, 0.65, 0.8, 0.8, 1.5)
x2 <- c(10, 6, 5, 4, 3)
x <- as.matrix(cbind(1, x1, x2))
x_t_x <- (t(x) %*% x)
x_t_y <-  (t(x) %*% y)
betas <- solve(x_t_x) %*% (x_t_y); betas
model <- lm(y ~ x1 + x2, data = datos); model$coefficients
model <- lm(y ~ 1 + x1 + x2); model$coefficients

## (c) Estime dicho modelo mediante el método de mínimos cuadrados ordinarios e
# interprete los coeficientes estimados. Calcule la matriz de varianzas y covarianzas.

var(x, y)
cov(x, y)

## (d) ¿Qué relación debería encontrarse entre el vector de residuos y las variables
# explicativas?

plot(x1, model$residuals)
plot(x2, model$residuals)

cor(x, model$coefficients)
cor(x1, model$coefficients)
cor(x, model$residuals)
cor(x1, model$residuals)

## (e) ¿Cuánto aumentarían las ventas si el gasto en publicidad se incrementa 0.2?

# (EJERCICIO 2) Suponga que se dispone del siguiente modelo para estimar el valor de una casa:
# y = b1 + b2 x + b3 x +b4x^2 +e
# 
# En donde y es el valor de la casa, x1 es una medida de tamaño de la casa y x2 representa la 
# distancia de la casa al centro de la ciudad. Se dispone de los siguientes datos:

y <- c(345, 238, 452, 422, 328, 375, 660, 466, 290)
x1 <- c(1650, 1870, 2230, 1740, 1900, 2000, 3200, 1860, 1230)
x2 <- c(3.5, 0.5, 1.5, 4.5, 1.8, 0.1, 3.4, 3.0, 1.0)
  
# Se pide:
## (a) Escriba el modelo en forma matricial. Defina las siguientes matrices: Y, b, X, X’,
# (X’X) y X’Y.

## (b) Estime dicho modelo mediante el método de mínimos cuadrados ordinarios. Calcule
# la matriz de varianzas y covarianzas.

## (c) Interprete los coeficientes estimados.

## (d) Estime los coeficientes utilizando R.