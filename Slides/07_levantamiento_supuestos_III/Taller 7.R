# HETEROCEDASTICIDAD
library(car)
library(lmtest)
library(wooldridge)

data("gpa3")
# load packages (which needed to be installed):


# Estimate model (only for spring data):
reg <- lm(cumgpa~ sat + hsperc + tothrs + female + black + white, data = gpa3, subset=(spring==1))

# Usual SE:
coeftest(reg)

# Refined White heteroscedasticity-robust SE:
coeftest(reg, vcov = hccm)


myH0 <- c("black", "white")

# Usual VCOV:
linearHypothesis(reg, myH0)

# Refined White VCOV:
linearHypothesis(reg, myH0, vcov=hccm)

# Classical White VCOV:
linearHypothesis(reg, myH0, vcov = hccm(reg, type="hc0"))

data("hprice1")

# Estimate model:
reg <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
reg

# Automatic BP test:
bptest(reg)

# Manual regression of squared residuals:
summary(lm(resid(reg)^2 ~ lotsize + sqrft + bdrms, data = hprice1))


# Estimate model:
reg <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data = hprice1)
reg

# BP test:
bptest(reg)

# White test:
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2))

d401k <- foreign::read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/401ksubs.dta")

# OLS (onlfy for singles: fsize==1):
lm(nettfa ~ inc + I((age-25)^2) + male + e401k, data = d401k, subset=(fsize==1))

# WLS:
lm(nettfa ~ inc + I((age-25)^2) + male + e401k, weight=1/inc, data = d401k, subset=(fsize==1))


# WLS:
wlsreg <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k, weight=1/inc, data = d401k, subset=(fsize==1))

# non-robust results:

coeftest(wlsreg)

# robust results:
coeftest(wlsreg,hccm)


data("smoke")

# OLS:
olsreg <- lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = smoke)
olsreg

# BP-Test:
bptest(olsreg)

# FGLS: estimation of the variance function:
logu2 <- log(resid(olsreg)^2)
varreg <- lm(logu2 ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = smoke)

# FGLS: WLS:
w <- 1/exp(fitted(varreg))
lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, weight = w, data = smoke)

# BOOTSTRAP

# (1) Introducción
# Bootstrap es un método de muestreo aleatorio con reemplazo. Entre sus otras aplicaciones, como la prueba de 
# hipótesis, es un enfoque simple pero poderoso para verificar la estabilidad de los coeficientes de regresión. 
# En nuestro artículo anterior, exploramos la prueba de permutación, que es un concepto relacionado pero que se 
# ejecuta sin reemplazo.
# La regresión lineal se basa en varios supuestos, y los coeficientes de las fórmulas probablemente se 
# distribuyen normalmente en el TCL. Muestra que, en promedio, si repitiéramos el experimento miles y miles de 
# veces, la línea estaría en intervalos de confianza.
# El enfoque bootstrap no se basa en esas suposiciones *, sino que simplemente realiza miles de estimaciones.
# * (Tenga en cuenta que el enfoque de bootstrap no viola ni pasa por alto los supuestos de normalidad, pero en 
# lugar de confiar en el TCL, construye su propia distribución de Bootstrap, que es asintóticamente normal).
# Exploraremos el método Bootstrapping y estimaremos los coeficientes de regresión de los datos
# simulados utilizando R.

# (2) Simulación de conjunto de datos
# Simularemos un conjunto de datos de una variable exploratoria de la distribución gaussiana y una variable de 
# respuesta construida agregando ruido aleatorio a la variable exploratoria. Los datos de población tendrían 
# 1000 observaciones.

set.seed(1989)
n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)
population.data <- as.data.frame(cbind(x, y))

# Tomaremos una muestra de 20 observaciones de estos datos.

sample.data <- population.data[sample(nrow(population.data), 20, replace = TRUE),]

# (3) Modelos de regresión simple
# Exploremos los modelos de regresión simple tanto para la población como para los datos de muestra:

population.model <- lm(y ~ x, data = population.data)
summary(population.model)

sample.model <- lm(y ~ x, data = sample.data)
summary(sample.model)

# Podemos ver que la intersección está sesgada para los datos de la muestra; sin embargo, el coeficiente de 
# pendiente está muy cerca del de la población, aunque solo tenemos 20 observaciones en nuestro conjunto de 
# datos de muestra. Los errores estándar son mucho más altos para el modelo de muestra.
# 
# Si graficamos los modelos, podemos ver qué tan cerca están las líneas:

plot(population.data$x, population.data$y)

abline(population.model, col = "red")
abline(sample.model, col = "blue")

# (4) Enfoque Bootstrap
# El enfoque Bootstrap plantea una pregunta: ¿qué pasaría si volvemos a muestrear los datos con reemplazo y 
# estimamos los coeficientes, qué tan extremo sería?
# 
# Aquí hay una simulación simple de 1000 ensayos, que vuelve a muestrear con reemplazo de estas 20 
# observaciones de nuestro conjunto de datos de muestra, ejecuta el modelo de regresión y guarda los 
# coeficientes que obtenemos allí. Al final, tendríamos 1000 pares de coeficientes.

# Containers for the coefficients
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:1000) {
  # Creo un nuevo conjunto de datos con reemplazo a partir de sample.data 
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]
  
  #Corro la regresión para cada nueva muestra
  model_bootstrap <- lm(y ~ x, data = sample_d)
  
  #Guardo los coeficientes
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}

# Tomamos el promedio de estos coeficientes y los compararíamos con los otros modelos que obtuvimos 
# anteriormente:

means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1))
knitr::kable(round(
  cbind(
    population = coef(summary(population.model))[, 1],
    sample = coef(summary(sample.model))[, 1],
    bootstrap = means.boot),4), 
  "simple", caption = "Coefficients in different models")
  
# Podemos ver, en este ejemplo en particular, que la constante está más cerca del modelo de población y la
# pendiente está aproximadamente al mismo nivel de precisión que el modelo de muestra.
# Pero lo que más nos interesa es la precisión de los intervalos de confianza.

confint(population.model)
confint(sample.model)
a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))

c <-
  round(cbind(
    population = confint(population.model),
    sample = confint(sample.model),
    boot = rbind(a, b)), 4)
colnames(c) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
knitr::kable(rbind(
  c('population',
    'population',
    'sample',
    'sample',
    'bootstrap',
    'bootstrap'),c))
# Podemos ver que la precisión es casi idéntica a la del modelo de muestra, e incluso un poco más ajustada para 
# la intersección.


# (5) La representación gráfica

#Predicting on new data
new.data = seq(min(x), max(x), by = 0.05)
conf_interval <-
  predict(
    sample.model,
    newdata = data.frame(x = new.data),
    interval = "confidence",
    level = 0.95)


#Plotting the results on the project step-by-spet
plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))
abline(coef(population.model)[1], coef(population.model)[2], col = "blue")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "black",
       lty = 2, lwd = 3)
abline(mean(sample_coef_intercept),
       mean(sample_coef_x1),
       col = "green",
       lty = 4, lwd=3)
lines(new.data, conf_interval[, 2], col = "black", lty = 3, lwd=3)
lines(new.data, conf_interval[, 3], col = "black", lty = 3, lwd=3)
legend("topleft",
       legend = c("Bootstrap", "Population", 'Sample'),
       col = c("red", "blue", 'green'),
       lty = 1:3,
       cex = 0.8)

# Podemos concluir que el enfoque de bootstrapping devuelve esencialmente los mismos resultados pero de una 
# manera estadísticamente diferente. No nos basamos en suposiciones, sino que simulamos los datos utilizando 
# un método de fuerza bruta. Esto podría ser especialmente útil cuando tenemos dudas sobre la distribución de 
# la que provienen los datos, o queremos verificar la estabilidad de los coeficientes, particularmente para 
# pequeños conjuntos de datos.
# 
# (6) Conclusión
# 
# Hemos explorado el enfoque bootstrap para estimar coeficientes de regresión. Usamos un modelo de regresión 
# simple para simplificar y representar claramente esta poderosa técnica. Concluimos que este enfoque es 
# esencialmente igual a los modelos MCO, sin embargo, sin depender de los supuestos. Es un método poderoso
# para estimar la incertidumbre de los coeficientes y puede usarse junto con los métodos tradicionales para 
# verificar la estabilidad de los modelos.