######################################################################################
############################## Taller 2 - Econometría I ##############################
################################## Introducción a R ##################################
######################################################################################

install.packages(setdiff(c("tidyverse", "readr", "readxl", "haven", "kable"),
                         rownames(installed.packages())))


######################################################################################
################################### Dplyr basics #####################################
######################################################################################

# Existen 6 funciones que nos permiten resolver una amplia gama de desafíos en
# lo que concierne a manipulación de datos:

## - Levantar observaciones por sus valores: filter()
## - Reordenar filas: arrange()
## - Levantar variables por sus nombres: select()
## - Crear nuevas variables a partir de existentes mutate() (y transmute())
## - Agrupar observarciones con group_by()
## - Generar medidas de resúmen a partir de distintos valores: summarise()

######################################################################################
##################################### Helpers ########################################
######################################################################################

# Tidyverse también nos provee de varias funciones que nos permiten  seleccionar variables en 
# función de sus nombres. Alguna de ellas son:

## - starts_with(): empieza con un prefijo.  
## - ends_with(): termina con un prefijo.  
## - contains(): contiene una determinada “string”.

######################################################################################
#################################### Tidy Data #######################################
######################################################################################

# Lamentablemente los datos suelen venir desordenados, entre otras  cosas debido a que por lo 
# general no se levantan desde una lógica  adecuada para nosotros, los que los analizamos. El
# paquete tidyr nos ofrece varias funciones útiles para ordenarlos.

## - Cuando las columnas son niveles de una variable las reunimos en una sola con pivot_longer().
## - pivot_wider(): es el opuesto de pivot_longer().
## - Si queremos separar los valores de una variable lo hacemos con separate().

######################################################################################
###################################### Joins #########################################
######################################################################################

# Si queremos unir datos de la base en la cual estamos trabajando con datos de otra, solo precisamos 
# una variable de matcheo (es decir, cún a ambas) y una de la siguientes funciones que más se adecúe a
# nuestro deseo, las cuales las podemos pensar como operaciones  entre conjuntos:

## - full_join(): unión de conjuntos.
## - inner_join(): intersección de conjuntos.
## - left_join(): se queda con todas las filas de la primera base que se le pasa.
## - right_join(): se queda con todas las filas de la segunda base que se le pasa.

# (!) Observación: Se generan Na's para el caso en que la variable de matcheo presenta valores dispares 
# entre las bases que queremos joinear, según la función que seleccionemos.

######################################################################################
###################################### ELBU ##########################################
######################################################################################

# Utilizamos una base de datos extraída del Estudio Longitudinal de Bienestar en el Uruguay llevado a cabo por
# el Instituto de  Economía (IECON), el cual consiste en un relevamiento longitudinal representativo de los niños
# que concurren al sistema de educación  primaria pública.

# Se recoge información referente a múltiples dimensiones del  bienestar de los niños en la muestra como también del 
# resto de los  integrantes de sus hogares, entre los que se destacan:
## - situación nutricional.
## - habilidades cognitivas y no cognitivas.
## - imaginación, razonamiento y sentimientos.
## - actividades de ocio e interacción social.
## - logros educativos.
## - ingresos.
## - calidad de la vivienda y bienestar subjetivo, entre otros.

# Tres “Olas”
# - La primera realizada en el año 2004 a 3000 niños del primer  grado de escuela.
# - La segunda realizada en el año 2005 a los mismos niños  analizados en la primera ola.
# - La tercera realizada en el año 2012 a los mismos niños  analizados en la primera ola.

# Las olas contienen bases:
## - De personas, con información referente al niño y personas del  hogar donde reside.
## - De hogares, con información más enfocada al niño, sus  capacidades de relacionamiento, emociones, amigos, etcétera.
# Utilizaremos la base de personas correspondiente a la Tercer ola de relevamiento.


# (a) ¡Vamos a leer los datos!

# Los datos están en un archivo .csv por lo que utilizamos la función read_csv del paquete readr.
# TAREA: Lean los datos y asígnelos con el nombre "personas"


dim(personas) # ¿Qué nos arroja este comando?

# No obstante, queremos quedarnos solo con algunas variables:
## - Sexo.  
## - Edad.
## - Situación conyugal.
## - Nivel educativo que cursa o cursó.  
## - Ingresos.
## - Ocupación principal / Tareas.
## - Acceso a planes de ayuda económica o alimenticia.

# (b) Seleccionando variables con select

## - Si queremos seleccionar una variable ponemos su nombre.  
## - Si queremos sacar una variable ponemos su nombre con un  signo de - adelante.
## - Si queremos seleccionar (o sacar) un un rango seguido de  columnas, por ejemplo, 
## de la A a la F, lo hacemos con A:F, o bien -(A:F).
## - Si por ejemplo queremos quitar de la base todas las variables que comienzan con la
## letra h, la función starts_with() nos resuelve el problema (ends_with() y contains() 
## operan de forma análoga.)

# TAREA: seleccione con las variables con las que vamos a seguir trabajando. Emplee el diccionario.


# (c) Renombramos columnas con rename

# TAREA: Los nombres de las variables nos confunden, se los cambiamos con  la función rename() del paquete dplyr,
# siguiendo el siguiente criterio:

# - sit_conyugal = f2
# - parent_jefe = f10
# - edad = f11
# - sexo = f12  
# - dedicacion_p = g1 
# - tipo_p = g4 
# - tareas_p = g5  
# - jub_pen = h3 
# - transf_pais = h7
# - transf_ext = h8
# - afam = h9a
# - benef_esp = h10

# (d) Variable nper y niño de la muestra

# La variable nper etiqueta con un número a cada miembro del hogar.  No sabemos a priori qué valor 
# toma para los niños seleccionados en  la muestra.

# Sospechamos que cuando toma valor 1 se trata del niño de la  muestra.

# TAREA: Estos niños deberían tener todos menos de 20 años, por lo que  filtramos la base por edad, 
# agrupamos según nper y contamos la cantidad de casos para cada grupo.


# TAREA: nper == 1 contiene 2137 observaciones, lo cual coincide con la  cantidad de niños en la muestra.
# Pero nos gustaría ordenar esa tabla según conteo en forma  decreciente. Lo hacemos con la función arrange().

# (e) Jefes de hogar según sexo

# Tenemos información sobre los jefes de hogar para cada hogar  correspondiente a los niños de la muestra.
# TAREA: Nos interesa descubrir si hay una diferencia por sexo, es decir si hay  predominancia de hogares con 
# jefe de hogar hombre o mujer.
# Para ello:
# Filtramos la variable parent.jefe para los casos que toma  valor 1 que corresponde a los jefes de hogar.
# Agrupamos por sexo.
# Contamos cuántos casos hay en cada categoría (9=NS/NC).  Creamos una variable que indique el porcentaje.




# TAREA: ahora empleamos transmute(). La diferencia con mutate() es que me deja solo la variable que 
# generó a partir de la función transmute(), mientras que mutate() incorpora la nueva variable al final del data set.

# (f) Análisis Territorial

# Nos gustaría tener la variable departamento que indica el  departamento al cuál pertenece el hogar de la muestra, 
# a los efectos  de observar si el resultado anterior presenta disparidades a nivel  territorial.
# Tenemos esta información en la base de Hogares y una variable de  matcheo que es nform, la cual indica el número de formulario y
# vale lo mismo para todos los miembros del hogar.
# TAREA: Nos interesa mantener todas las observaciones de la base de  personas, y “pegarles” el departamento al cual corresponden, 
# por lo  cual usamos la función left_join.

# Podemos hacerlo en 2 pasos o en 1:
# Opción 1: Seleccionamos la columna que nos interesa y luego, unimos las bases.
# Opción 2: Anidamos funciones.

# Observación: Se iguala el nombre nuevo al nombre viejo (para hogares).




