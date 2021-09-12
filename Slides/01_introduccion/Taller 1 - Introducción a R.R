########################################################
############### Taller 1 - Econometría I ###############
################### Introducción a R ###################
########################################################

# R como calculadora

9+12

# Código incompleto

objeto_1 <- seq(1, 30, 0.5)
# plot(objeto_1

# Instalar paquetes     
install.packages("dplyr")

# Buscando ayuda
help(lm)
?lm
help("+")

# Uso de funciones

mean(x = 1:10)
mean(1:10)
mean(x = c(1:10, NA), trim = 0, na.rm = TRUE)
mean(trim = 0, x = c(1:10, NA), na.rm = TRUE)
mean(c(1:10, NA), 0, TRUE)

# Objetos

mejor_no = c(1, 2, 3)
asi_si <- c(4:6)

## Tipos de objetos

logico <- c(TRUE, FALSE, TRUE) 
double <- seq(from = 1, to = 2, 0.3)
character <- letters[1:5]

# Estructuras de datos

## Vectores

### Un vector es un arreglo de una dimensión. En R existen tres clases principales de vectores y 
### se crean con la función combine c():
  

### (a) Numérico
#### Cree un vector numérico con los valores -1, 2.5, 3, 4, 5.1 y llámelo "num_vec"


### (b) Character
#### Cree un vector de tipo character con los valores Mon, Tue, Wed, Thu, Sat, Sun y llámelo 
#### char_vec

### (c) Lógico
#### Cree un vector de tipo lógico con los valores VERDADERO y FALSO y llámelo 
#### log_vec

### La función class() nos dice cuál es la clase o tipo del vector.

#### Pruébela con los vectores recién creados

### Otra función importante es length() que nos dice cuál es la longitud del vector.

#### Pruébela con los vectores recién creados

### Ejemplo: Ganancias - Ruleta y póker. Mis ganancias de póker por día de la semana son:
  
poker_gan <- c(150, 178, -6, 166, -80, -119, -142)
poker_gan
  
### Nombres de vectores. La función names() nos permite nombrar los elementos de cada vector.

### Por ejemplo, a cada elemento de las ganancias de poker del ejercicio anterior, asignaremos 
### el nombre del día de la semana en que se obtuvieron.

dias <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

#### Póngale nombres (dias) al vector poker_gan
  
### Selección de elementos en vectores. La selección de elementos de un vector se realiza 
### indicando las posiciones a seleccionar entre [ ]. 

### Estas posiciones pueden indicarse por medio de un vector numérico o de caracteres si los 
### elementos del vector están nombrados.

### (a) Vector numérico:
  
poker_gan[ c(1, 5) ]
poker_gan[ 1:3 ]

#### Nombres:
  
poker_gan[ c("Mon", "Tue")]

### Operaciones con vectores

ruleta_gan <- c(-48, 151, 198, -16, 134, -153, 126)
dias <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
names(ruleta_gan) <- dias
poker_gan + ruleta_gan

### En R para cualquier operación (+, -, *, /) de vectores, las operaciones son elemento a elemento.
### Por ejemplo, al sumar vectores:
  
###la primera posición del primer vector se suma con la primera posición del segundo vector,

###la segunda posición del primer vector se suma con la segunda posición del segundo vector

### y así sucesivamente.

## Matrices

### Una matriz es un arreglo de dos dimensiones en el que todos los elementos son 
### del mismo tipo, por ejemplo: numéricos.

### La función matrix() permite crear la matriz de un vector especificando las dimensiones, 
### por ejemplo:
  
matrix(data = 1:9, nrow = 3, ncol = 3)

### En el siguiente vector se presentan los ingresos totales y de lanzamiento de cada película de 
### la saga Harry Potter.
  
### Ejemplo: Box Office Mojo: Harry Potter
  
sales_hp <- c(497066400, 426630300, 401608200, 399302200, 377314200, 
              359788300, 357233500, 328833900, 141823200, 189432500, 
              142414700, 135197600, 99635700, 92756000, 134119300, 
              138752100)

sales_mat <- matrix(sales_hp, nrow = 8)
sales_mat
  
### La función dim() regresa la dimensión de la matriz (renglones y columnas).



### La función nrow() regresa el número de renglones de la matriz y ncol() el número de columnas.

#### ¿Qué nos dice al usarlas con sales_mat?

### Nombres de matrices. En R es posible agregar nombres a los renglones y columnas de una 
### matriz con las funciones colnames() y rownames(). Considerando los siete títulos de la saga,
### asignamos los títulos de las películas a los renglones con la función rownames():
  
titles_hp <- c(
  "1. HP and the Sorcerer's Stone",
  "8. HP and the Deathly Hallows Part 2",
  "4. HP and the Goblet of Fire",
  "2. HP and the Chamber of Secrets",
  "5. HP and the Order of the Phoenix",
  "6. HP and the Half-Blood Prince",
  "3. HP and the Prisoner of Azkaban",
  "7. HP and the Deathly Hallows Part 1")

#### Asígnele nombres a las filas

sales_hp <- c("total", "release_date")

#### Asígnele nombres a las columnas con sales_hp

sales_mat

### Selección de elementos en una matriz. Al igual que un vector, los elementos de una matriz 
### pueden seleccionarse con un vector de posiciones o un vector de nombres. Pero, en este se 
### define la posición de ambas dimensiones, filas y columnas [ , ].

### Por ejemplo, si queremos obtener una submatriz para las primeras tres películas de 
### las ventas:
  
sales_mat[c(1, 4, 7), 1:2]
  
### Operaciones en matrices. Al igual que los vectores, las operaciones son elemento a elemento. 
### Siguiendo con el ejemplo de ingresos, para facilitar la lectura de los datos dividimos entre 
### un millón cada valor.

sales_mat_mill <- sales_mat/1000000
sales_mat_mill

### Lo mismo sucede con un vector. Supongamos que el siguiente vector contiene el número de 
### cines en los que se exhibió cada película.

theaters_vec <- c(3672, 4375, 3858, 3682, 4285, 4325, 3855, 4125)
theaters_vec

### Calculemos el ingreso promedio por cada cine para el total de ingresos y en la fecha de 
### lanzamiento.

sales_mat_avg <- sales_mat/theaters_vec
sales_mat_avg

## Dataframe

### Un dataframe es un objeto de dos dimensiones en R. Puede verse como un arreglo de vectores de 
### la misma dimensión, similar a una matriz.

### La ventaja de un dataframe, es que a diferencia de una matriz, los vectores o columnas pueden 
### ser de diferentes tipos.

### Puedo crear dataframes con la función data.frame().
### Una forma de crear un dataframe es asignando vectores.

muestra_df <- data.frame(secuencia = 1:5,
                         aleatorio = rnorm(5),
                         letras = c("a", "b", "c", "d", "e"))
muestra_df
  
### O bien, se pude transformar una matriz con la misma función. Tomemos los datos de los ingresos 
### de las películas de la saga de HP y hagamos una matriz.

sales_df <- data.frame(sales_mat)
sales_df
  
### Nombres de dimensiones. Al igual que matrices, las funciones rownames() y colnames() permiten 
### nombrar los renglones y columnas del objeto.

colnames(sales_df) <- c("total_grosses", "opening_grosses")
sales_df
  
### Selección de elementos. 
### Para dataframes, además de seleccionar posiciones de renglones y columnas con [ , ], 
### se puede usar el signo $.

sales_df$total_grosses

### Funciones útiles para data frames. Existen algunas que ayudan a tratar dataframes.

### (i) head()
head(sales_df)

### (ii) str()
str(sales_df)

### (iii)  dim(), nrow() y ncol()
dim(sales_df)
nrow(sales_df)
ncol(sales_df)

