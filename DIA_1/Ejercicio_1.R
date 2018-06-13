#Dia 1
#Ejercicios de practica.


#Borrar los datos de memoria
rm(list=ls())

#Instalar las librerias
install.packages("here")

#Cargar las librerias
library(here)


#***Ejercicio 1***

#Definir vectores x , y.
x <- c(1,3,4,5,7,9)
y <- c(2,3,5,7,11,13)

#Realizar las soguientes operaciones.

x + 1
y*2
length(x)
3 + sqrt(x)
sum(x)
sum(x > 5)
sum( x >5 | x < 3)
y[3]
y[-3]
y[x]
x + y[seq(1:length(x))]


#***Ejercicio 2***
#Cargar la base de datos

#2.1 Leer archivo 
datos_mora <- read.csv(paste0(here("DATOS"),"/","mora_toyset.csv"))

#2.2 Se accede a las variables con el simbolo $. Por ejemplo para acceder a la variable Id
datos_mora$Id

#2.2 Para conocer la clase de la variable, se utiliza la funcion class. Por ejemplo datos_mora es un data.frame
class(datos_mora)

#2.3 Para conocer cuantas variables. Se puede conocer usando la dimension del data frame. En este caso son 30 variables.
dim(datos_mora)

#2.3 Para convertir las variables a tipo cuantitativas. Se utiliza el tipo de la variable a factor y también se puede 
#cambiar las etiquetas con la funcion levels.
datos_mora$Nar <- as.factor(datos_mora$Nar)
levels(datos_mora$Nar) <- c("SI", "NO")

datos_mora$Cal <- as.factor(datos_mora$Cal)
levels(datos_mora$Nar) <- c("Caldas", "No_Caldas")


#2.4 Medidas estadisticas

#valor promedio
valor_promedio_yield <- mean(datos_mora$Yield)
#mediana
valor_mediana_yield <- median(datos_mora$Yield)
#desviacion estandar
valor_desviacionEstandar_yield <- sd(datos_mora$Yield)
#varianza
valor_varianza_yield <- var(datos_mora$Yield) 



#2.5 Aplicacion de la funcion summary
summary(datos_mora$Yield)
summary(datos_mora$PrecAcc_2)
summary(datos_mora$trmm_3)

#2.6 Graficas barras
boxplot(datos_mora$trmm_3, main="Box Plot de Preciptacion del tercer mes antes de cosecha", col= "blue")

















