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
#valor maximo
valor_maximo <- max(datos_mora$Yield)
#valor minimo
valor_minimo <- min(datos_mora$Yield)
#varianza
valor_varianza_yield <- var(datos_mora$Yield) 
#desviacion estandar 
valor_desviEsta_yield <- sd(datos_mora$Yield) 
#mediana
valor_mediana_yield <- median(datos_mora$Yield)
#desviacion estandar
valor_desviacionEstandar_yield <- sd(datos_mora$Yield)
#varianza
valor_varianza_yield <- var(datos_mora$Yield) 
#histograma
histo_yield <- hist(datos_mora$Yield, main ="Histograma del rendimiento", col= "yellow")


#2.5 Aplicacion de la funcion summary
summary(datos_mora$Yield)
summary(datos_mora$PrecAcc_2)
summary(datos_mora$trmm_3)


#3. Multiple linear regreession 

fit <- lm (Yield ~ AB_Thorn + intDrain + slope, data = datos_mora)
summary(fit)

#3.1 Coeficientes del modelo

#Pr valores mas pequenhos que 0.05 significa que la variable 
#incide en el modelo. En este caso, Slope se no incide en cambio
#slope si incide.

coefficients(fit)
#R squared multiplo Multiple R-squared es 0.3998
#https://data.library.virginia.edu/diagnostic-plots/
#Residuos Non-linear relationship
#Normal si los residuos son normalmente distribuidos
#Scale location si los residuos estan distribuidos
#igual que los predictores, son iguales si se ve una linea
#recta
#Los casos mas influyentes en el analisis.


plot(fit)












