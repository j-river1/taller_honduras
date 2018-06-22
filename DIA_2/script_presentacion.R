#Ejemplo de Rmwagen
rm(list=ls())
install.packages("RMAWGEN")
install.packages("randomForest")
library(RMAWGEN)
library(randomForest)
library(MASS)
library(stats)
library(graphics)
library(ggplot2)
library(here)
set.seed(1222)


#Lectuta de datos del paquete 
data(trentino)

#Estaciones para ser llenados los valores faltantes
station <- c("T0090", "T0083")

#Nombres de las estaciones
names(ELEVATION) <- STATION_NAMES
names(LOCATION) <- STATION_NAMES
ELEVATION[station]

#Localizacion de las estaciones
LOCATION[station]

#Parametro para utilizar el paquete 

PREC_CLIMATE <- NULL
year_max <- 1990
year_min <- 1961
origin <- "1961-1-1"

#Simulacion periodo
#Calibración del periodo

n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5

#order test p
p_test <- 1
p_prec <- 3
exogen <- NULL
exogen_sim <- exogen

#Valores a predecir
summary(PRECIPITATION[station])



#Valores para la precipitacion
generationP03GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                              prec_all=PRECIPITATION,
                                                              year_min = year_min,
                                                              year_max = year_max,
                                                              p=p_prec,
                                                              n_GPCA_iteration= n_GPCA_iter,
                                                              n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                              exogen = exogen,
                                                              exogen_sim = exogen_sim,
                                                              sample = "monthly",
                                                              mean_climate_prec = PREC_CLIMATE,
                                                              no_spline = FALSE
                                                              )


qqplot(generationP03GPCA_prec$prec_mes$T0090, generationP03GPCA_prec$prec_gen$T0090, xlab = "Generado", ylab = "Observado", main="QQ plot del observado \n Precipitacion")


#Valores para la temperatura 


TN_CLIMATE <- NULL
TX_CLIMATE <- NULL
generationP01GPCA_temp <- ComprehensiveTemperatureGenerator(station= station,
                                                            Tx_all = TEMPERATURE_MAX,
                                                            Tn_all = TEMPERATURE_MIN,
                                                            year_min = year_min,
                                                            year_max = year_max,
                                                            p = p_test,
                                                            n_GPCA_iteration = n_GPCA_iter,
                                                            n_GPCA_iteration_residuals = n_GPCA_iteration_residuals,
                                                            sample="monthly",
                                                            mean_climate_Tn=TN_CLIMATE,
                                                            mean_climate_Tx=TX_CLIMATE,
                                                            ) 


qqplot(generationP01GPCA_temp$output$Tx_gen$T0090, generationP01GPCA_temp$input$data_original$T0090, xlab = "Generado", ylab = "Observado", main="QQ plot del observado \n Temperatura")

#Random Forest

attach(Boston)
set.seed(101)

#Esta base de datos esta compuesta por 506 filas y 14 columnas
#La descripcion de cada variable se encuentra
#http://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html

#Dimesion de la base de datos
dim(Boston)

#Training set 
train=sample(1:nrow(Boston),300)

#Random fores
Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)

#grafica
plot(Boston.rf, main ="Cantidad Optima de árboles")

#Grafica para descubrir la cantidad optima de las variables.
#OOB is la media de la prediccion de cada muetrs
#banging error. Es tomar  S arboles. Crear muestras aleatorias con reemplazamiento de tamanho. Cada una de esas 
#se llama data set. Bagging.

#este paso es para conocer el error de las variables de cada muestra

oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}


matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split", main ="Cantidad Óptima de variables")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#------------------
#Control de Calidad
#------------------

datos_tempera <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_TM_CD.txt"), header = T)
datos_tempera <- na.omit(datos_tempera )
datos_tempera$Date <- as.Date(as.character(datos_tempera$Date), format= "%Y-%m-%d")
datos_tempera_aux <- datos_tempera



ggplot(datos_tempera, aes(x=Date, y=Value)) + geom_point() + ggtitle("Temperatura Mínima") +theme(plot.title = element_text(hjust = 0.5)) +
ylab("Grados Centigrados")  


#Scores para varios tipos de distribucion
#Chi Squared
#Podemos cambiar la probabilidad
#




graficar_score<- function(tipo, proba )
{
  
  datos_temperatura <- datos_tempera
  prob = proba
  scores <- scores(datos_temperatura$Value, type= tipo, prob=prob )
    
  
  index_out <- which(scores == "TRUE")
  index_norm <- which(scores == "FALSE")
  datos_temperatura$Datos[index_out] <- "Outliers"
  datos_temperatura$Datos[index_norm] <- "Normales"
  
    
  if(tipo == "chisq")
  {
    name <- "Chi Squared"
  }

  if(tipo == "t")
  {
    name <- "t squared"
  }  
  
  if(tipo == "z")
  {
    name <- "z scored"
  }  
  
  
  
    ggplot(datos_temperatura, aes(x=Date, y = Value, color = Datos)) + geom_point() + ggtitle(paste("Temperatura Mínima ", name , " con probabilidad", prob)) +theme(plot.title = element_text(hjust = 0.5)) + scale_color_brewer(palette="Dark2")+
      ylab("Grados Centigrados")

  
}





#Cooks distance 
mod <- lm(Value ~ ., data=datos_tempera_aux)
cooksd <- cooks.distance(mod)
umbral <- 4*mean(cooksd, na.rm=T)  
index <- which(cooksd>umbral)
index <- as.numeric(names(table(index)))

datos_tempera_aux$Datos <- "Normal"
datos_tempera_aux$Datos[index] <- "Outliers"


ggplot(datos_tempera_aux, aes(x=Date, y= Value, color= Datos)) + geom_point() + ggtitle(paste("Grafica de Outliers Ánalsis Multivariado")) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_brewer(palette="Dark2")+
  ylab("Grados Centigrados")

#---------------------
#Analisis Multivariado
#---------------------


datos_estacion_todas <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa.txt"), header = T)
datos_estacion_todas <- na.omit(datos_estacion_todas)
datos_estacion_todas$Date <- as.Date(as.character(datos_estacion_todas$Date), format= "%Y-%m-%d")


#Cooks distance 
mod <- lm(TM ~ ., data=datos_estacion_todas)
cooksd <- cooks.distance(mod)
umbral <- 4*mean(cooksd, na.rm=T)  
index <- which(cooksd>umbral)
index <- as.numeric(names(table(index)))



datos_estacion_todas$Datos <- "Normal"
datos_estacion_todas$Datos[index] <- "Outliers"

ggplot(datos_estacion_todas, aes(x=Date, y= TM, color= Datos)) + geom_point() + ggtitle(paste("Grafica de Outliers Ánalsis Multivariado")) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_brewer(palette="Dark2")+
  ylab("Grados Centigrados")











