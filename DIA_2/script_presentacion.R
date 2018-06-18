#Ejemplo de Rmwagen
rm(list=ls())
install.packages("RMAWGEN")
library(RMAWGEN)
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


qqplot(generationP03GPCA_prec$prec_gen$T0090, generationP03GPCA_prec$prec_gen$T0090, xlab = "Generado", ylab = "Observado", main="QQ plot del observado \n Precipitacion")


#Valores para la temperatura 

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





