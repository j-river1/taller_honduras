#Script Ejercicio practico 

rm(list=ls())

#Ejercicio 1
#Lectura de los siguientes datos:

datos_TM <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_TM_CD.txt"), header = T)
datos_TX <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_TX_CD.txt"), header = T)
datos_SR <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_SR_WAM2.txt"), header = T)
datos_P <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_P_MM.txt"), header = T)
datos_RH <- read.table(paste0(here("DATOS"),"/ChiapasAgroipsa_RH_NE.txt"), header = T)

#1. Formato a los archivos 

datos_TM$Date <- as.Date(as.character(datos_TM$Date), format= "%Y-%m-%d")
datos_TX$Date <- as.Date(as.character(datos_TX$Date), format= "%Y-%m-%d")
datos_SR$Date <- as.Date(as.character(datos_SR$Date), format= "%Y-%m-%d")
datos_P$Date <- as.Date(as.character(datos_P$Date), format= "%Y-%m-%d")
datos_RH$Date <- as.Date(as.character(datos_RH$Date), format= "%Y-%m-%d")

#2. Valores superiores e inferiores para cada variable.

#Temperatura Maxima
#Supongamos que la temperatura máxima está entre los 25 y 40 grados

index_TX <- which(datos_TX$Value > 25 & datos_TX$Value < 40)
datos_TX[index_TM] <- NULL

#Temperatura Minima 
#Supongamos que la temperaturas mínima está entre los 25 y 40 grados

index_TM <- which(datos_TM$Value > 25 & datos_TM$Value < 35)
datos_TM[index_TM] <- NULL

#Radiacion solar 
#Radiacion solar mayoes a 1080 WAM2

index_SR <- which(datos_SR$Value > 1080 & datos_SR$Value < 0)
datos_SR[index_SR] <- NULL


#Precipitacion
#Supongamos que esa región que sea mayor a 150
index_P <- which(datos_P$Value > 150 & datos_P$Value < 0)
datos_P[index_P] <- NULL


#Datos Humedad Relativa
index_RH <- which(datos_RH$Value > 100 & datos_RH$Value < 0)
datos_RH[index_RH] <- NULL








