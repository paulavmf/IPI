#**pendiente poner label!!!!


# install.packages("ggplot2")
# install.packages("anytime")
# install.packages("lubridate")
# install.packages("astsa")
install.packages("readxl")
install.packages("dplyr")
install.packages("fpp2")
install.packages("fma")
install.packages("tsbox")


library(readr)
library(ggplot2)
library(stringi)
library(readr)
library(anytime)
library(astsa)
library(xts)

setwd("~/Documentos/Rstudio/TFM")
#quizá me interesa tener los meses cOmo columnas
datos<-read.csv2('IPI_GEN.csv',header = FALSE, stringsAsFactors = FALSE)

#comprobando datos
str(datos)
summary(datos)
datos$V1
str(datos$V2) 

#limpiado datos
stri_trim(a) #quito los espacios en blanco del principio
datos$V1<-stri_trim(datos$V1)
datos$V1

#convirtiéndolos en fechas
datos$V1<-parse_date(datos$V1,"%Y %B ",locale=locale("es")) #el primer datos no sale porque tiene una (p) al final... simplemente lo introduzco a mano
datos$V1[1]<-as.Date("2018-07-01")
datos$V1 #ya tengo mis fechas como las quiero
str(datos$V1)
str(datos)


#*********QUIZÁ DEBERÍA REPETIRLO TODO USANDO UN OBJETO TS???*****************
#xts es una matriz en la que el índice son fechas
#Lo convierto en un objeto xts **No sé si es mejor convertirlo en un objeto ts

datosT = (xts(datos[,-1], order.by = datos[,1]))
datosT #yeah
#comprobando datosT cpmo time serie (xts)
plot(datosT)
time(datosT)
str(datosT)
#funciones propias de xts
periodicity(datosT) #me salen un motón de mensajes de warning
nmonths(datosT) #no me sale warnings con esto
nyears(datosT)
datosT["2017-01-01"]
plot.xts(datosT) #funviona también el plot normal y el ts.plot

#EMPIEZO A TRABAJARLOS:
#quito la tendencia lineal
plot(diff(datosT))
plot(diff(datosT,s = 12))# s hace referencia al periodo... cambiándolo parece que nada cambia...
#diff(datosT, lag = s) lo utilizaría si mis datos son estacionales ( lo que parece que no es el caso)
#es una serie espacionaria??? -> No parece.....
#Ploteo conjunto de las datos con o sin tendencia
par(mfrow = c(2,1))
plot(datosT)
plot(diff(datosT))
#Stationary Chek Test
par(mfrow = c(2,1))
acf(datosT) #no sé qué significa el resultado
pacf(datosT)  #no sé qué significa el resultado
Box.test(datosT, lag = 1, type ="Ljung-Box")
#me devuelve un p-value= 2.2e-12 se puede decir que LA SERIE ES ESTACIONARIA
#**no sé muy bien que es lag pero al parecer no afecta




#Puedo modelarlo con un ARIMA, toda serie estacionaria puede ser escrita como una combinación de WHITE NOISEruido blanco
#RUIDO BLANCO-> SEÑAL ALEATORIA-> sus valores en dos tiempos diferentes no guardan correlación estadística-> PROCESO ESTOCÁSTICO

#Simulo mi serie mediante un arima (tengo que simular mis datos sin tendencia)
model<-arima(diff(datosT),order = c(0,0,0))
model
##.................
#comparo el resultado de mi arima(media y varianza), con el de mis datos
DdatosT<-diff(datosT)
mean(DdatosT) #xq no le gusta???? quizá porque no es un objeto ts si no xts???
var(diff(datosT)) #NO LE GUSTA
#REAL DIFERENCIA ENTRE RANDOM WALK Y WHITE NOISE??????
#tODAY = yESTERDAY +nOISE(ZERO WHITE NOISE)
##...........
#MODELO RANDOM WALK

#ahora a mi modelo creado le añado la linealidad

int_model<-model$coef
ts.plot(datosT)
abline(0,int_model)

#algo falla en mi mierda***** ver si es porque estoy jugando con xts en vez de ts
int_mode