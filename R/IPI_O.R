library(readr)
library(ggplot2)
library(stringi)
library(anytime)
library(astsa)
library(xts)
library(readxl)
library(stringr)
library(dplyr)
library(fpp2)
library(tsbox)
library("MASS")


#creyendo que los datos proporcionados por el ISTAC eran datos del IPI corregidos a efectos de calendario etc... bajé los que se supone originales del INE.
#al final estos resultaron ser los mismos... Por lo que supongo que los datos que se encuentran en el ISTAC no son los corregidos
#setwd("~/Documentos/Rstudio/TFM/IPI_base(2015)")
datos<-read_xlsx('IPI_base(2015)/IPI_ORGINAL.xlsx', na = "", sheet = 2)
datosV<-read_xlsx('IPI_base(2015)/IPI_ORGINAL.xlsx', na = "",sheet = 3)
str(datos)
str(datosV)
summary(datos)
tail(datos)
datos <- na.omit(datos, invert = TRUE)


#parse dates

datos$X__1


library("lubridate")
#convierto las fechas a formate
datos$X__1<- parse_date(datos$X__1,"%YM%m")
class(datos$X__1)
head(datos)

#xonvierto a formato xts
datosxts<-(xts(datos[,-1], order.by = datos$X__1))
head(datosxts)
datosxts
head(datosxts)
IPI<-datosxts

#write.csv(as.data.frame(IPI), "IPI.csv", row.names = TRUE)


