library(readr)
library(ggplot2)
library(stringi)
library(readr)
library(anytime)
library(astsa)
library(xts)
library(readxl)
library(stringr)
library(dplyr)
library(fpp2)
library(tsbox)

setwd("~/Documentos/Rstudio/TFM/IPI_base(2015)")
#BC<-read.csv2('bienes_consumo.csv',header = FALSE, stringsAsFactors = FALSE)
list.files(pattern=".csv$")
list.filenames<-list.files(pattern=".csv$")
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()

# create a loop to read in your data
for (i in 1:length(list.filenames))
{
  list.data[[i]]<-read.csv2(list.filenames[i], header = FALSE, stringsAsFactors = FALSE)
}

# add the names of your data to the list
#les quito la terminación "csv" antes de seguir
list.filenames<-gsub(".csv","",list.filenames)
names(list.data)<-list.filenames


#función que hace el limpia y con las fechas y me convierte cada fichero en un objeto xts acumulados en una lista
xts_gen <- function(datos){
  #limpiado datos
  #quito los espacios en blanco del principio
  datos$V1<-stri_trim(datos$V1)
  #convirtiéndolos en fechas
  datos$V1<-parse_date(datos$V1,"%Y %B ",locale=locale("es")) #el primer datos no sale porque tiene una (p) al final... simplemente lo introduzco a mano
  datos$V1[1]<-as.Date("2018-07-01")
  datos$V1 #ya tengo mis fechas como las quiero
  class(datos)
  #*********QUIZÁ DEBERÍA REPETIRLO TODO USANDO UN OBJETO TS???*****************
  #xts es una matriz en la que el índice son fechas
  #Lo convierto en un objeto xts **No sé si es mejor convertirlo en un objeto ts

  result<-(xts(datos[,-1], order.by = datos[,1]))

 
  return(result)
}

#esta función me hace lo mismo que un for para los elementos de una lista MUY CREMA
lapply(list.data, head) #compruebo mis tablas
#creoo una nueva lista con las nuevas tablas xts
list.data_xts<-list()

#le aplico la función a toda la lista
list.data_xts<-lapply(list.data, xts_gen) 


#compruebo que todo está bien
lapply(list.data_xts,class)

lapply(list.data_xts,periodicity)
lapply(list.data_xts,nmonths)
lapply(list.data_xts,head)
lapply(list.data_xts, tail)
lapply(list.data_xts, summary)

#creo la xts con todo
i= 1
IPI<- merge.xts(list.data_xts[[i]],list.data_xts[[i+1]]) 
for (i in 3:length(list.data_xts)){
IPI<- merge.xts(IPI,list.data_xts[[i]]) 
}




#comprobaciones sobre la nueva xts que acabo de crear con todos los IPI
class(IPI)
head(IPI)
names(IPI)
coredata(IPI)
index(IPI)

#Cambio el nombre de las columnas al original porque merge hace lo que le da la ganacon eso
IPI<-`colnames<-`(IPI,(c(list.filenames)))
head(IPI)


plot.zoo(x = IPI[ ,c(list.filenames)])
IPI_diff<-diff(IPI)
#diff sirve para hacer cualquier time series estacionaria
plot.zoo(x = IPI_diff[ ,c(list.filenames)])


#otro tipo de ploteo
autoplot(IPI, facets =  FALSE)
#no sé que es facets en realidad

#plotear mi serie por estaciones (útil si mi serie es estacional)
ggseasonplot(as.ts(IPI$general)) #SOLO PARA TS.. NO SIRVE PARA XTS... y
#con coordenadas polares



#***********************LO MISMO PARA EL IPRI***********************

setwd("~/Documentos/Rstudio/TFM/IPRI_base(2015)")

# create a loop to read in your data
for (i in 1:length(list.filenames))
{
  list.data[[i]]<-read.csv2(list.filenames[i], header = FALSE, stringsAsFactors = FALSE)
}

# add the names of your data to the list
#les quito la terminación "csv" antes de seguir
list.filenames<-gsub(".csv","",list.filenames)
names(list.data)<-list.filenames



#esta función me hace lo mismo que un for para los elementos de una lista MUY CREMA
lapply(list.data, head) #compruebo mis tablas
#creoo una nueva lista con las nuevas tablas xts
list.data_xts<-list()

#le aplico la función a toda la lista
list.data_xts<-lapply(list.data, xts_gen) 


#compruebo que todo está bien
lapply(list.data_xts,class)

lapply(list.data_xts,periodicity)
lapply(list.data_xts,nmonths)
lapply(list.data_xts,head)
lapply(list.data_xts, tail)
lapply(list.data_xts, summary)

#creo la xts con todo
i= 1
IPRI<- merge.xts(list.data_xts[[i]],list.data_xts[[i+1]],join = "right") 
for (i in 3:length(list.data_xts)){
  IPRI<- merge.xts(IPRI,list.data_xts[[i]],join = "right") 
}


#Cambio el nombre de las columnas al original porque merge hace lo que le da la ganacon eso
IPRI<-`colnames<-`(IPRI,(c(list.filenames)))
head(IPRI)


#comprobaciones sobre la nueva xts que acabo de crear con todos los IPI
class(IPRI)
head(IPRI)
names(IPRI)
coredata(IRPI)
index(IPRI)



plot.zoo(x = IPRI[ ,c(list.filenames)])
IPRI_diff<-diff(IPRI)
plot.zoo(x = IPRI_diff[ ,c(list.filenames)])



