

xts_gen2<-function(datos){
  
  ##LIMPIANDO
  #FECHAS
  #quito espacios en blanco en las fechas
  datos$X__1<-stri_trim(datos$X__1)
  #paso las fechas datos español a fechas datos ingles datostedibles por R
  datos$X__1<-parse_date(datos$X__1,"%Y",locale=locale("es"))#el primer datos no sale porque tidatose una (p) al final... simplemdatoste lo introduzco a mano
  class(datos$X__1) 
  datos$X__1
  
  #NÚMEROS(LOS DATOS)
  #limpio lo números porque muchos son caracteres:
  #1. paso el core de los datos a data frame
  df<-data.frame(datos[,2:length(datos)])
  #Buvle que busca las columnas que son caracteres y las pasada a data frame
  for (n in c(1:length(df))) {
    if (is.numeric(df[,n])== FALSE){
      df[,n] = as.numeric(df[,n])}
  }
  str(df) #ahora todo el mundo es número
  
  #lo convierto datos una xts
  datosxts<-(xts(df, order.by = datos$X__1))
  #lo convierto datos una xts
  
  #Arreglo el nombre de las coumnas (habían cambiado algunos caracteres)
  datosxts<-`colnames<-`(datosxts,(c(names(datos[,2:length(datos)]))))
  return(datosxts)
  
  
}



# datoscuesta Industrial Anual de Productos (EIAP)

## Producción por agrupaciones de actividad (CNAE-2009)

#Serie 2008-2017 (miles de euros) (datos anuales)!
remove(datos)
setwd("~/Documentos/Rstudio/TFM/EIAP")
datos<-read_xls('2008_2017.xls',skip = 7, na = "....")
#comprobando
head(datos)
str(datos)
tail(datos)
summary(datos)


EIAP_2008<-xts_gen2(datos)
head(EIAP_2008)

#summary y plot
summary(EIAP_2008)
plot.zoo(x = EIAP_2008[ ,c(names(datosxts))])


# datos_diff<-diff(datos) #PORQUÉ No funciona??
# plot.zoo(x = datos_diff[ ,c(names(datos))])



##Producción por agrupaciones de actividad (CNAE-93)
#Serie 2001-2007 (millones de pesetas).
setwd("~/Documentos/Rstudio/TFM/EIAP")
datos<-read_xls('2001_2007.xls',skip = 7)
head(datos)
str(datos)
tail(datos)
summary(datos)
EIAP_2001<-xts_gen2(datos)
head(EIAP_2001)
summary(EIAP_2001)

#summary y plot
summary(EIAP_2001)
plot.zoo(x = EIAP_2001[ ,c(names(EIAP_2001))])



##Producción por agrupaciones de actividad (CNAE-93)
#Serie 1993-2000 (millones de pesetas).
remove(datos)
setwd("~/Documentos/Rstudio/TFM/EIAP")
datos<-read_xls('1993_2000.xls',skip = 7)
head(datos)
str(datos)
tail(datos)
summary(datos)


EIAP_1993<-xts_gen2(datos)
head(EIAP_1993)

#summary y plot
summary(EIAP_1993)
plot.zoo(x = EIAP_1993[ ,c(names(EIAP_1993))])
help(merge)

ncol(EIAP_1993)
names(EIAP_1993)
ncol(EIAP_2001)
names(EIAP_2001)
ncol(EIAP_2008)
names(EIAP_1993)

rbind.xts(index(EIAP_1993),index(EIAP_2001),index(EIAP_2008))
names(EIAP_1993)==names(EIAP_2001)
