
#Recopilación de estadísticas de productos derivados del petróleo 




#setwd("~/Documentos/Rstudio/TFM/")

############CONSUMO DEL FUELOLEO
remove(datos)
datos<-read_xls('Recopilación de estadísticas de productos derivados del petróleo/consumo_fueloleo.xls',skip = 7)

#CLEANING
#Pasando a números


head(datos)

datos$FUELÓLEO
class(datos$FUELÓLEO)
tail(datos, n=10)

l<-str_remove(as.character(datos[,2]),"\\.")
l<-str_remove(datos$FUELÓLEO,"\\.")
datos$FUELÓLEO<-str_replace(l,"\\,",".") #HAY QUE PONER DOS PUNTOS SLASHES!!!!!

head(datos)
datos$FUELÓLEO<-as.numeric(datos$FUELÓLEO)
head(datos)

#cleaning dates
#quito espacios en blanco en las fechas
datos$X__1<-stri_trim(datos$X__1)
head(datos$X__1)
#paso las fechas datos español a fechas datos ingles datostedibles por R
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es"))#el primer datos no sale porque tidatose una (p) al final... simplemdatoste lo introduzco a mano
class(datos$X__1) 
head(datos$X__1)


#tabla xts

#lo convierto datos una xts
fueoleo<-(xts(datos[,2], order.by = datos$X__1))
head(fueoleo)
nrow(fueoleo)
#Recopilación de estadísticas de productos derivados del petróleo 

#################CONSUMO DEL GASOLEO
remove(datos)
datos<-read_xls('Recopilación de estadísticas de productos derivados del petróleo/consumo_Gasoleo.xls',skip = 7)

#CLEANING
#Pasando a números


head(datos)
datos<-datos[-c(1),]
class(datos$GASÓLEO)
tail(datos, n=10)

l<-str_remove(datos$GASÓLEO,"\\.")
datos$GASÓLEO<-str_replace(l,"\\,",".") #HAY QUE PONER DOS PUNTOS SLASHES!!!!!

head(datos)
datos$GASÓLEO<-as.numeric(datos$GASÓLEO)
head(datos)

#cleaning dates
#quito espacios en blanco en las fechas
datos$X__1<-stri_trim(datos$X__1)
head(datos$X__1)
#paso las fechas datos español a fechas datos ingles datostedibles por R
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es"))#el primer datos no sale porque tidatose una (p) al final... simplemdatoste lo introduzco a mano
class(datos$X__1) 
head(datos$X__1)


#tabla xts

#lo convierto datos una xts
GASOLEO<-(xts(datos[,2], order.by = datos$X__1))
head(GASOLEO)
nrow(GASOLEO)


##########################CONSUMO GASOLINA

remove(datos)
datos<-read_xls('Recopilación de estadísticas de productos derivados del petróleo/consumo_Gasolina.xls',skip = 7)

#CLEANING
#Pasando a números


head(datos)
class(datos$GASOLINA)
tail(datos, n=10)

l<-str_remove(datos$GASOLINA,"\\.")
datos$GASOLINA<-str_replace(l,"\\,",".") #HAY QUE PONER DOS PUNTOS SLASHES!!!!!

head(datos)
datos$GASOLINA<-as.numeric(datos$GASOLINA)
head(datos)

#cleaning dates
#quito espacios en blanco en las fechas
datos$X__1<-stri_trim(datos$X__1)
head(datos$X__1)
#paso las fechas datos español a fechas datos ingles datostedibles por R
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es"))#el primer datos no sale porque tidatose una (p) al final... simplemdatoste lo introduzco a mano
class(datos$X__1) 
head(datos$X__1)


#tabla xts

#lo convierto datos una xts
gasolina<-(xts(datos[,2], order.by = datos$X__1))
head(gasolina)
nrow(gasolina)


#creo una sola tabla con todo
consumo_gas<-cbind(fueoleo,GASOLEO,gasolina)
head(consumo_gas)


#summary y plot
summary(consumo_gas)
plot.zoo(x = consumo_gas[ ,c(names(consumo_gas))])



###################################################Series mensuales del precio del petróleo. 1986 Enero - 2018 Septiembre

remove(datos)
datos<-read_xls('Recopilación de estadísticas de productos derivados del petróleo/precio_petroleo_1968_2018.xls',skip = 7)

#CLEANING
#Pasando a números


head(datos)
class(datos$GASOLINA)
tail(datos, n=10)

l<-str_remove(datos$`Precio del petróleo (barril Brent)`,"\\.")
datos$`Precio del petróleo (barril Brent)`<-str_replace(l,"\\,",".") #HAY QUE PONER DOS PUNTOS SLASHES!!!!!

head(datos)
datos$`Precio del petróleo (barril Brent)`<-as.numeric(datos$`Precio del petróleo (barril Brent)`)
head(datos)

#cleaning dates
#quito espacios en blanco en las fechas
datos$X__1<-stri_trim(datos$X__1)
head(datos$X__1)
#paso las fechas datos español a fechas datos ingles datostedibles por R
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es"))#el primer datos no sale porque tidatose una (p) al final... simplemdatoste lo introduzco a mano
class(datos$X__1) 
head(datos$X__1)


#tabla xts

#lo convierto datos una xts
precio_gas<-(xts(datos[,2], order.by = datos$X__1))
head(precio_gas)
nrow(precio_gas)
summary(precio_gas)
plot.xts(precio_gas)#datos en dólares



