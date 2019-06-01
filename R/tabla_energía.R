#####################################ENERGÍA#####################################
#Producción bruta, disponibilidad y consumo de energía eléctrica por islas de Canarias y periodos.

#setwd("~/Documentos/Rstudio/TFM/Producción_disponibilidad_consumo_energía_eléctricas")
EN<-read_xls('Producción_disponibilidad_consumo_energía_eléctricas/ENERGIA.xls',skip = 7, na = "..", n_max = 333)
#comprobando

head(EN)
str(EN)
tail(EN)
summary(EN)

#limpiando
EN$X__1<-stri_trim(EN$X__1)


#Elimino la primero línea que pone canarias
EN<-EN[-c(1),]
head(EN, n = 10)

#paso las fechas en español a fechas en ingles entedibles por R
EN$X__1<-parse_date(EN$X__1,"%Y %B ",locale=locale("es"))#el primer EN no sale porque tiene una (p) al final... simplemente lo introduzco a mano
class(EN$X__1) 

#convierto las cadenas a números

EN[,2:4]<-lapply(EN[,2:4], as.numeric)
head(EN,3)
#lo convierto en una xts
EN<-(xts(EN[,2:4], order.by = EN$X__1))
class(EN)
head(EN)
tail(EN)
summary(EN)

plot.zoo(x = EN[ ,c(names(EN))])
EN_diff<-diff(EN) #PORQUÉ No funciona??
plot.zoo(x = EN_diff[ ,c(names(EN))])


