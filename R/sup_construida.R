#setwd("~/Documentos/Rstudio/TFM")
datos<-read_xls('lic_obras.xls', skip = 5)
str(datos)
summary(datos)
head(datos)
tail(datos)
datos <- na.omit(datos, invert = TRUE)

datos$X__1<-stri_trim(datos$X__1 )
#convirtiéndolos en fechas
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es")) #el primer datos no sale porque tiene una (p) al final... simplemente lo introduzco a mano
class(datos$X__1)
datos$`Superficie a construir según destinos por comunidades autónomas y periodos.(M m²)`<-as.numeric(datos$`Superficie a construir según destinos por comunidades autónomas y periodos.(M m²)`)
result<-(xts(datos$`Superficie a construir según destinos por comunidades autónomas y periodos.(M m²)`, order.by = datos$X__1 ))
class(result)
str(result)
sup_construida<-result
sup_construida<-`colnames<-`(sup_construida,"Superficie a construir(m2)")

autoplot(sup_construida)
head(sup_construida)

