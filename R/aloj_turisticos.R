#número de alojamientos turísticos abiertos al mes en canarias
#setwd("~/Documentos/Rstudio/TFM")
datos<-read_xls('aloj_tur_abiertos.xls')

str(datos)
summary(datos)
tail(datos)
datos <- na.omit(datos, invert = TRUE)

datos$X__1<-stri_trim(datos$X__1 )
#convirtiéndolos en fechas
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es")) #el primer datos no sale porque tiene una (p) al final... simplemente lo introduzco a mano
class(datos$X__1)
result<-(xts(datos$`Alojamientos turísticos abiertos`, order.by = datos$X__1 ))
class(result)
turismo<-result
turismo<-`colnames<-`(turismo,"Alojamientos turísticos abiertos")
autoplot(turismo)
head(turismo)
