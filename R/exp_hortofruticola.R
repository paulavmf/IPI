#exportacion productos hortofrutículas por toneladas al mes
#no hay datos desde julio del 2016 y en junio siempre tengo 0.... lo cual lo puedo poner como missing value y rellenarlo

setwd("~/Documentos/Rstudio/TFM")
datos<-read_xls('exp_hortofruticola.xls', skip = 8)

str(datos)
summary(datos)
tail(datos)
datos <- na.omit(datos, invert = TRUE)

datos$X__1<-stri_trim(datos$X__1 )
#convirtiéndolos en fechas
datos$X__1<-parse_date(datos$X__1,"%Y %B ",locale=locale("es")) #el primer datos no sale porque tiene una (p) al final... simplemente lo introduzco a mano
class(datos$X__1)
l<-str_remove(datos$`exp hortofruticola (ton)`,"\\.")
datos$`exp hortofruticola (ton)`<-str_replace(l,"\\,",".") #HAY QUE PONER DOS PUNTOS SLASHES!!!!!
datos$`exp hortofruticola (ton)`<-as.numeric(datos$`exp hortofruticola (ton)`)
result<-(xts(datos$`exp hortofruticola (ton)` , order.by = datos$X__1 ))
class(result)
str(result)
exp_hortofrut<-result
autoplot(exp_hortofrut)

#esta gráfica es muy rara... no sé si es así de verdad o qué..... 
