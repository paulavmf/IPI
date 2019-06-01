#setwd("~/Documentos/Rstudio/TFM/empleo")

datos<-read_xls('empleo/pob_ocupada.xls')
#comprobando


datos$`TOTAL POBLACIÓN`<-gsub(",", ".",datos$`TOTAL POBLACIÓN`)
datos$`TOTAL POBLACIÓN`<- as.numeric(datos$`TOTAL POBLACIÓN`)

datos$X__1<- gsub(" Primer trimestre", "M01", datos$X__1)
datos$X__1<- gsub(" Segundo trimestre", "M04", datos$X__1)
datos$X__1<- gsub(" Tercer trimestre", "M07", datos$X__1)
datos$X__1<- gsub(" Cuarto trimestre", "M10", datos$X__1)

datos$X__1<- parse_date(datos$X__1,"%YM%m")
ocupacion<-(xts(datos[,-1], order.by = datos$X__1))

#ver si el método de esta función se puede mejorar... por ahora el resultado es bastante bueno
tmp<- approx(index(ocupacion),coredata(ocupacion), xout = index(IPI$`Índice general`), 
               rule = 2, method = "linear", ties = mean)
ocupacion_aprox<-(xts(tmp$y, order.by = tmp$x))
remove(tmp)
names(ocupacion_aprox)<-paste("empleo/población ocupada")
