#C H U L E T A 
setwd("~/Documentos/Rstudio/TFM")
datos<-read.csv2('IPI_GEN.csv',header = FALSE, stringsAsFactors = FALSE)
ncol(datos) #me da el número de columnas de mi data frame
nrow(datos) #me da el número de filas de mi data frame
####
x<-numeric(length(datos)) #creo un VECTOR de tipo float, vacío de tantas olumnas como columnas tiene datos
str(x)
x2<-numeric(nrow(datos))
str(x2) #tcrea un VECTOR  como tfilas tiene datos
help(na.omit)

#pipe function (%>%) para funciones anidadas
#cómo se usa?
#function(foo)       # These two
#foo %>% function()  # are the same!
#foo %>% function    # Inconsistent