#ESTUDIO PREVIO


setwd("~/Documentos/Rstudio/TFM")
source("plots_IM.R")
#STUDY SEASONILITY (estos datos no son estacionales)

#IPI
#para plotear por series
ggsubseriesplot(as.ts(IPI$`Índice general` ))
ggseasonplot(as.ts(IPI$`Índice general`, start(IPI), end = end(IPI)))
#me dice que mis datos no son estacionales
density(IPI$`Índice general`)
plot(density(IPI$`Índice general`))

hist(IPI$`Índice general`)
#TEST DE NORMALIDAD
library(nortest)
lillie.test(IPI$`Índice general`)
#mI HIPÓTESIS ES QUE OS DATOS SIGUEN UNA NORMAL SI MI P-VALUE ES MAYOS DE 0.05 NO PUEDO RECHAZAR LA HIPÓTESIS
#El P-value me da 0.00000... que es MENOR que 0.05 por lo cual puedo rechazar la hipótesis MISDATOS NO SIGUEN UNS NORMAL
#correclacioón
ggAcf(IPI$`Índice general`)
plot(diff(IPI$`Índice general`))
ggAcf(diff(IPI$`Índice general`))
?stl


#CONSUMO DE GASOLINA
#para plotear por series
ggsubseriesplot(as.ts(consumo_gas$GASÓLEO))
ggseasonplot(as.ts(consumo_gas$GASÓLEO, start(IPI), end = end(IPI)))
#me dice que mis datos no son estacionales
plot(density(consumo_gas$GASÓLEO))
hist(consumo_gas$GASÓLEO)
#TEST DE NORMALIDAD
library(nortest)
lillie.test(consumo_gas$GASÓLEO)
#mI HIPÓTESIS ES QUE OS DATOS SIGUEN UNA NORMAL SI MI P-VALUE ES MAYOS DE 0.05 NO PUEDO RECHAZAR LA HIPÓTESIS
#El P-value me da 0.00000... que es MENOR que 0.05 por lo cual puedo rechazar la hipótesis MISDATOS NO SIGUEN UNS NORMAL
ggAcf(consumo_gas$GASÓLEO)
plot(diff(consumo_gas$GASÓLEO))
ggAcf(diff(consumo_gas$GASÓLEO))



#Mis datos tienen que ser estacionarios. Como estos tienen una clara línea de tending, obviemente no lo son. 
#una técnica es usar la dunción diff de differencing que básicamente hallará la variación 
plot(diff(IPI$`Índice general`))
ggAcf(diff(IPI$`Índice general`))

plot(diff(consumo_gas$GASÓLEO))
ggAcf(diff(consumo_gas$GASÓLEO))

autoplot(IPI$`Índice general`)

tsx= consumo_gas$GASÓLEO
tsy = precio_gas$`Precio del petróleo (barril Brent`()
y<-diff(coredata(tsy[paste(a,b,sep="/")]))
x<-diff(coredata(tsx))


x = diff(coredata(precio_gas$`Precio del petróleo (barril Brent)`[paste(start(consumo_gas$GASÓLEO),end(consumo_gas$GASÓLEO),sep = "/")]))
y = diff(coredata(consumo_gas$GASÓLEO))
length(x)==length(y)
renyi1d(x,y)
renyi2d(x,y)


x = diff(coredata(precio_gas$`Precio del petróleo (barril Brent)`[paste(start(IPI$`Índice general`),end(IPI$`Índice general`),sep = "/")]))
y = diff(coredata(IPI$`Índice general`)) #no es necesario llamar al coredata... el paquete funciona con xts
length(x)==length(y)
renyi1d(x,y)
renyi2d(x,y)

library(tidyr)
x = diff(IPI$`Índice general`[paste(S)])
y = diff(EN$`Consumo de energía eléctrica`[paste(start(IPI$`Índice general`,"/",sep = ""))] )
xy<-cbind.xts(x,y)
xy<-remove_missing(xy)
renyi1d(xy$Índice.general,xy$Consumo.de.energía.eléctrica)
renyi2d(xy$Índice.general,xy$Consumo.de.energía.eléctrica)
x<-xy$Índice.general
y<-xy$Consumo.de.energía.eléctrica


te_shannon<-transfer_entropy(x, y)
te_shannon$coef

######
#creo dos tablas con los datos que quiero comparar
calc_transfer_entropy<-function(datos1,datos2){
  xy<-create_xy(datos1,datos2)
  transfer_entropy(xy[,1],xy[,2])
}


conjunto2<-cbind(cbind.xts(EN,`colnames<-`(consumo_gas,paste("consumo",colnames(consumo_gas)))),EN)
plot.zoo(conjunto2)



  
  res<-lapply(conjunto3[periodo1], function(d){
    xy<-create_xy(d,IPI$`Índice general`)
    tail(xy)
    print(colnames(xy[,1]))
    colnames(xy)
    te<-transfer_entropy(xy[,1],xy[,2])
    data.table(tabla = paste(colnames(xy[,1])),
                       dir=  c("X->Y", "Y->X"),
                       coef(te)[1:2, 2:3],
                       pvalue = coef(te)[1:2, 4])
    #comparando todo
    
  })
  
  df <- rbindlist(res)
  
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->IPI GENERAL",
                                "IPI GENERAL->X"))]
  ggplot(df, aes(x = tabla, y = pvalue)) + 
    facet_wrap(~dir) +
    geom_hline(yintercept = 0.001, color = "blue") +
    geom_hline(yintercept = 0.01, color = "blue")+
    geom_hline(yintercept = 0.05, color = "blue")+
    geom_hline(yintercept = 0.1, color = "blue")+
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "P-Value") +
    geom_point()
  
  
  
  
  
#######probando bucle para plotear todos los periodos juntos->CONSEGUIDO YEAH!
df<-list()
  for (p in periodos){
    res<-lapply(conjunto2[p], function(d){
      print(colnames(d))
      xy<-create_xy(d,IPI$`Índice general`)
      te<-transfer_entropy(xy[,1],xy[,2])
      data.table(tabla = paste(colnames(xy[,1])),
                 periodo = p,
                 dir=  c("X->Y", "Y->X"),
                 coef(te)[1:2, 2:3],
                 pvalue = coef(te)[1:2, 4])
                
      
    })
  
    df <- rbind(df,rbindlist(res))
    
  }    
    df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                       labels = c("X->IPI GENERAL",
                                  "IPI GENERAL->X"))]
    ggplot(df, aes(x = tabla, y = pvalue)) + 
      facet_wrap(~dir) +
      geom_hline(yintercept = 0.001, color = "blue") +
      geom_hline(yintercept = 0.01, color = "blue")+
      geom_hline(yintercept = 0.05, color = "blue")+
      geom_hline(yintercept = 0.1, color = "blue")+
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "datos X", y = "P-Value") +
      geom_point(aes(color = periodo))
    
    
  
    #######probando representar conjunto contra conjunto->CONSEGUIDO YEAH!
    df<-list()
    for (i in 1:ncol(conjunto3)) {
      for (j in 1:ncol(conjunto3)) {
        xy <- create_xy(conjunto3[, i], conjunto3[, j])
        te <- transfer_entropy(xy[, 1], xy[, 2])
        df <- rbind(df,data.table(
          x = as.factor(paste(colnames(xy[, 1]))),
          y = as.factor(paste(colnames(xy[, 2]))),
          dir =  c("X->Y", "Y->X"),
          coef(te)[1:2, 2:3],
          pvalue = coef(te)[1:2, 4]
        ))
      }
    }
    
      df<-df[pvalue!=1]
  
      df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                       labels = c("X->Y",
                                  "Y->X"))]
 
      ggplot(df, aes(x = x, y = y)) + 
      facet_wrap(~dir) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "datos X", y = "datos Y") +
      geom_point(aes(color=ifelse(((abs(pvalue)>0 & abs(pvalue)>0.05)),"Influye", "no influye"))) 
      #+ scale_color_manual(guide=FALSE, values=c("green", "red"), labels("influye", "no influye")) 

      



#REPRESENTANDO LA TRANSFERENCIA DE ENTROPÍA

ggplot(df, aes(x = x, y = y)) + 
  facet_wrap(~dir) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "datos X", y = "datos Y") +
  geom_point(aes(size = ete)) 



#REPRESENTANDO LAS DOS COSAS

ggplot(df, aes(x = x, y = y, size = ete)) + 
  facet_wrap(~dir) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "datos X", y = "datos Y") +
  #geom_point(aes(size = ete)) 
  geom_point(aes(color=ifelse(((abs(pvalue)>0 & abs(pvalue)>0.05)),"no influye","Influye")))+
  guides(color=guide_legend("P-value"))




#######probando bucle para plotear todos los periodos juntos por periodos
conjunto3p<-conjunto3[periodo2]
df<-list()
for (i in 1:ncol(conjunto3p)) {
  for (j in 1:ncol(conjunto3p)) {
    if (colnames(conjunto3p[, i])!=(colnames(conjunto3p[, j]))){
    xy <- create_xy(conjunto3p[, i], conjunto3p[, j])
    print(colnames(xy[, 1]))
    print(colnames(xy[, 2]))
    te <- transfer_entropy(xy[, 1], xy[, 2])
   
    df <- rbind(df,data.table(
      x = as.factor(paste(colnames(xy[, 1]))),
      y = as.factor(paste(colnames(xy[, 2]))),
      dir =  c("X->Y", "Y->X"),
      coef(te)[1:2, 2:3],
      pvalue = coef(te)[1:2, 4]
    ))
    }
  }
}


df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                   labels = c("X->Y",
                              "Y->X"))]

ggplot(df, aes(x = x, y = y)) + 
  facet_wrap(~dir) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "datos X", y = "datos Y") +
  geom_point(aes(color=ifelse(((abs(pvalue)>0 & abs(pvalue)>0.05)),"no influye","Influye"))) +
guides(color=guide_legend("P-value"))
#+ scale_color_manual(guide=FALSE, values=c("green", "red"), labels("influye", "no influye")) 

#QUIERO PONER EN UNA LISTA TODOS LOS FACORES CON PVALUE MENOS DE 0.05 Y EN EL ORDEN QUE VA
result<- subset(df[order(ete)], pvalue < 0.05, select = c(x,y,dir))


list_result<-list()
for (i in 1:nrow(result)){
  if (result$dir[i]== "X->Y"){
    list_result[[i]]<- paste(result$x[i],"->",result$y[i])
  }
  else{
    list_result[[i]]<- paste(result$y[i],"->",result$x[i])
  }
}

cat(paste(list_result, collapse = '\n'))
