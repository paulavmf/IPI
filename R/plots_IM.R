library(RTransferEntropy)
library(ggplot2)
library(future)
library(data.table)
library(gridExtra)


#FUNCIÓN PARA COMBINAR DOS SERIES TEMPORALES, CREAR SU DIFERENCIA Y HACER QUE TENGAN LA MISMA FECHA DE FIN Y DE COMIENZO

create_xy<-function(data1,data2){  
  xy<-cbind.xts(diff(data1),diff(data2))
  xy<-suppressWarnings(remove_missing(xy, na.rm = TRUE))
  return(xy)
}



#In R you can only return one object. You could put the three values in a list or a vector and then return that list or vector.


renyi1d<-function(x,y){
  #Me devuelve la curva de la entropía de Renyi para los distintos valores de q en contraste con el valor de Shannon
  set.seed(1234567890)
  qs <- c(seq(0.1, 0.9, 0.1), 0.99)
  # calculate the X->Y transfer entropy value
  te <- sapply(qs, function(q) calc_te(x, y, entropy = "renyi", q = q))
  names(te) <- sprintf("q = %.2f", qs)
  te
  te_shannon <- calc_te(x, y)
  te_shannon
  text_df <- data.frame(x = 0.25, 
                        y = te_shannon, 
                        lab = sprintf("Shannon's TE = %.4f", te_shannon))
  
  ggplot(data.frame(x = qs, y = te), aes(x = x, y = y)) +
    geom_hline(yintercept = te_shannon, color = "red", linetype = "dashed") +
    geom_smooth(se = F, color = "black", size = 0.5) +
    theme_light() +
    labs(x = "Values for q", y = "Renyi's Transfer Entropy",
         title = "Renyi's Transfer Entropy for different Values of q",  subtitle = paste("X =",colnames(x),"and","Y =", colnames(y), sep = " ")) +
    geom_text(data = text_df,
              aes(label = lab), color = "red", nudge_y = 0.01)
}


#####UN POCO MÁS PRO
renyi2d<-function(x,y){
  #misma finrmación que renyi2d pero añadiendo un error
  #Esta función dibuja una gráfica con las distintas transferencias de entropía para distintos valores de p, transferencia de renyi.
  #además dibuja una línea roja señalando la transferencia de entropía de shanon y también señala el posible error
  qs <- c(seq(0.1, 0.9, 0.1), 0.99)
  
  q_list <- lapply(qs, function(q) {
    set.seed(1234567890)  
    # transfer_entropy will give a warning as nboot < 100
    suppressWarnings({
      tefit <- transfer_entropy(x, y, lx = 1, ly = 1, 
                                entropy = "Renyi", q = q, 
                                shuffles = 50, quantiles = c(5, 95), #no pillo para qué sirve esto
                                nboot = 100, quiet = T)
    })
    
    data.table(
      q   = q,
      dir = c("X->Y", "Y->X"),
      coef(tefit)[, 2:3]
    )
    
  })
  qdt <- rbindlist(q_list)
  
  sh_dt <- data.table(
    dir = c("X->Y", "Y->X"),
    ete = c(calc_ete(x, y), calc_ete(y, x)) #me da la de shannon
  )
  
  qdt[, pe := qnorm(0.95) * se]
  ggplot(qdt, aes(x = q, y = ete)) +
    geom_hline(yintercept = 0, color = "darkgray") + 
    geom_hline(data = sh_dt, aes(yintercept = ete), linetype = "dashed",
               color = "red") +
    geom_point() +
    geom_errorbar(aes(ymin = ete - pe, ymax = ete + pe), 
                  width = 0.25/10, col = "blue") +
    facet_wrap(~dir) +
    labs(x = "Values for q", y = "Renyi's Transfer Entropy",
         title = "Renyi's Transfer Entropy for different Values of q",
         subtitle = paste("X =",colnames(x),"and","Y =", colnames(y), sep = " "))
  #return(sh_dt)
}

#NO ENTIENDO PORQUÉ ME DAN COSAS DIFERENTES

#también quiero el resultado del P-VALUE y de ETE solo para tener una idea


compara_pvalues<-function(conjunto,dato_comparar, ordenar = TRUE){
  #esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja solo la gráfica correpondiente a los distintos P-values
  #para ambas direcciones
  
  res<-lapply(conjunto, function(d){
    xy<-create_xy(d,dato_comparar)
    te<-suppressWarnings(transfer_entropy(xy[,1],xy[,2],quiet = T))
    data.table(tabla = paste(colnames(xy[,1])),
               dir=  c("X->Y", "Y->X"),
               coef(te)[1:2, 2:3],
               pvalue = coef(te)[1:2, 4])
    
  })
  
  
  df <- rbindlist(res)
  
  if (ordenar == TRUE){
  # order the ticker by the pvalue of X->Y
    df[, tabla := factor(tabla, 
                        levels = unique(df$tabla)[order(df[dir == "X->Y"]$pvalue)])]
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
    geom_point()

}





compara_ete<-function(conjunto,dato_comparar, ordenar = TRUE){
  #esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja solo la gráfica correpondiente a la trasferencia de entropía enfectiva
  #para ambas direcciones
  res<-lapply(conjunto, function(d){
    xy<-create_xy(d,dato_comparar)
    te<-suppressWarnings(transfer_entropy(xy[,1],xy[,2],quiet = T))
    data.table(tabla = paste(colnames(xy[,1])),
               dir=  c("X->Y", "Y->X"),
               coef(te)[1:2, 2:3])
    
  })
  
  df <- rbindlist(res)
  df$pvalue
  
  if (ordenar ==TRUE){
  # order the ticker by the ete of X->Y
  df[, tabla := factor(tabla, 
                       levels = unique(df$tabla)[order(df[dir == "X->Y"]$ete, decreasing = TRUE )])]
  }
  order
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->IPI GENERAL",
                                "IPI GENERAL->X"))]
  ggplot(df, aes(x = tabla, y = ete)) + 
    facet_wrap(~dir) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "Effective Transfer Entropy") +
    geom_point()
  
}



compara<-function(conjunto,dato_comparar, ordenar = TRUE){
#esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja las dos gráficas correspondientes
  #para ambas direcciones
  res<-lapply(conjunto, function(d){
    xy<-create_xy(d,dato_comparar)
    te<-suppressWarnings(transfer_entropy(xy[,1],xy[,2],quiet = T))
    data.table(tabla = paste(colnames(xy[,1])),
               dir=  c("X->Y", "Y->X"),
               coef(te)[1:2, 2:3],
               pvalue = coef(te)[1:2, 4])
            
    
  })
  
  
  df <- rbindlist(res)
  
  if (ordenar == TRUE){
    # order the ticker by the pvalue of X->Y
    df[, tabla := factor(tabla, 
                         levels = unique(df$tabla)[order(df[dir == "X->Y"]$pvalue)])]
  }
  
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->IPI GENERAL",
                                "IPI GENERAL->X"))]
  p1<-ggplot(df, aes(x = tabla, y = pvalue)) + 
    facet_wrap(~dir) +
    geom_hline(yintercept = 0.001, color = "blue") +
    geom_hline(yintercept = 0.01, color = "blue")+
    geom_hline(yintercept = 0.05, color = "blue")+
    geom_hline(yintercept = 0.1, color = "blue")+
    ggtitle("Comparativa P-value")+
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "P-Value") +
    geom_point()
  
  p2<-ggplot(df, aes(x = tabla, y = ete)) + 
    facet_wrap(~dir) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    ggtitle("Comparativa Transferencia Efectiva de Entropía")+
    labs(x = "datos X", y = "Effective Transfer Entropy") +
    #esto me añade una barra con el ancho de la distribución del error... la barrita sale muy grande..
    #no creo  ue de mucha info
    geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                       ymax = ete + qnorm(0.95) * se),  
                  width = 0.25, col = "blue") +
    geom_point()
  return(list(p1,p2))
  
}


#COMPARAR VARIOS PERIODOS

compara_periodos_pvalue<- function(conjunto,dato_comparar,periodos){
  #esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #para cada uno de los periodos a estudiar
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-X
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja la gráfica p-value
  #para ambas direcciones y distinguiendo por colores cada periodo
df<-list()
for (p in periodos){
  res<-lapply(conjunto3[p], function(d){
    xy<-create_xy(d,IPI$`Índice general`)
    te<-transfer_entropy(xy[,1],xy[,2],quiet = T)
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
p1<-ggplot(df, aes(x = tabla, y = pvalue)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0.001, color = "blue") +
  geom_hline(yintercept = 0.01, color = "blue")+
  geom_hline(yintercept = 0.05, color = "blue")+
  geom_hline(yintercept = 0.1, color = "blue")+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
  labs(x = "datos X", y = "p-value") +
  geom_point(aes(color = periodo))


}



compara_periodos_ete<- function(conjunto,dato_comparar,periodos){
  #esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #para cada uno de los periodos a estudiar
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-X
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja la gráfica trasferencia efectiva de entropía
  #para ambas direcciones y distinguiendo por colores cada periodo
  df<-list()
  for (p in periodos){
    res<-lapply(conjunto3[p], function(d){
      xy<-create_xy(d,IPI$`Índice general`)
      te<-transfer_entropy(xy[,1],xy[,2],quiet = T)
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
  ggplot(df, aes(x = tabla, y = ete)) + 
    facet_wrap(~dir)  +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "Effective Transfer Entropy")  +
    geom_point(aes(color = periodo))
}




#SACO EN UN SOLO PLOT LA GRÁDICA QUE COMPARA LOS P-VALUE Y LA TRANSFERENCIA DE ENTROÍA EFECTIVA
compara_periodos<- function(conjunto,dato_comparar,periodos){
  #esta función compara un conjunto de series temporales xts con una sola serie temporal (muchas contra una)
  #para cada uno de los periodos a estudiar
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-X
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja las dos gráficas correspondientes
  #para ambas direcciones y distinguiendo por colores cada periodo
  df<-list()
  for (p in periodos){
    res<-lapply(conjunto[p], function(d){
      xy<-create_xy(d,IPI$`Índice general`)

      if ((p == "2002-01-01/2007-12-01") & (colnames(d)== "Alojamientos.turísticos.abiertos")){
      te<-transfer_entropy(rep(c(1,2,3),10),rep(c(0,0,0),10), quiet = TRUE)
      data.table(tabla = paste(colnames(xy[,1])),
                 periodo = p,
                 dir=  c("X->Y", "Y->X"),
                 coef(te)[1:2, 2:3],
                 pvalue = coef(te)[1:2, 4])
      }
      else{
        
        te<-transfer_entropy(xy[,1],xy[,2],quiet = T, shuffles = 200)
        data.table(tabla = paste(colnames(xy[,1])),
                   periodo = p,
                   dir=  c("X->Y", "Y->X"),
                   coef(te)[1:2, 2:3],
                   pvalue = coef(te)[1:2, 4])
    
      }
    })
    
    
    df <- rbind(df,rbindlist(res))
    
  }    
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->IPI GENERAL",
                                "IPI GENERAL->X"))]
 
 p1<- ggplot(df, aes(x = tabla, y = pvalue)) + 
    facet_wrap(~dir) +
    geom_hline(yintercept = 0.001, color = "blue") +
    geom_hline(yintercept = 0.01, color = "blue")+
    geom_hline(yintercept = 0.05, color = "blue")+
    geom_hline(yintercept = 0.1, color = "blue")+
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "p-value") +
   ggtitle("Comparativa P-value")+
    geom_point(aes(color = periodo), size = 5)

 p2<- ggplot(df, aes(x = tabla, y = ete)) + 
    facet_wrap(~dir)  +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "Effective Transfer Entropy")  +
    geom_point(aes(color = periodo),size = 5)
    grid.arrange(p1, p2, nrow = 2)
    
    return(list(p1,p2))
  
}



conjunto_conjunto<- function(conjunto){
  
  #esta función compara un conjunto de series temporales xts con el mismo conjunto de series temporales dos a dos (muchas contra muchas)
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-X
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja las dos gráficas 
  #la gráfica P-value pinta en un color diferente las series temporales que tienen transferencia de entropía relevante y las que no
  #En la gráfica de transferencia d entropía efectiva los puntos e mayor tamaño son los que tienen mayor transferencia de entropía
  df<-list()
  for (i in 1:ncol(conjunto)) {
    for (j in 1:ncol(conjunto)) {
      if (colnames(conjunto[, i])!=(colnames(conjunto[, j]))){
      xy <- create_xy(conjunto[, i], conjunto[, j])
      te <- transfer_entropy(xy[,1],xy[,2],quiet = T, shuffles = 200)
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
  
  df<-df[pvalue!=1]
  
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->Y",
                                "Y->X"))]
  
  p1<-ggplot(df, aes(x = x, y = y)) + 
    facet_wrap(~dir) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "datos Y") +
    geom_point(aes(color=ifelse(((abs(pvalue)>0 & abs(pvalue)>0.05)),"no influye","Influye"))) +
    guides(color=guide_legend("P-value"))
  
  p2<-ggplot(df, aes(x = x, y = y)) + 
    facet_wrap(~dir) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "datos Y") +
    geom_point(aes(size = ete)) 
  
  
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
  
  
  
  return(list(p1,p2,cat(paste(list_result, collapse = '\n'))))
  
}



conjunto_conjunto2<- function(conjunto){
  
  #esta función compara un conjunto de series temporales xts con el mismo conjunto de series temporales dos a dos (muchas contra muchas)
  #calcula la trasferencia de entropía de X(conjunto)->Y(la serie única) e Y<-X
  #el resultado: p-value y ete lo almacena en una tabla y luego dibuja UNA SOLA GRÁFICA 
  #la gráfica P-value pinta en un color diferente las series temporales que tienen transferencia de entropía relevante y las que no
  #En la gráfica de transferencia d entropía efectiva los puntos e mayor tamaño son los que tienen mayor transferencia de entropía
  df<-list()
  for (i in 1:ncol(conjunto)) {
    for (j in 1:ncol(conjunto)) {
      if (colnames(conjunto[, i])!=(colnames(conjunto[, j]))){
      xy <- create_xy(conjunto[, i], conjunto[, j])
      te <- transfer_entropy(xy[,1],xy[,2],quiet = T,shuffle = 300)
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

  df<-df[pvalue!=1]
  
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->Y",
                                "Y->X"))]
  result<- subset(df, pvalue < 0.05, select = c(x,y,dir,ete))
  
  ggplot(df, aes(x = x, y = y, size = ete)) +
    facet_wrap(~dir) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "datos Y") +
    #geom_point(aes(size = ete))
    geom_point(aes(color=ifelse(((abs(pvalue)>0 & abs(pvalue)>0.05)),"no influye","Influye")))+
    guides(color=guide_legend("P-value"))
  
  
  # #LA GRÁFICA DE SIEMPRE
  # p2<-ggplot(result, aes(x = x, y = y, size = ete)) +
  #   facet_wrap(~dir) +
  #   theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
  #   labs(x = "datos X", y = "datos Y") +
  #   geom_point(aes(size = ete))+
  #   guides(color=guide_legend("P-value"))
  # 
  # 
  # list_result<-list()
  # for (i in 1:nrow(result)){
  #   if (result$dir[i]== "X->Y"){
  #     list_result[[i]]<- paste(result$x[i],"->",result$y[i])
  #   }
  #   else{
  #     list_result[[i]]<- paste(result$y[i],"->",result$x[i])
  #   }
  # }
  # return(list(p1,p2,cat(paste(list_result, collapse = '\n'))))

}



conjunto_conjunto2p<- function(conjunto,periodos){
  df<-list()
  for (p in periodos){
    for (i in 1:ncol(conjunto)) {
      for (j in 1:ncol(conjunto)) {
        if (colnames(conjunto[, i])!=(colnames(conjunto[, j]))){
          xy <- create_xy(conjunto[, i], conjunto[, j])
          te <- transfer_entropy(xy[,1],xy[,2],quiet = T,shuffle = 20)
          df <- rbind(df,data.table(
            x = as.factor(paste(colnames(xy[, 1]))),
            y = as.factor(paste(colnames(xy[, 2]))),
            periodo = p,
            dir =  c("X->Y", "Y->X"),
            coef(te)[1:2, 2:3],
            pvalue = coef(te)[1:2, 4]))
        }
      }
    }
  }
  df <- rbind(df,rbindlist(res))
  
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("X->IPI GENERAL",
                                "IPI GENERAL->X"))]
  
  result<- subset(df, pvalue < 0.05, select = c(x,y,dir,ete)) 
  ggplot(result, aes(x = x, y = y, size = ete)) +
    facet_wrap(~dir) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +
    labs(x = "datos X", y = "datos Y") +
    geom_point(aes(size = ete))+
    geom_point(aes(shape = periodo))
  
  
}         

    
