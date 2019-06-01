library(RTransferEntropy)
library(ggplot2)
IPI<-datosxts$`Índice general`
GAS<-consumo_gas$GASOLINA
start(IPI)
start(GAS)
end(GAS)
IPI[end(GAS)]
IPI[start(GAS)]
n<-2500
rep(0,n+1) #repite el 0 n+1 veces #básicamente lo puedo utilizar para crear una lista en un bubcle
set.seed(12345) #supongo que esto es para el número rándom
x <- rep(0, n + 1)
y <- rep(0, n + 1)

for (i in 2:(n + 1)) {
  x[i] <- 0.2 * x[i - 1] + rnorm(1, 0, 2)
  y[i] <- x[i - 1] + rnorm(1, 0, 2)
}
rnorm(1, 0, 2) #me da un número random dentro de una normal dada, en este caso con media 1 varianza 2
#Tengo dos variables x e y en la que y depende de x pero a la verz tienen un componente random (básicamente simula)

#me cargo el primer valor de los dos... ¿porqué?
x <- x[-1]
y <- y[-1]


library(future)
# enable parallel processing for all future transfer_entropy calls
plan(multiprocess)
set.seed(12345)
shannon_te <- transfer_entropy(x, y)
shannon_te     
#El transfer entropy en dirección x-> es 0.922 mientras que al revés es 0.006.. 
#como se espera hay una importante transferencia de X a Y

#If we only want to calculate the transfer entropy from X to Y (for the opposite 
#flow we would simple have to reverse the input parameters) and omit the bootstrap 
#and other calculations, we can use the calc_te function (or the calc_ete function for the effective transfer entropy).
calc_te(x, y) #me da el transfer entropy x-> 0.9922
calc_ete(x,y) #me da Eff. TE (effective transfer entropy) que no sé muy bien que es

#and Y->X
calc_te(y, x)
#> [1] 0.002455513
calc_ete(y, x)
#> [1] 0

#Note that the effective transfer entropy relies on a random component (induced by shuffling and repeated reestimation). 
#Therefore, the results might be slightly different.... (LO TENDRÉ QUE LEER OTRA VEZ)... no sé todavia que es un bootstrap

#EJEMPLO 2 DONDE NO SE REALIZA UNA RELACIÓN LINEAL

Set.seed(12345)
n <- 2500
x <- rep(0, n + 200)
y <- rep(0, n + 200)

x[1] <- rnorm(1, 0, 1)
y[1] <- rnorm(1, 0, 1)

for (i in 2:(n + 200)) {
  x[i] <- 0.2 * x[i - 1] + rnorm(1, 0, 1)
  y[i] <- sqrt(abs(x[i - 1])) + rnorm(1, 0, 1)
}

x <- x[-(1:200)]
y <- y[-(1:200)]

shannon_te2 <- transfer_entropy(x, y)
#aunque TE y Eff.TE son bajos, el p-value es de 0.000 para la reción X->Y lo que quiere decir que hay una relación



library(data.table) # for data manipulation



res$ADBE

help(split)
head(split(stocks, stocks$ticker))


library(data.table) # for data manipulation


rt<-as.xts(stocks[ticker=="AES"]$ret, order.by = as.Date(stocks[ticker=="AES"]$date))
sp500<-as.xts(stocks[ticker=="AES"]$sp500, order.by = as.Date(stocks[ticker=="AES"]$date))

te <- transfer_entropy(rt, sp500, shuffles = 50, nboot = 100, quiet = T)
te


res <- lapply(split(stocks, stocks$ticker), function(d) {
  te <- transfer_entropy(d$ret, d$sp500, shuffles = 50, nboot = 100, quiet = T)
  
  data.table(
    ticker = d$ticker[1],
    dir = c("X->Y", "Y->X"),
    coef(te)[1:2, 2:3]
  )
})

df <- rbindlist(res)

# order the ticker by the ete of X->Y
df[, ticker := factor(ticker, 
                      levels = unique(df$ticker)[order(df[dir == "X->Y"]$ete)])]

# rename the variable (xy/yx)
df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                   labels = c("Flow towards Market",
                              "Flow towards Stock"))]

ggplot(df, aes(x = ticker, y = ete)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = "Effective Transfer Entropy") +
  geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                    ymax = ete + qnorm(0.95) * se),  
                width = 0.25, col = "blue") +
  geom_point()



