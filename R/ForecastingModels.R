

########################STUDY SEASONILITY (estos datos no son estacionales)
#para plotear por series
ggsubseriesplot(as.ts(IPI$`Índice general` )) #me dice que mis datos no son estacionales pero creo que simplemente el objeto ts está mañ hecho
ggseasonplot(as.ts(IPI$`Índice general`, start(IPI), end = end(IPI)))


#################################LA CORRELACIÓN
ggAcf(IPI$`Índice general`)

#EL IPI GENERAL ESTÁ SÚPER CORRELACIONADO
gglagplot(IPI$`Índice general`, lags = 9) #eSTE PLOR NO LO TERMINO DE ENTENDER
plot(diff(IPI$`Índice general`))

ggAcf(diff(IPI$`Índice general`))
#aHORA ESLO ESTÁ MUCHO MENOS PORQUE LE QUITÉ EL TRENDING


########################EMPIEZO A PROBAR FORECAST


#NAIVE FORECAST

fcIPI_gen<-naive(IPI$`Índice general`, h = 5)
class(fcIPI_gen) #CLASE FORECAST
autoplot(fcIPI_gen)


#CHECK RESIDUALS: 
#comprobar si el residuo de el naive forecast punto por punto de mi serie temportal es un white noise gaussiano:
#3 FORMAS:
# 1. hacer un check residuals utilizando pipe
IPI$general %>% naive() %>% checkresiduals()

# 2.en vez de hacer 
checkresiduals(naive(IPI$`Índice general`))
#SE PUEDE DECIR QUE SIGUE UNA NORMAL?? esto no me ha quedado claro, repasar

#Creo la versión ts de los dastos
ts_IPIgen<-ts(IPI$`Índice general`, start =c(2002, 1), end = c(2018,7) , frequency = 12)
str(ts_IPIgen)


#lo del ts o no ts me está volviendo media loca
#TRAINING SET TEST SET HALLANDO ACCURACY
training<-subset(ts_IPIgen, end = 100)
naive_IPIgen <- naive(training, h = 99)
accuracy(naive_IPIgen,ts_IPIgen ) #OTRA VEZ EL PROBLEMA DE QUE NO ES UN TS

#USANDO EL AS.TS SIN VOLVERME LOCA
training2<-subset(as.ts(IPI$general), end = 100)
naive_IPIgen2 <- naive(training2, h = 99)
accuracy(naive_IPIgen2, as.ts(IPI$general))  #OTRA VEZ EL PROBLEMA DE QUE NO ES UN TS
#DE ESTA MANERA EL ERROR MASE DA LLIGERAMENTE DIFERENTE, PORQUÉ???

#llamo a una error en concreto en la matriz accuracy
accuracy(naive_IPIgen2, as.ts(IPI$general))["Test set", "MAPE"]

autoplot(naive_IPIgen) + autolayer(fitted(naive_IPIgen)) 


#USANDO CROSS VALIDATION
#error que produce naive forecast ccalculando 10 pasos con cross validation
e<- tsCV(IPI$general, forecastfunction = naive, h = 10)


# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)


#el error mse según e horizonte
data.frame(h = 1:10, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()



#SIMPLE EXPONENTIAL SMOOTHING
sesfc <- ses(IPI$general,  h = 10)
summary(sesfc)
autoplot(sesfc)
autoplot(sesfc) + autolayer(fitted(sesfc)) #veo como predoice el mode lo cada valor

#HOLTS METHOD, igual al simple exponential smoothing pero añadiendo la tendencia (trend)

# Produce 10 year forecasts of austa using holt()
fcholt <-  holt(IPI$general, h = 10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)+ autolayer(fitted(fcholt))

# Check that the residuals look like white noise
checkresiduals(fcholt)
#si mi p-value es menor que 0.05 mi residio no es ehite noise

#ETS MODEL
#elige el modelo EXPONENTIAL SMOOTHING minimizando el aikeike's parametro 
#es como una probabilidad de que el modelo esté equivocado...
etsfc <- ets(IPI$general)
#ME DA UN (M,N,N) osea, tiene un multiplicative trend no es aditivo y no es seansonal
checkresiduals(etsfc)
#ets no es una clase forecast en si por eso tengo que hacer autoplot(forecast(...))

#BOX-COX transformations
#transformación exponencial de los datos, es decir, aplicar logaritmo, raíz, raíz cúbica, para hallar un mejor modelo ETS
l = BoxCox.lambda(IPI$general)
IPI$general %>% BoxCox(lambda = l) %>% autoplot()

#ARIMA MODEL
#CREA UN MODELO ARIMA ÓPIMO (optimizando AIC) DE MANERA AUTOMÁTICA
#ARIMA = autoregressive integrated moving average
ar_model <-auto.arima(IPI$general)
checkresiduals(ar_model) #MIS RESIDUOS NO SON WHITE NOISE
summary(ar_model)
ar_model%>%forecast(h = 5)%>%autoplot()


#Para crear mi propio ARIMA(0,1,1) "include.constant = FALSE" quiere decir sin drift

IPI$general %>% Arima(order = c(0, 1, 1), include.constant = TRUE) %>% forecast() %>% autoplot()

IPI$general %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

#COMPARANDO ETS Y ARIMA MEDIANTE CROSS VALIDATION
# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}
remove(e1, e2)
#la verdad es que no entiendo porque las funciones fets y farima función así......
# Compute CV errors for ETS as e1
e1 <- tsCV(IPI$general, fets, h = 1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(IPI$general, farima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm = TRUE) #el mejor modelo es ets
mean(e2^2, na.rm = TRUE)
IPI$general %>% ets() %>% forecast(h = 10) %>% autoplot()


#DYMAMIX REGRESSION MODELS
#uso información de otra TS para mi forecast

autoplot(IPI$general, IPI$energía, facets = TRUE)

fit <- auto.arima(IPI$general,xreg = IPI$energía)
summary(fit)

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)


checkresiduals(fit) #no es white noise..
#a la hora de hacer el fit tengo que dar un valor al error(la TS que uso, en eeste caso la energía)
forecast(fit, xreg = cbind(rep(mean(window(IPI$energía,start = "2015-01-01", end = end(IPI)),10))))



#NEURAL NETWORK
nnfit<- nnetar(IPI$general, lambda = 0)
autoplot(forecast(nnfit, h =10))
help(nnetar)


fcast <- forecast(nnfit, PI=TRUE, h=30)
autoplot(fcast)
