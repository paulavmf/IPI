plot.xts(IPI$`Índice general`, main = "Índice de producción Industrial")
plot.xts(conjunto2$Energía.eléctrica.disponible, main = "Energía eléctrica disponible(MW/h)")
plot.xts(conjunto2$Producción.bruta.de.energía.eléctrica, main = "Producción Bruta de Energía Eléctrica(MW/h)")
plot.xts(conjunto2$Consumo.de.energía.eléctrica , main = "Consumo de Energía Eléctrica(MW/h)")
plot.xts(EN, legend.loc = "bottomright", auto.legend=TRUE, main="Datos de Energía Eléctrica (MW/h)", ylab("MW/H"))

plot.xts(consumo_gas$FUELÓLEO, main = "Consumo de Fuelóleo(Ton)")
plot.xts(conjunto2$consumo.GASÓLEO, main = "Consumo de Gasóleo(Ton)")
plot.xts(conjunto2$consumo.GASOLINA, main = "Consumo de Gasolina(Ton)")
plot.xts(consumo_gas, legend.loc = "bottomright", auto.legend=TRUE, main="Consumo de derivados del petróleo en Canarias(Ton)", ylab("MW/H"))


plot.xts(conjunto2$Precio.del.petróleo..barril.Brent. , main = "Precio en Dólares del Barril de Brent")
plot.xts(sup_construida, main = "Superficie a Construir (m²)")
plot.xts(ocupacion, main = "Población Ocupada en Canarias")
plot.xts(turismo, main = "Alojamientos Turísticos Abiertos")

c
IPI<-IPI$`Índice general`
ggAcf(IPI)
p = periodogram(IPI)

autoplot(naive_IPI) + autolayer(serie = "ajuste", fitted(naive_IPI)) +
  xlab("año") + ylab("index")



autoplot(training) +
  autolayer(meanf(training, h=12),
            series=paste("Mean MAPE=",round(MAPE_mean,digits = 3)), PI=FALSE) +
  autolayer(naive(training, h=12),
            series=paste("Naïve MAPE=",round(MAPE_naive,digits = 3)), PI=FALSE) +
  autolayer(rwf(training, drift=TRUE, h=12),
            series=paste("Drift MAPE=",round(MAPE_drift,digits = 3)), PI=FALSE)  +
  ggtitle("Índice de Producción industrial") +
  xlab("año") + ylab("index") +
  guides(colour=guide_legend(title="Forecast"))

  