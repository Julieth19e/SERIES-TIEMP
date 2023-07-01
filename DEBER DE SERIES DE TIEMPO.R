###Universidad Regional Amazónica Ikiam
##Nombre: Erika Simbaña 
##SERIES DE TIEMPO 
library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)

#Autocorrelación
plot(lynx)
En esta gráfica se puede observar como existe una tendecia de manera polimonial,
además de una estacionalidad no constante y una autocorrelación alta.
acf(lynx)
Por el contrario aqui se puede observar lo que es una autocorrelación  
cual se la hace creando un modelo AR(k-1), en la que se muestra trazos por defecto 
sobre estimaciones de la función de autovarianza. 

library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)

ggAcf(lynx,type="correlation") + ggtitle("Auto-correlatión de avistamiento de linces")
"Esta gráfica hace referencia en cambio a una autocorrelacion parcial la cual muestra 
decaimiento exponencial en el ACF"

ggAcf(lynx,type="partial") + ggtitle("Auto-correlatión de avistamiento de linces")
"Esta gráfica igual hace referencia a una autocorrelación parcial en la cual se muestra
un valor p demostrado en rezagos sobresalientes significativos en el PACF"

ggtsdisplay(lynx,main='Avistamiento anual max de linces')
"Aqui se puede observar como su estacionalidad es de media estable y  
varianza estable de igual forma. Y en cuanto las gráficas de ACF Y PACF son distintas"

linces = as.data.frame(lynx)
l0 = linces[1:length(linces$x)-1, 1]
l1 = linces[2:length(linces$x),1]
plot(x = l0, y = l1)
"Aquí se puede ver una gráfica QQ en la cual se puede ver como la distribución de probabilidad 
esta de forma significativa e informativa"

library(moments)
hist(lynx)
"En este caso podemos ver como la frecuencia tiene un gran tamaño de un valor de >60 entre  0 a 1000 linces"

kurtosis(lynx)
skewness(lynx)

#Auto regresion (AR)
ar.model <- auto.arima(lynx, max.d=0, max.q = 0, allowdrift = T)
ar.model
"Se demuestra el ajuste lineal en el cual se puede ver como el valor de probabilidad es ineficaz
teniendo como valor -935.02, de igual forma se puede observar como el AIC logaritmo de verosimilitud es de 
1878.03 teniendo similitud con el de AICC"

#Media Movil (MA)
ma.model <- auto.arima(lynx, max.d=0, max.p = 0, allowdrift = T)
ma.model
"Aquí se puede ver como el valor de la probalidad incremento en un -938.89 al igual que incrementó 
el valor de AIC en un 1887.77"
#Auto Regresion con Media Movil (ARMA)
arma.model <- auto.arima(lynx, max.d=0, allowdrift = T)
arma.model
"En este caso se puede ver como este modelo hace que sea mejor teniendo un AIC mucho más bajo 
que los otros dos modelos en un 1876.17"
#ARIMA
arima.model <- auto.arima(lynx, allowdrift = T)
arima.model
"En este último el valor de AIC es igual ya que es intregado al modelo de ARMA "
#Representacion de los modelos
ajustados <- cbind(lynx, ar.model$fitted, ma.model$fitted, arma.model$fitted,arima.model$fitted)
autoplot(ajustados)
"En este caso esta gráfica hace referencia a la combinación de todos los modelos en cuanto a la data de linces,
el cual se puede deducir finalmente que el modelo que mejor se asocia es el de ARIMA, además de demostar 
una media estable al igual que la varianza, la estacionalidad en este caso se puede deducir que es constante
y una tendencia polimonial"
#PREDICCIONES
ar.forecast <- forecast(ar.model, h = 20, level = 95)
ma.forecast <- forecast(ma.model, h = 20, level = 95)
arma.forecast <- forecast(arma.model, h = 20, level = 95)
arima.forecast <- forecast(arima.model, h = 20, level = 95)
g1 <- autoplot(ar.forecast)
g2 <- autoplot(ma.forecast)
g3 <- autoplot(arma.forecast)
g4 <- autoplot(arima.forecast)
grid.arrange(g1,g2, g3, g4, nrow=2, ncol=2)
"En cuanto a la predicción se puede ver como en los 4 gráficos son distintos teniendo como el más efectivo
o adecuado el ARIMA (0,0,3)"
#Cargar la data
linces <- read.csv("captured_lynx.csv")
summary(lynx)

#Análisis preliminar
y <- ts(lynx, start = 1821, frequency = 1)
# 1: año
# 12: meses
# 52: semanas
# 4 : trimestres
# 3: quatrimestres
# 365: diario

autoplot(y) + ggtitle("Linces capturados por año (1821-1934)") +
  ylab("Número de Linces") + xlab("Años") + theme_bw()

#Análisis de estacionalidad y otros patrones
dy <- diff(y) # quita tendencias 
autoplot(dy) + ggtitle("Linces capturados por año (1821-1934)") +
  ylab("Número de Linces") + xlab("Años") + theme_bw()


cafe <- fpp2::auscafe
cafe_plot <- autoplot(cafe) + 
  ggtitle("Gasto en café en Australia (Abril 1982 - Sept.2017)") +
  ylab("Gasto (Billón AUS $)") + xlab("Tiempo") + theme_bw()

cafe_dif <- diff(cafe)
# Estacional
cafe_season <- ggseasonplot(cafe_dif) + 
  ggtitle("Estacionalidad del gasto en café") +
  ylab("Gasto (Billón AUS $)") + xlab("Tiempo") + theme_bw()
# Comparacion de meses
cafe_mes <- ggsubseriesplot(cafe_dif) +
  ggtitle("Comparación mensual del gasto en café") +
  ylab("Gasto (Billón AUS $)") + xlab("Tiempo") + theme_bw()

cafe_plot


cafe_season

cafe_mes

## Descomposicion
cafe = auscafe
cafe %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Descomposicion multiplicativa de gastos en café AUS") + 
  theme_bw()


cafe %>% seas(x11="") -> fit

autoplot(cafe, series="Data") +
  autolayer(trendcycle(fit), series="Tendencia", size = 0.8) +
  autolayer(seasadj(fit), series="Estacional", size = 0.8) +
  xlab("Año") + ylab("Gasto en café billón AUS $ ") +
  ggtitle("Consumo de café en Australia ") +
  scale_colour_manual(values=c("darkgray","blue","red"),
                      breaks=c("Data","Estacional","Tendencia")) + 
  theme_light()


##Proyecciones
# Método simple: benchamark estacionanal (seasonal naive )
lince_sn <- snaive(dy)   # SD_residuals = 1327.5588
summary(lince_sn)

checkresiduals(lince_sn)


# Método intermedio: ets (exponential smoothing model)
lince_ets <- ets(dy)    # SD_residuals = 1197.701
summary(lince_ets)

checkresiduals(lince_ets)

# Método avanzado: ARIMA (AutoRegressive Integrative Moving Average)
lince_arima <- auto.arima(lynx, 
                          d = 1, D = 1,
                          stepwise = FALSE,
                          approximation = FALSE,
                          trace = TRUE)    # SD_residuals = 988.7527
summary(lince_arima)

checkresiduals(lince_arima)

#linces_sn_futuro <- forecast(lince_sn, h = 20)
linces_ets_futuro <- forecast(lince_ets, h = 30)
linces_arima_futuro <- forecast(lince_arima, h = 10)

#autoplot(linces_sn_futuro)
autoplot(linces_ets_futuro)

autoplot(linces_arima_futuro)

autoplot(linces_arima_futuro, include = 10)


###GDP de un país
library(readxl)
library(forecast)
library(tseries)
gdp <- read_excel("data/gdp_ind.xlsx")
View(gdp)
gdp_ts <- ts(gdp$GDP, start = min(gdp$DATE),end = max(gdp$DATE),frequency = 4)


plot(gdp_ts)
