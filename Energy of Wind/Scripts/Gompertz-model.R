rm(list = ls())

library(tidyverse)
library(easynls)
library(nlme)

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

data = read.csv(file = paste(getwd(),'/data/owid-energy-data.csv', sep=""), head=TRUE, sep=',')
model_col = c('year', 'country', 'wind_electricity')

# Germany Case
data_germany = data[data['country']=='Germany',]
# Electricity generation from wind, measured in terawatt-hours
mask_has = data_germany['wind_electricity']>0
mask_na = is.na(data_germany['wind_electricity'])

data_germany_wind = data_germany[!mask_na & mask_has, model_col]

# add end data so that model doesn't give error message
newdata = data.frame(year = 2060, country='Germany',wind_electricity=380)
data_germany_wind = rbind(data_germany_wind, newdata)

# Logistic model, Asym/(1+exp((xmid-input)/scal))
y.low <- nls(wind_electricity ~ SSlogis(year, Asym, xmid, scal), data=data_germany_wind)
summary(y.low)
Asym = coef(y.low)[1]
xmid.mid = coef(y.low)[2]-1.3
scal.mid = coef(y.low)[3]/1.3
x = c(2022:2080)
y.mid = Asym/(1+exp((xmid.mid-x)/scal.mid))

xmid.high = coef(y.low)[2]-2
scal.high = coef(y.low)[3]/2
y.high = Asym/(1+exp((xmid.high-x)/scal.high))

setwd('..')
png(file = paste(getwd(),'/Visualizations/logistic-model-germany.png', sep=""), width=600, height=600)

plot(wind_electricity~year, data_germany_wind,
     xlab='Year', xlim = range(1985,2060),
     ylab='Electricity generation from wind, TWh', ylim = range(0, 400),
     main='Projections of Electricity Generation from wind, Germany')

curve(predict(y.low, newdata = data.frame(year=x)), add=TRUE, col='green', lty = 2, lwd=2)
lines(x,y.mid, col='orange', lty = 2, lwd=2)
lines(x,y.high, col='red', lty = 2, lwd=2)
abline(h=390, col='purple', lwd=3)
abline(v=2035, col='blue', lwd=3)
legend(1986, 350, legend=c("Logistic low prediction", "Logistic mid prediction", 'Logistic high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()

# Poland Case
data_poland = data[data['country']=='Poland',]
mask_has = data_poland['wind_electricity']>0
mask_na = is.na(data_poland['wind_electricity'])
data_poland_wind = data_poland[!mask_na & mask_has, model_col]

# add end data so that model doesn't give error message
newdata = data.frame(year = 2070, country='Poland',wind_electricity=66)
data_poland_wind = rbind(data_poland_wind, newdata)

y.low <- nls(wind_electricity ~ SSlogis(year, Asym, xmid, scal), data=data_poland_wind)
summary(y.low)
Asym = coef(y.low)[1]
xmid.mid = coef(y.low)[2]-1.3
scal.mid = coef(y.low)[3]/1.3
x = c(2022:2080)
y.mid = Asym/(1+exp((xmid.mid-x)/scal.mid))

xmid.high = coef(y.low)[2]-2
scal.high = coef(y.low)[3]/2
y.high = Asym/(1+exp((xmid.high-x)/scal.high))

png(file = paste(getwd(),'/Visualizations/logistic-model-poland.png', sep=""), width=600, height=600)
plot(wind_electricity~year, data_poland_wind,
     xlab='Year', xlim = range(2000,2070),
     ylab='Electricity generation from wind, TWh', ylim = range(0, 70),
     main='Projections of Electricity Generation from wind, Poland')

curve(predict(y.low, newdata = data.frame(year=x)), add=TRUE, col='green', lty = 2, lwd=2)
lines(x,y.mid, col='orange', lty = 2, lwd=2)
lines(x,y.high, col='red', lty = 2, lwd=2)
abline(h=67, col='purple', lwd=3)
abline(v=2035, col='blue', lwd=3)
legend(2040, 20, legend=c("Logistic low prediction", "Logistic mid prediction", 'Logistic high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()