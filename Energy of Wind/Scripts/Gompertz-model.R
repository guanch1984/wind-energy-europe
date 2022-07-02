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

# Gompertz model

data.g = data.frame(cbind(data_germany_wind$year-1986, data_germany_wind$wind_electricity))
germany.gomp <- nls(X2 ~ SSgompertz(X1, Asym, xmid, scal), data=data.g)
summary(germany.gomp)
Asym = coef(germany.gomp)[1]
xmid = coef(germany.gomp)[2]
scal.low = coef(germany.gomp)[3]
scal.mid = coef(germany.gomp)[3]/1.04
scal.high = coef(germany.gomp)[3]/1.1
x = c(1986:2080)-1986
y.low = Asym*exp(-xmid*scal.low^x)
y.mid = Asym*exp(-xmid*scal.mid^x)
y.high = Asym*exp(-xmid*scal.high^x)

setwd('..')
png(file = paste(getwd(),'/Visualizations/Gompertz-model-germany.png', sep=""), width=600, height=600)

plot(wind_electricity~year, data_germany_wind,
     xlab='Year', xlim = range(1985,2060),
     ylab='Electricity generation from wind, TWh', ylim = range(0, 400),
     main='Projections of Electricity Generation from wind, Germany')
lines(x+1986,y.low, col='green', lty = 2, lwd=2)
lines(x+2001,y.mid, col='orange', lty = 2, lwd=2)
lines(x+2009,y.high, col='red', lty = 2, lwd=2)
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

data.p = data.frame(cbind(data_poland_wind$year-2001, data_poland_wind$wind_electricity))
poland.gomp <- nls(X2 ~ SSgompertz(X1, Asym, xmid, scal), data=data.p)
summary(poland.gomp)
Asym = coef(poland.gomp)[1]
xmid = coef(poland.gomp)[2]
scal.low = coef(poland.gomp)[3]
scal.mid = coef(poland.gomp)[3]/1.04
scal.high = coef(poland.gomp)[3]/1.1
x = c(2001:2080)-2001
y.low = Asym*exp(-xmid*scal.low^x)
y.mid = Asym*exp(-xmid*scal.mid^x)
y.high = Asym*exp(-xmid*scal.high^x)


png(file = paste(getwd(),'/Visualizations/Gompertz-model-poland.png', sep=""), width=600, height=600)

plot(wind_electricity~year, data_poland_wind,
     xlab='Year', xlim = range(2000,2070),
     ylab='Electricity generation from wind, TWh', ylim = range(0, 70),
     main='Projections of Electricity Generation from wind, Poland')
lines(x+2001,y.low, col='green', lty = 2, lwd=2)
lines(x+2008,y.mid, col='orange', lty = 2, lwd=2)
lines(x+2012,y.high, col='red', lty = 2, lwd=2)
abline(h=67, col='purple', lwd=3)
abline(v=2035, col='blue', lwd=3)
legend(2040, 10, legend=c("Logistic low prediction", "Logistic mid prediction", 'Logistic high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()
