rm(list = ls())
dev.off()

# code book
# https://github.com/owid/energy-data/blob/master/owid-energy-codebook.csv
library(rstudioapi)
library(tidyverse)
library(car)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

data = read.csv(file = paste(getwd(),'/data/0-wind-data.csv',sep=""), head=TRUE, sep=',')
colnames(data)=c('year', 'country', 'installed_cap_MW', 'generation_GWh', 'capacity_factor')
ggplot(data, aes(x = year, y = installed_cap_MW/1000, group=country)) +
  geom_line(aes(color=country)) +
  geom_point(aes(color=country)) + 
  ylim(0, 70) +
  labs(title = '1990-2021 Wind Installed Capacity in 
  Germany, Poland, China measured in (GW)', x='Year', y= 'Installed Capacity in (GW)') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file='0-installed_cap.png',height = 10, width = 10)

# 15 years projection
future_year = data.frame('year'=c(2022:2050))
data_germany = data[data['country']=='Germany',1:3]
data_poland = data[data['country']=='Poland',1:3]
data_china = data[data['country']=='China',1:3]

# Germany Case
model.germany.slr1 = lm(installed_cap_MW~year, data_germany)
bc.model.germany.slr1 = boxCox(model.germany.slr1)
lambda.bc.model.germany.slr1 = bc.model.germany.slr1$x[which.max(bc.model.germany.slr1$y)]
# lambda.bc.model.germany.slr1 = 0.5 , using sqrt transformation
model.germany.slr2 = lm(sqrt(installed_cap_MW)~year, data_germany)
png(file='0-germany-slr-bc.png', height = 500, width = 500)
par(mfrow=c(2,2))
plot(model.germany.slr2)
dev.off()
summary(model.germany.slr2)
# found S shape residual system error

model.germany.slr2.pred = predict(model.germany.slr2, future_year)**2
png(file='0-germany-slr-projection.png', height = 500, width = 500)
plot(c(data_germany[,'year'], future_year[,'year']),
     c(data_germany[,'installed_cap_MW'], model.germany.slr2.pred)/1000,
     xlab='Year', 
     ylab=cbind('BoxCox Lambda=', '0.5'),
     main='Pre conflict projection of wind installed capacity GW , Germany ')
dev.off()

# Poland Case
model.poland.slr1 = lm(installed_cap_MW~year, data_poland)
bc.model.poland.slr1 = boxCox(model.poland.slr1)
lambda.bc.model.poland.slr1 = bc.model.poland.slr1$x[which.max(bc.model.poland.slr1$y)]
# lambda.bc.model.poland.slr1 = 0.3838384 , using sqrt transformation
model.poland.slr2 = lm(sqrt(installed_cap_MW)~year, data_poland)
png(file='0-poland-slr-bc.png', height = 500, width = 500)
par(mfrow=c(2,2))
plot(model.poland.slr2)
dev.off()
summary(model.poland.slr2)
# found S shape residual system error

model.poland.slr2.pred = predict(model.poland.slr2, future_year)**2
png(file='0-poland-slr-projection.png', height = 500, width = 500)
plot(c(data_poland[,'year'], future_year[,'year']),
     c(data_poland[,'installed_cap_MW'], model.poland.slr2.pred)/1000,
     xlab='Year', 
     ylab=cbind('BoxCox Lambda=', '0.5'),
     main='Pre conflict projection of wind installed capacity GW , Poland ')
dev.off()

# China Case
model.china.slr1 = lm(installed_cap_MW~year, data_china)
bc.model.china.slr1 = boxCox(model.china.slr1)
lambda.bc.model.china.slr1 = bc.model.china.slr1$x[which.max(bc.model.china.slr1$y)]
# lambda.bc.model.china.slr1 = 0.5 , using sqrt transformation
model.china.slr2 = lm(sqrt(installed_cap_MW)~year, data_china)
png(file='0-china-slr-bc.png', height = 500, width = 500)
par(mfrow=c(2,2))
plot(model.china.slr2)
dev.off()
summary(model.china.slr2)
# found S shape residual system error

model.china.slr2.pred = predict(model.china.slr2, future_year)**2
png(file='0-china-slr-projection.png', height = 500, width = 500)
plot(c(data_china[,'year'], future_year[,'year']),
     c(data_china[,'installed_cap_MW'], model.china.slr2.pred)/1000,
     xlab='Year', 
     ylab=cbind('BoxCox Lambda=', '0.5'),
     main='Pre conflict projection of wind installed capacity GW , China ')
dev.off()


# max capacity point
max.germany = data.frame(year = 2055, country='Germany',installed_cap_MW=185000)
data_germany = rbind(data_germany, max.germany)
max.poland = data.frame(year = 2070, country='Poland',installed_cap_MW=65000)
data_poland = rbind(data_poland, max.poland)
max.china = data.frame(year = 2050, country='China',installed_cap_MW=2300000)
data_china = rbind(data_poland, max.china)


#  Gompertz Model
data.g = data.frame(cbind(data_germany$year-1990, data_germany$installed_cap_MW))
germany.gomp <- nls(X2 ~ SSgompertz(X1, Asym, xmid, scal), data=data.g)
summary(germany.gomp)
Asym = coef(germany.gomp)[1]
xmid = coef(germany.gomp)[2]
scal.low = coef(germany.gomp)[3]
scal.mid = coef(germany.gomp)[3]/1.04
scal.high = coef(germany.gomp)[3]/1.1
x = c(1990:2080)-1990
y.low = Asym*exp(-xmid*scal.low^x)/1000
y.mid = Asym*exp(-xmid*scal.mid^x)/1000
y.high = Asym*exp(-xmid*scal.high^x)/1000

png(file='0-germany-gompertz-projection.png', height = 500, width = 500)
plot(data_germany$year, data_germany$installed_cap_MW/1000,
     xlab='Year', xlim = range(1990,2060),
     ylab='Installed Capacity in (GW)', ylim = range(0, 250),
     main='Gompertz model projection of wind capacity, Germany')
lines(x+1990,y.low, col='green', lty = 2, lwd=2)
lines(x[-(1:16)]+2004,y.mid[-(1:16)], col='orange', lty = 2, lwd=2)
lines(x[-(1:10)]+2010,y.high[-(1:10)], col='red', lty = 2, lwd=2)
abline(h=189, col='purple', lwd=3)
legend(1990, 180, legend=c("Gompertz low prediction", "Gompertz mid prediction", 'Gompertz high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()

data.p = data.frame(cbind(data_poland$year-2002, data_poland$installed_cap_MW))
poland.gomp <- nls(X2 ~ SSgompertz(X1, Asym, xmid, scal), data=data.p)
summary(poland.gomp)
Asym = coef(poland.gomp)[1]
xmid = coef(poland.gomp)[2]
scal.low = coef(poland.gomp)[3]
scal.mid = coef(poland.gomp)[3]/1.04
scal.high = coef(poland.gomp)[3]/1.1
x = c(2002:2080)-2002
y.low = Asym*exp(-xmid*scal.low^x)/1000
y.mid = Asym*exp(-xmid*scal.mid^x)/1000
y.high = Asym*exp(-xmid*scal.high^x)/1000

png(file='0-poland-gompertz-projection.png', height = 500, width = 500)
plot(data_poland$year, data_poland$installed_cap_MW/1000,
     xlab='Year', xlim = range(2000,2080),
     ylab='Installed Capacity in (GW)', ylim = range(0, 100),
     main='Gompertz model projection of wind capacity, Poland')
lines(x+2002,y.low, col='green', lty = 2, lwd=2)
lines(x[-(1:10)]+2011,y.mid[-(1:10)], col='orange', lty = 2, lwd=2)
lines(x[-(1:7)]+2015,y.high[-(1:7)], col='red', lty = 2, lwd=2)
abline(h=67, col='purple', lwd=3)
legend(2040, 20, legend=c("Gompertz low prediction", "Gompertz mid prediction", 'Gompertz high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()


# Logistic model, Asym/(1+exp((xmid-input)/scal))
# Germany
y.low <- nls(installed_cap_MW ~ SSlogis(year, Asym, xmid, scal), data=data_germany)
summary(y.low)
Asym = coef(y.low)[1]
xmid.mid = coef(y.low)[2]-1.3
scal.mid = coef(y.low)[3]/1.3
x = c(2020:2080)
y.mid = Asym/(1+exp((xmid.mid-x)/scal.mid))/1000

xmid.high = coef(y.low)[2]-2
scal.high = coef(y.low)[3]/2
y.high = Asym/(1+exp((xmid.high-x)/scal.high))/1000

png(file = '0-germany-logistic-projection.png', width=500, height=500)
plot(data_germany$year, data_germany$installed_cap_MW/1000,
     xlab='Year', xlim = range(1990,2060),
     ylab='Installed Capacity in (GW)', ylim = range(0, 250),
     main='Logistic model projection of wind capacity, Germany')
curve(predict(y.low, newdata = data.frame(year=x))/1000, add=TRUE, col='green', lty = 2, lwd=2)
lines(x,y.mid, col='orange', lty = 2, lwd=2)
lines(x,y.high, col='red', lty = 2, lwd=2)
abline(h=189, col='purple', lwd=3)
legend(1990, 180, legend=c("Logistic low prediction", "Logistic mid prediction", 'Logistic high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()

# Poland
y.low <- nls(installed_cap_MW ~ SSlogis(year, Asym, xmid, scal), data=data_poland)
summary(y.low)
Asym = coef(y.low)[1]
xmid.mid = coef(y.low)[2]-4
scal.mid = coef(y.low)[3]/1.5
x = c(2020:2080)
y.mid = Asym/(1+exp((xmid.mid-x)/scal.mid))/1000

xmid.high = coef(y.low)[2]-6
scal.high = coef(y.low)[3]/2
y.high = Asym/(1+exp((xmid.high-x)/scal.high))/1000

png(file = '0-poland-logistic-projection.png', width=500, height=500)
plot(data_poland$year, data_poland$installed_cap_MW/1000,
     xlab='Year', xlim = range(2000,2080),
     ylab='Installed Capacity in (GW)', ylim = range(0, 70),
     main='Logistic model projection of wind capacity, Poland')
curve(predict(y.low, newdata = data.frame(year=x))/1000, add=TRUE, col='green', lty = 2, lwd=2)
lines(x,y.mid, col='orange', lty = 2, lwd=2)
lines(x,y.high, col='red', lty = 2, lwd=2)
abline(h=67, col='purple', lwd=3)
legend(2040, 20, legend=c("Logistic low prediction", "Logistic mid prediction", 'Logistic high prediction'),
       col=rep(c('green', 'orange', 'red'),1), lty=rep(2,3), cex=0.8)
dev.off()


# Financial analysis
