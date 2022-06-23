rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(car)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

data = read.csv(file = paste(getwd(),'/data/owid-energy-data.csv',sep=""), head=TRUE, sep=',')

# 15 years projection
future_year = data.frame('year'=c(2022:2036))

# China Case
data_china = data[data['country']=='China',]
# Electricity generation from wind, measured in terawatt-hours
mask_has = data_china['wind_electricity']>0
mask_na = is.na(data_china['wind_electricity'])

data_china_wind = data_china[!mask_na & mask_has, model_col]

model5 = lm(wind_electricity~year, data_china_wind)
bc.model5 = boxCox(model5)
lambda.model5 = bc.model5$x[which.max(bc.model5$y)]

model6 = lm((wind_electricity**lambda.model5-1)/lambda.model5~year, data_china_wind)
par(mfrow=c(2,2))
plot(model6)
summary(model6)

y = (data_china_wind[,'wind_electricity']**lambda.model5-1)/lambda.model5
plot(data_china_wind[,'year'], y, xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model5),
     main='SLR of Electricity generation from wind, China')
abline(model6, col='orange', lty=2, lwd=2)