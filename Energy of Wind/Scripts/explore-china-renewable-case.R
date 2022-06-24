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

# columns I am interested in, that might affect wind energy
col.factor = c('country', 'year',
# 'electricity_demand',
'electricity_generation',
'fossil_electricity',
'renewables_electricity',
'coal_electricity',
'oil_electricity',
'gas_electricity',
'hydro_electricity',
'nuclear_electricity',
'solar_electricity',
'wind_electricity',
# 'biofuel_electricity',
'population',
'gdp')

data_china_wind = data_china[!mask_na & mask_has, col.factor]
# fill in missing data
data_china_wind[data_china_wind['year']==2019, 'gdp'] = 14279937467431
data_china_wind[data_china_wind['year']==2020, 'gdp'] = 14722730697890.1
data_china_wind[data_china_wind['year']==2021, 'gdp'] = 14722730697890.1 * 1.081
data_china_wind[data_china_wind['year']==2021, 'renewables_electricity'] = 2480
data_china_wind
cor(data_china_wind[,2:ncol(data_china_wind)])

model.slr = lm(wind_electricity~year, data_china_wind)
bc.model.slr = boxCox(model.slr)
lambda.model.slr = bc.model.slr$x[which.max(bc.model.slr$y)]

model.slr.tran = lm((wind_electricity**lambda.model.slr-1)/lambda.model.slr~year, data_china_wind)
par(mfrow=c(2,2))
plot(model.slr.tran)
summary(model.slr.tran)

hist(model.slr.tran$residuals)

y = (data_china_wind[,'wind_electricity']**lambda.model.slr-1)/lambda.model.slr
plot(data_china_wind[,'year'], y, xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model.slr),
     main='SLR of Electricity generation from wind, China')
abline(model.slr.tran, col='orange', lty=2, lwd=2)

model2 = lm(wind_electricity~year+oil_electricity+gdp, data_china_wind)
bc.model2 = boxCox(model2)
lambda.model2 = bc.model.slr$x[which.max(bc.model2$y)]
model2.tran = lm((wind_electricity**lambda.model2-1)/lambda.model2~year+oil_electricity+gdp, data_china_wind)
par(mfrow=c(2,2))
plot(model2.tran)
summary(model2.tran)
hist(model2.tran$residuals)
