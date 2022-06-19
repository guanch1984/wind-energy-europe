rm(list = ls())
# code book
# https://github.com/owid/energy-data/blob/master/owid-energy-codebook.csv
library(rstudioapi)
library(tidyverse)
library(car)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

data = read.csv(file = paste(getwd(),'/data/owid-energy-data.csv',sep=""), head=TRUE, sep=',')
# 15 years projection
future_year = data.frame('year'=c(2022:2036))
model_col = c('year', 'country', 'wind_electricity', 'gdp', 'population')
# Columns used for analysis
# year
# country
# wind_electricity
# wind_elec_per_capita
# gdp
# population

# Germany Case
data_germany = data[data['country']=='Germany',]
# Electricity generation from wind, measured in terawatt-hours
mask_has = data_germany['wind_electricity']>0
mask_na = is.na(data_germany['wind_electricity'])

data_germany_wind = data_germany[!mask_na & mask_has, model_col]

model1 = lm(wind_electricity~year, data_germany_wind)
bc.model1 = boxCox(model1)
lambda.model1 = bc.model1$x[which.max(bc.model1$y)]

model2 = lm((wind_electricity**lambda.model1-1)/lambda.model1~year, data_germany_wind)
par(mfrow=c(2,2))
plot(model2)
summary(model2)

y = (data_germany_wind[,'wind_electricity']**lambda.model1-1)/lambda.model1
plot(data_germany_wind[,'year'], y, xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model1),
     main='SLR of Electricity generation from wind, Germany')
abline(model2, col='orange', lty=2, lwd=2)


model2_pred = (predict(model2, future_year) * lambda.model1 + 1) ** (1/lambda.model1) 
model2_year_pred = data.frame(matrix(ncol = length(data_germany_wind), nrow = dim(future_year[1])))
colnames(model2_year_pred) = colnames(data_germany_wind)
model2_year_pred['year'] = future_year
model2_year_pred['wind_electricity'] = model2_pred
model2_total = rbind(data_germany_wind, model2_year_pred)
plot(model2_total[,'year'], model2_total[,'wind_electricity'], xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model1),
     main='Pre conflict projection of Electricity from wind, Germany ')

# fill up missing gdp data
data_germany_wind[data_germany_wind['year']==2019, 'gdp'] = 3.885961e+12 * (1+0.006)
data_germany_wind[data_germany_wind['year']==2020, 'gdp'] = 3.885961e+12 * (1+0.006)*(1-0.046)
data_germany_wind[data_germany_wind['year']==2021, 'gdp'] = 3.885961e+12 * (1+0.006)*(1-0.046)*(1+0.029)
cor(data_germany_wind[,c[1,3,4,5]])
# year gdp and population are highly correlated

# Poland Case
data_poland = data[data['country']=='Poland',]
# Electricity generation from wind, measured in terawatt-hours
mask_has = data_poland['wind_electricity']>0
mask_na = is.na(data_poland['wind_electricity'])

data_poland_wind = data_poland[!mask_na & mask_has, model_col]

model3 = lm(wind_electricity~year, data_poland_wind)
bc.model3 = boxCox(model3)
lambda.model3 = bc.model3$x[which.max(bc.model3$y)]

model4 = lm((wind_electricity**lambda.model3-1)/lambda.model3~year, data_poland_wind)
par(mfrow=c(2,2))
plot(model4)
summary(model4)

y = (data_poland_wind[,'wind_electricity']**lambda.model3-1)/lambda.model3
plot(data_poland_wind[,'year'], y, xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model3),
     main='SLR of Electricity generation from wind, Poland')
abline(model4, col='orange', lty=2, lwd=2)

model4_pred = (predict(model4, future_year) * lambda.model3 + 1) ** (1/lambda.model3) 
model4_year_pred = data.frame(matrix(ncol = length(data_poland_wind), nrow = dim(future_year[1])))
colnames(model4_year_pred) = colnames(data_poland_wind)
model4_year_pred['year'] = future_year
model4_year_pred['wind_electricity'] = model4_pred
model4_total = rbind(data_poland_wind, model4_year_pred)
plot(model4_total[,'year'], model4_total[,'wind_electricity'], xlab='Year', 
     ylab=cbind('BoxCox Lambda=',lambda.model3),
     main='Pre conflict projection of Electricity from wind, Poland')


plot(model2_total[, 'year'], model2_total[,'wind_electricity'], col='orange', 
     xlab='Year', 
     ylab='Electricity from wind, measured in terawatt-hours',
     main='Wind electricity development, Poland 15 years behind Germany')
lines(model2_total[,'year'], model2_total[,'wind_electricity'], col='orange', lty=1, lwd=2)
points(model4_total[,'year']-15, model4_total[,'wind_electricity'], col='red')
lines(model4_total[,'year']-15, model4_total[,'wind_electricity'], col='red', lty=2, lwd=2)
legend(1986, 400, legend=c("Germany", "Poland"),
       col=c("orange", "red"), lty=1:2, lwd=2:2, cex=0.8)














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




# financial
fin_data = read.csv(file = paste(getwd(),'/data/financial.csv',sep=""), head=TRUE, sep=',')
colnames(fin_data) = c('Year', 'Company', 'Revenue', 'P.L', 'mW.Sold')
mask_v = fin_data['Company']=='Vestas'
fin_vestas = fin_data[mask_v,]
fin_vestas['Revenue'] = as.numeric(unlist(fin_vestas['Revenue']))
fin_vestas['P.L'] = as.numeric(unlist(fin_vestas['P.L']))
fin_vestas['mW.Sold'] = as.numeric(unlist(fin_vestas['mW.Sold']))

mask_2007 = data_poland_wind['year']>=2007
# correlation between poland electricity and vestas mW sold number
cor(data_poland_wind[mask_2007,'wind_electricity'],fin_vestas[,'mW.Sold'])
cor(data_poland_wind[mask_2007,'wind_electricity'],fin_vestas[,'Revenue'])
54.05**