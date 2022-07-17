rm(list = ls())

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
future_year = data.frame('year'=c(2022:2036))
data_germany = data[data['country']=='Germany',]
data_poland = data[data['country']=='Poland',]
data_china = data[data['country']=='China',]

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
png(file='0-poland-slr-bc.png', height = 500, width = 500)
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

