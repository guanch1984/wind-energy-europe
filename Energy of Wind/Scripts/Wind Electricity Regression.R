rm(list=ls())
set.seed(2030)

data <- read.csv("/Users/FGasta/Documents/GitHub/Team-27/Energy of Wind/Scripts/data/owid-energy-data.csv")
library(tidyverse)
library(MASS)
library(dplyr)
library(forecast)

germany <- data %>% filter(country == "Germany") %>% filter(wind_electricity != 0)
head(germany)
germany[is.na(germany)] <- 0
germany <- germany[c('year','wind_electricity')]
germany

poland <- data %>% filter(country == "Poland") %>% filter(wind_electricity != 0)
head(poland)
poland[is.na(poland)] <- 0
poland <- poland[c('year','wind_electricity')]
poland

#SLR Germany
germany_model <- lm(wind_electricity~year,germany)
summary(germany_model)
plot(germany_model)

testyears <- matrix(c(2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036),nrow=15)
testyears
windelectricity <- matrix(data=NA,nrow=15)
windelectricity

testmatrix <- cbind(testyears,windelectricity)
colnames(testmatrix) <- c("year","wind_electricity")
testmatrix <- as.data.frame(testmatrix)
testmatrix

slrprediction <- as.matrix(predict(germany_model,testmatrix))
slrgermanyprediction <- cbind(testyears,slrprediction)
colnames(slrgermanyprediction) <- c("year","wind_electricity")
slrfullgermany <- rbind(germany,slrgermanyprediction)
plot(slrfullgermany)
slrfullgermany


#Box Cox Transformed SLR Germany
bc <- boxcox(germany$wind_electricity~germany$year)
lambda <- bc$x[which.max(bc$y)]
lambda

boxgermany_model <- lm(((wind_electricity^lambda-1)/lambda)~year,germany)
summary(boxgermany_model)
plot(boxgermany_model)

boxprediction <- as.matrix(predict(boxgermany_model,testmatrix))
boxgermanyprediction <- cbind(testyears,boxprediction)
colnames(boxgermanyprediction) <- c("year","wind_electricity")
boxfullgermany <- rbind(germany,boxgermanyprediction)
plot(boxfullgermany)
boxfullgermany



#SLR Poland
poland_model <- lm(wind_electricity~year,poland)
summary(poland_model)
plot(poland_model)

testyears <- matrix(c(2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036),nrow=15)
testyears
windelectricity <- matrix(data=NA,nrow=15)
windelectricity

testmatrix <- cbind(testyears,windelectricity)
colnames(testmatrix) <- c("year","wind_electricity")
testmatrix <- as.data.frame(testmatrix)
testmatrix

slrprediction <- as.matrix(predict(poland_model,testmatrix))
slrpolandprediction <- cbind(testyears,slrprediction)
colnames(slrpolandprediction) <- c("year","wind_electricity")
slrfullpoland <- rbind(poland,slrpolandprediction)
plot(slrfullpoland)
slrfullpoland


#Box Cox Transformed SLR Poland
bc <- boxcox(poland$wind_electricity~poland$year)
lambda <- bc$x[which.max(bc$y)]
lambda

boxpoland_model <- lm(((wind_electricity^lambda-1)/lambda)~year,poland)
summary(boxpoland_model)
plot(boxpoland_model)

boxprediction <- as.matrix(predict(boxpoland_model,testmatrix))
boxpolandprediction <- cbind(testyears,boxprediction)
colnames(boxpolandprediction) <- c("year","wind_electricity")
boxfullpoland <- rbind(poland,boxpolandprediction)
plot(boxfullpoland)
boxfullpoland






