rm(list = ls())

library(tidyverse)

vestas =read.csv(file = 'data/VWDRY-stock-price.csv', head=TRUE, sep=',')

col = c('Date', 'Adj.Close')
mask_vestas = grepl('01-01', vestas[,'Date'])
vestas_annual = vestas[mask_vestas, col]

ggplot(data = vestas, aes(x = as.Date(Date), y = Adj.Close, group=1)) +
  geom_line(col='black') +
  geom_point(col='orange') +
  # geom_smooth(method="glm", method.args=list(family=gaussian(link="log")), color = "coral1")+
  # geom_smooth(method="lm", color = "cyan", lwd = .5, linetype = 'dotdash')
  scale_x_date(date_breaks = "years") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x = "Date",
       y = "Price",
       title = "Vestas Stock Price from 2007-2022")

gamesa = read.csv(file = 'data/GCTAY-stock-price.csv', head=TRUE, sep=',')

mask_gamesa = grepl('01-01', gamesa[,'Date'])
gamesa_annual = gamesa[mask_gamesa, col]

ggplot(data = gamesa, aes(x = as.Date(Date), y = Adj.Close, group=1)) +
  geom_line(col='black') +
  geom_point(col='orange') +
  # geom_smooth(method="glm", method.args=list(family=gaussian(link="log")), color = "coral1")+
  # geom_smooth(method="lm", color = "cyan", lwd = .5, linetype = 'dotdash')
  scale_x_date(date_breaks = "years") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x = "Date",
       y = "Price",
       title = "Siemens-Gamesa Stock Price from 2010-2022")
