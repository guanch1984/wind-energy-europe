rm(list = ls())

library(tidyverse)

data = read.csv(file = 'data/financial.csv', head=TRUE, sep=',')
colnames(data) = c('Year', 'Company', 'Revenue', 'P.L', 'mW.Sold')
mask_v = data['Company']=='Vestas'
mask_g = data['Company']=='Gamesa'
col_sel = c('Year', 'Revenue')
vestas_rev = data[mask_v,col_sel]
gamesa_rev = data[mask_g,col_sel]
data
