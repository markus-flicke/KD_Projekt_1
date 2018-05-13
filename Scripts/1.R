library(dbt.DataIO)
library(DbtFunc)
library(pareto)
library(AdaptGauss)

library(DataVisualizations)
library(dbt.Statistics)

library(dbt.Plot)
#setwd(paste(getwd(),"/../", sep =""))

aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- aml_raw$Data
header <- aml_raw$Header

scale_adjusted <- apply(data, 2, function (col) {col / max(col)})
logtransformed <- apply(data, 2, function (x) {log(x+1)})

roottransformed <- apply(data, 2, function (x) {x^0.5})

dbt_qqnorm(logtransformed[,5])
header
