library(dbt.DataIO)
library(DbtFunc)
library(pareto)
library(AdaptGauss)

aml_raw <- ReadLRN('../Data/13AMLKHd10')
healthy_raw <- ReadLRN('../Data/13PKd10')

data <- aml_raw$Data
header <- aml_raw$Header

scale_adjusted <- apply(data, 2, function (col) {col / max(col)})

for (i in 1:length(header)){
  func(data[,i], header[i])
}