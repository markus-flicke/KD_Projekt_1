library(dbt.DataIO)
library(DbtFunc)
library(pareto)
library(AdaptGauss)
#setwd(paste(getwd(),"/../", sep =""))

aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- healthy_raw$Data
header <- healthy_raw$Header

scale_adjusted <- apply(data, 2, function (col) {col / max(col)})

for (i in 1:length(header)){
  PlotDistributionAnalysis(data[,i], header[i], 'Healthy')
}
