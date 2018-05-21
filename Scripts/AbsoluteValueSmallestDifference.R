library(pareto)
library(DataVisualizations)
library(dbt.Statistics)
library(AdaptGauss)
library(dbt.Plot)
library(dbt.DataIO)
library(DbtFunc)

#setwd('/users/m/uni/kd/kd_projekt_1')
aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- healthy_raw$Data
header <- healthy_raw$Header

minDifference <- function(v){
  sorted <- sort(v)
  differences <- diff(sorted, lag = 1)
  return(min(differences[differences>0]))
}
apply(data, 2, minDifference)

par(mfcol=c(4,length(header)/2), mar=c(0,1,1,0))
counter = 1
for (h in header){
  sorted <- sort(data[,counter])
  differences <- diff(sorted, lag = 1)
  plot(sorted, yaxt = 'n', xaxt='n')
  title(h, ylab='sorted values',line=0)
  plot(differences, yaxt = 'n', xaxt='n', pch="x")
  title('', ylab='neighbor difference',line=0)
  counter = counter + 1
}

print('done')

