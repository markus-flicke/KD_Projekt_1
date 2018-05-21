library(pareto)
library(DataVisualizations)
library(dbt.Statistics)
library(AdaptGauss)
library(dbt.Plot)
library(dbt.DataIO)
library(DbtFunc)

setwd('/users/m/uni/kd/kd_projekt_1')
aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- healthy_raw$Data
header <- healthy_raw$Header

sum(is.na(data))

