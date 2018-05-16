library(pareto)
library(DataVisualizations)
library(dbt.Statistics)
library(AdaptGauss)
library(dbt.Plot)
library(dbt.DataIO)
library(DbtFunc)
# Semi Manual way to do a Gauss Mixture Model using AdaptGauss. 
# The below also plots a qqplot vs our model and does a chi2test. 


setwd('/users/m/uni/kd/kd_projekt_1')
aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- aml_raw$Data
header <- aml_raw$Header

attribute_id = 2
dir <- paste(getwd(),"/Plots/Healthy/", header[attribute_id],"/",header[attribute_id],"_", sep="")

png(paste(dir, "GaussMM.png", sep=""))
adaptValues <- AdaptGauss(data[,header[attribute_id]])
dev.off()



png(paste(dir, "QQ_vs_GaussMM.png", sep=""))
AdaptGauss::QQplotGMM(data[,attribute_id], adaptValues$Means, adaptValues$SDs, adaptValues$Weights)
dev.off()

chi2Test <- AdaptGauss::Chi2testMixtures(data[,attribute_id], adaptValues$Means, adaptValues$SDs, adaptValues$Weights)
write.table(chi2Test, paste(dir, "chi2Test", sep=""))
write.table(adaptValues, paste(dir, "GMM", sep=""))

chi2Test
