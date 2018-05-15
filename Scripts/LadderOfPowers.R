library(pareto)
library(DataVisualizations)
library(dbt.Statistics)
library(AdaptGauss)
library(dbt.Plot)
library(dbt.DataIO)
library(DbtFunc)

PlotLadderOfPowers = function(d, relativePath = '', cdx = ''){
  #INPUT
  # d: Untransformierte Daten 
  #OPTIONAL
  # relativePath: path to save the plot, wd if empty
  # cdx: Name oor variable, only used for saving (could infer from relative path if we stick to our conventions)
  
  # Erstellt Bilder von QQplots vor und nach Transformationen
  
  png(paste(relativePath, "/", cdx, "_LadderOfPowers.png", sep=""))
  par(mfrow=c(2,2))
  
  #Original
  dbt_qqnorm(d, Name = 'Original')
  
  #Logged
  trans <- sapply(d, function (x) {log(x+1)})
  dbt_qqnorm(trans, Name = 'Log(x+1)')
  title(main="█████████████████████", col.main ="white")
  title(main = 'Logged Transform')
  
  #Rooted
  trans <- sapply(d, function (x) {x^0.5})
  dbt_qqnorm(trans, Name = 'x^0.5')
  title(main="█████████████████████", col.main ="white")
  title(main = 'Rooted Transform')
  
  #Squared
  trans <- sapply(d, function (x) {x^2})
  dbt_qqnorm(trans, Name = 'x^2')
  title(main="█████████████████████", col.main ="white")
  title(main = 'Squared Transform')
  
  dev.off()
}

#WARNING: You may have to manually set the working directory to KD_Projekt_1

aml_raw <- ReadLRN('./Data/13AMLKHd10')
healthy_raw <- ReadLRN('./Data/13PKd10')

data <- aml_raw$Data
header <- aml_raw$Header

for (i in 1:length(header)){
  PlotLadderOfPowers(data[,i], paste(getwd(), "/Plots/", 'AML', "/", header[i], sep=""), header[i])
}

