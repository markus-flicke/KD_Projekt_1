PlotDistributionAnalysis = function(d, t = 'Data', folder = 'DistributionAnalysis', p = getwd()){
  library(pareto)
  library(DataVisualizations)
  library(dbt.Statistics)
  library(AdaptGauss)
  library(dbt.Plot)

  
  #INPUT
  # d: Daten 
  #OPTIONAL
  # folder: Name des Ordners in dem die bilder gespechert werden.
  # path: Pfad, an dem der Ordner angelegt wird.
  # t: Titel der Plots
  
  #Erstellt die Bilder (.png) von: PDEplot, histopt, dbt_qqnorm, boxplot
  
  base <- paste(getwd(), "/Plots/", folder, "/", t, sep="")
  #dir.create(base)

  #PDEPlot
  png(paste(base, "/", t, "_PDEplot.png", sep=""))
  PDEplot(d, title = t)
  dev.off()
  
  #histopt
  png(paste(base, "/", t, "_histopt.png", sep=""))
  histopt(d, Title = t)
  dev.off();
  
  #dbt_qqnorm
  png(paste(base,  "/", t, "_dbt_qqnorm.png", sep=""));
  dbt_qqnorm(d, Name = t);
  dev.off();
  
  #boxplot
  png(paste(base,  "/", t, "_boxplot.png", sep=""));
  boxplot(d, names = t);
  dev.off();
  
  #setwd(currentDirectory);
}#end function PDA