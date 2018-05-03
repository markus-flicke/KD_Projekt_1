plotMeasureRAAR=function(Raar,label='ProjectionMethod',gPlotList=list(RAARplot=ggplot()),LineType="solid",Shape=16,PointsPerE=10,fancy=F){
  #plotMeasureTundD(TDmatrix,label='ProjectionMethod',color='blue',gPlot=ggplot())
  #Plottet T und D von Venna/Kaski2001 als Kurvenverlauf ueber die ersten 50k
  # INPUT
  # Raar[1:kmax]   		Output von RAAR() eines Projektionsverfahrens
  # label								string, Bennenung der  Kurve
  # Optional
  # gPlotList         weiteren plotMeasureTundD uebergeben zum uewbereinander zeichnen
  # LineTypeq         linetyp als string
  # Shape             punktart als integer
  # PointsPerE        Abstand zwischen markierungspunkten auf linie, integer
  # Output
  #Ein ggPlot
  #author: MT 01/2016
  #Example
  #   x=plotMeasureTundD(TundD[[1]],label=names[1])
  #   for(i in 2:length(names))
  #     x=plotMeasureTundD(TundD[[i]],label=names[i],gPlotList=x)
  
  knn=1:length(Raar)
  
  auc=vector()
    r=100*Raar
    r2=0
    normierung=vector()
    for(k in 1:length(Raar)){
      r2=r2+r[k]/k
    }
    auc=r2/sum(1/c(1:length(Raar)))
    if(!fancy)
      label=paste(label,'AUC =',round(auc,3))
  df = data.frame("KNN" = knn, "Raar" = r,"Projection"=factor(label))
  #return(df)

  ind=seq(from=1,to = length(knn),by=PointsPerE)
  dfpoints= data.frame(
    "KNN" = knn[ind],
    "Raar" = r[ind] ,
    "Projection" = factor(label)
  )#Siehe Readme in Doku vo NeRV
  #return(df)
  
  plt1 <- gPlotList$RAARplot + geom_line(data = df, aes_string(x = "KNN", y ="Raar", colour = "Projection"),size=2,linetype=LineType) +
  geom_point(
      data = dfpoints,
      aes_string(x = "KNN", y = "Raar", colour = "Projection"),
      size = 5.5,
      shape=Shape,
      show.legend = FALSE,
    )+
    ylab('RAAR in %')+coord_trans(x="log")#+scale_y_continuous(limits=c(0, 100))
  if(fancy)
    plt1=plt1+theme(panel.background = element_blank(), legend.key = element_blank(),axis.line =element_line(colour='black'),
           axis.title.y = element_text(size = rel(2), angle = 90),
           axis.title.x = element_text(size = rel(2), angle = 00),
           axis.text.x = element_text(size = rel(2)),
           axis.text.y = element_text(size = rel(2)),
           plot.title =  element_text(size = rel(2)),
           legend.text =  element_text(size = rel(2)),
           legend.title =  element_text(size = rel(2)),
           legend.justification 	='center'
    )
  return(list(RAARplot=plt1,AUC=auc))
}