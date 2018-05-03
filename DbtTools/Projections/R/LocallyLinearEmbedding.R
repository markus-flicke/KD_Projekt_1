LocallyLinearEmbedding  = function(Data,OutputDimension=2,k,PlotIt=FALSE,Cls){
  # Locally Linear Embedding as introduced in 2000 by Roweis, Saul and Lawrence. 
  # projection=LocallyLinearEmbedding(Data,k=10) 
  # INPUT 
  # Data[1:n,1:d]      array of data: n cases in rows, d variables in columns
  # OPTIONAL

  # OutputDimension           data is projected onto a R^p where P is the maximum ( default ==2)
  #                           of the dimension chosen by cmdscale and OutputDimension
  # k                       number of k nearest neighbors              

  # PlotIt                    bool, defaut=FALSE, if =TRUE: ClassPlot of every current Position of Databots will be made.
  #                           OutputDimension>2 only the first two dimensions will be shown
  # cls                       vector, Classifikation of Data if available, ClassPlots will be colorized
  
  # OUTPUT is a list with following elements:
  # ProjectedPoints[1:n,OutputDimension]   n by OutputDimension matrix containing coordinates of the Projection: A matrix of the fitted configuration.

  
  # author: MT 06/2015
  if (missing(Data))
    stop('No Data given')
  Data
  
  if (!is.matrix(Data))
    stop('Data has to be a matrix, maybe use as.matrix()')
  
  AnzVar = ncol(Data)
  AnzData = nrow(Data)
  warning('RDRToolbox for LLE has still some major bugs')
  if (missing(k))
    stop('k nearest neighbor value missing')
  
  requireNamespace("RDRToolbox")
  
  ProjectedPoints = RDRToolbox::LLE(data = Data, dim = OutputDimension, k =
                                      k)
  
  if (PlotIt) {
    if (missing(Cls))
      Cls = rep(1, AnzData)
    
    string = paste0('LLE projection with k ', k)
    
    PlotProjectedPoints(ProjectedPoints, Cls = Cls, main = string)
    #requireNamespace("dbt.ClassAnalysis"
    #dbt.ClassAnalysis::ClassPlot(ProjectedPoints[,1],ProjectedPoints[,2],Cls=Cls,Title=string)
    
  }
  return(list(ProjectedPoints = ProjectedPoints))
  
}