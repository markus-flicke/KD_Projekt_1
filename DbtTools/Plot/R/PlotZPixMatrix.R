PlotZPixMatrix=function(LineCoords,ColCoords,Z,Lines,Columns,Tiling=TRUE,LowLim=min(Z),HiLim=max(Z)){
# PlotPixMatrix(Data,Names,LowLim,HiLim);
# plot Best Matches as a pixel coulour picture
#
# INPUT
# LineCoords[1:n,1]       from Data(1:n,2) data cases in rows, variables in columns
# ColCoords[1:n,2]        from Data(1:n,2) data cases in rows, variables in columns
# Z[1:n,1]                Vektor
# OPTIONAL
# Tiling                  gekachelt=TRUE (default)
# LowLim,HiLim           limits for the color axis, Default: min und max
# 
# Reimplemented in R by MT, matlab: Version: ALU September 2007


  Data = matrix(NaN,Lines,Columns) #alles NaN initialisieren
  AnzPoints = length(Z)
  for(i in 1:AnzPoints){
      Data[LineCoords[i],ColCoords[i]] = Z[i]
  }
  
  if(Tiling){Data = TileMatrix(Data)} # kacheln
  
  PlotPixMatrix(Data=Data,LowLim=LowLim,HiLim=HiLim)
}

