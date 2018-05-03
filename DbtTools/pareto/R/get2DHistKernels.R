get2DHistKernels <- function(InputDists, OutputDists){
  x=InputDists[lower.tri(InputDists, diag = FALSE)]

  y=OutputDists[lower.tri(OutputDists, diag = FALSE)]
#requireRpackage('reshape2')

noNaNInd <- which(!is.nan(x)&!is.nan(y))
x <- x[noNaNInd]
y <- y[noNaNInd]

data <- cbind(x,y)
nData <- length(x)

paretoRadius <- ParetoRadius(Data = data)
# 2D Histogram:
# Get optimal number of bins
xnum <- OptNrOfBins(Data = data[,1])
ynum <- OptNrOfBins(Data = data[,2])

# Getting the kernels of optimal histograms for each variable
minx = min(data[,1])
maxx = max(data[,1])
i = maxx-minx
xbins = seq(minx, maxx, i/xnum)

miny = min(data[,2])
maxy = max(data[,2])
i = maxy-miny
ybins = seq(miny, maxy, i/ynum)

return(kernels <- as.matrix(expand.grid(x = xbins, y = ybins)))
}  
