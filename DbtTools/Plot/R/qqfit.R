qqfit <-
function(x, y, yint){

# qqfit(x,y,yint)
#
# INPUT
# x,y - original points vectors of length (1:AnzPoints)
# yint - y values  such that (x,yint) are the poihnts of regression line
#
# OUTPUT
# list - A list, which contains the difference y-yint as the first and the

# mean-absolute-delta as the second element.
anzPoints = length(x)

# Throw Error if x is to small
if(anzPoints < 2) {
  print("Error: To few points to caclulate MeanAbsDelta")
  }
else{
  delta <- y-yint
  sumAbsDelta <- sum(abs(delta))
  meanAbsDelta <- sumAbsDelta/anzPoints
  return(list(delta,meanAbsDelta))
}

}

