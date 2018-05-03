`toPercent` = topercent <- function(data){
#
# INPUT
# data     data matrix
#
# OUTPUT
# scaled   scaled data such that in each comum of data
#          Minumum is 0 and Maximum is 100

#
#uses toRange
  

	
	scaled= toRange(data,0,100)
	return(scaled)

 }

