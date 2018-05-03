`sRelDiffTransInvers` <- function(sRelDiff){

	x <- sign(sRelDiff) * relDiff2Yield(abs(sRelDiff))

 	return (x) 

}