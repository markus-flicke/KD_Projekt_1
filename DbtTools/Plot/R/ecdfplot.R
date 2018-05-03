ecdfplot<-function(Data,xlab = NULL, ylab = NULL, main = NULL,...){

# graphical parameters
if(length(xlab) == 0){
	xlab <- ''
}
if(length(ylab) == 0){
	ylab <- ''
}
if(length(main) == 0){
	main <- ''
}


plot(ecdf(Data), xlab = xlab, ylab = ylab, main = main, verticals=TRUE, pch=20, cex=0.8, col="red", ...)


}