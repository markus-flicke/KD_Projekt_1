ladderplot <-
function(Data, Names=NULL, cls=NULL, filename=""){

 dbtMatlabColours <- c("green", "red", "blue", "cyan", "magenta", "yellow")

 d <- dim(Data)
 rows <- d[1]
 minValue <- min(Data,na.rm=TRUE)
 maxValue <- max(Data,na.rm=TRUE)
 numVars <- d[2]
 # TODO: Compare length of Data and class vector 
 
 if(length(cls)==0) cls<-rep(0, rows)
 
 plot(Data[1,], type="l", ylim=c(minValue, maxValue), xlim=c(1, numVars), col=dbtMatlabColours[cls[1]%%length(dbtMatlabColours)+1], xlab="", ylab="")
 for(i in 2:rows){
    curColour <- dbtMatlabColours[cls[i]%%length(dbtMatlabColours)+1]
    lines(x=1:numVars, y=Data[i,], col=curColour )
 }
 med <- minValue + (maxValue-minValue)/2
 if(length(Names)>0){
    for(m in 1:numVars){
      text(x=m, y=med, Names[m], srt=90, cex=0.8)
    }
 }
 
 
 
 }

