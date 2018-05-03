cauchypdf <- function(Data,m=0,md=1){

 DataPDF<-pcauchy(Data,location=m,scale=md)
 plot(Data,DataPDF)
 return (DataPDF) 

 }

