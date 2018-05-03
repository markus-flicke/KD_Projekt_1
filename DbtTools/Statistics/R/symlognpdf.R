symlognpdf <- function(Data,M,S){
#pdf = symlognpdf(Data,M,S);
# for M>0 same as dlnorm(Data,M,S); (Dichte der log-Normalverteilung)
# for M < 0: mirrored at y axis
#INPUT
#Data[1:n]  x-values
#M,S        Mean and Sdev of lognormal
  
 temp<-symlognSigmaMue(M,S)
 mu<-temp$mu
 sig<-temp$sig
 if(M>=0){
     pdfkt<-dlnorm(Data,mean=mu,sd=sig)  
 }else{
    pdfkt<-Data*0
    negDataInd<-which(Data<0)
    pdfkt[negDataInd] <- dlnorm(-Data[negDataInd],mean=mu,sd=sig)
    plot(Data,pdfkt)
 }
 return (pdfkt) 
 }

