LeptoByKurtosis <- function(x){
        
 rows<-nrow(x)
 cols<-ncol(x)
 mKurt<-3
 dummy<-noNaN(x)
 x<-dummy[[1]]
 rows<-dummy[[2]] 
 anzInDistrb<-dummy[[3]]  
 if(cols<2 || is.vector(x)){
    Kurtosis<-kurtosis(x)
    sKurt<-4.91*(1/sqrt(anzInDistrb))
    KurtosisGaussCdf<-pnorm(Kurtosis,mKurt,sKurt)
    ProbIsLeptokurtic<-max(0,2*KurtosisGaussCdf-1)        
 }else{   
    Kurtosis<-c()
    sKurt<-c()
    KurtosisGaussCdf<-c()
    ProbIsLeptokurtic<-c()
    for(i in 1:cols){
        Kurtosis[i]<-kurtosis(x)
        sKurt[i]<-4.91*(1/sqrt(anzInDistrb))
        KurtosisGaussCdf[i]<-pnorm(Kurtosis,mKurt,sKurt)
        ProbIsLeptokurtic[i]<-max(0,2*KurtosisGaussCdf[i]-1)
    }
 }
 return (list(ProbIsLeptokurtic=ProbIsLeptokurtic,Kurtosis=Kurtosis,sKurt=sKurt)) 

 }

