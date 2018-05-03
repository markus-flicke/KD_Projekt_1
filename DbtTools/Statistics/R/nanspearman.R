nanspearman <- function(x){

 d<-ncol(x)
 CorM<-diag(d)
 notNaNs<-sum(apply(!is.na(x),1,sum))
 N<-diag(notNaNs)
 for(i in 1:d-1){
    for(j in (i+1):d){
        neither_nan<-which((is.nan(x[,i]))+is.nan(x[,j])==0)
        nn<-length(neither_nan)
        N[i,j]=nn
        N[j,i]=nn
        if(nn>0){
            CorM[i,j]=spearman(x[neither_nan,i],x[neither_nan,j])
        }else {
            CorM[i,j]=NaN
        }
        CorM[j,i]=CorM[i,j]
    }
 }
 return (list(CorM=CorM,N=N)) 

 }

