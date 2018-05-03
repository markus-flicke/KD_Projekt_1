equalsuccessors <- function(x){

  nrOfEquals<-sum(diff(x)==0,na.rm=TRUE)
  ind<-which(diff(x) == 0)
 return (list(nrOfEquals=nrOfEquals,Indices=ind)) 
}

