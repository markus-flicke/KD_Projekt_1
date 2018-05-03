allnotnans <- function(x){

  n<-sum(!is.na(x),na.rm=TRUE)

  return(n)
}

