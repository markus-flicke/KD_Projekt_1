nantovalue <- function(z,value){

    out<-z
    out[is.na(out)]<-value

 return (out) 

 }

