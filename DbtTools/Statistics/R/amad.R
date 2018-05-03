amad <- function(x){

 adjfactor<-1.3
 ergMad<-dbt_mad(x)
 amad<- ergMad$centerMad*adjfactor
 leftAmad<- ergMad$leftMad*adjfactor
 rightAmad<- ergMad$rightMad*adjfactor
 return (list(amad=amad,leftAmad=leftAmad,rightAmad=rightAmad)) 

 }

