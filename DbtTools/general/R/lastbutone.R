lustbutone <- function(x){
# returns the last and last but one element of an vector
# author: MT
# exmaple
#           ende=lustbutone(Stressverlauf)
#           letztereintrag=ende$Last
#           vorletztereintragende$Lastbutone
last=x[length(x)] 
vorletzter=x[length(x)-1]
return(list(Last=last,Lastbutone=vorletzter))
}