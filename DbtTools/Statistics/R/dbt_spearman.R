dbt_spearman <- function(x, y){
rho<-cor(rank(x),rank(y)) 
return(rho)

 }

