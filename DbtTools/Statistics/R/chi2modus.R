chi2modus <- function(dof){

 if (dof<2){
    chi2modus=0
 }
 else{ 
    chi2modus=dof-2 
 }
 return (chi2modus) 

 }

