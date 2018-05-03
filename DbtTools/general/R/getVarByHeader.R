getVarByHeader=function(V,i){
#  getVarByHeader(V,i)
# Selectes Variable by Name in Header
# INPUT
# V       list of ReadLRN or ReadData
# i       which Variable do you want to select
# Output
# Var     Vecotor of Variable wit Varname=Header[i] in Global Environment
# author MT 08/2015
  print(paste('You selected',V$Header[i]))
  return(assign(V$Header[i],as.vector(V$Data[,V$Header[i]]),envir =.GlobalEnv))
}