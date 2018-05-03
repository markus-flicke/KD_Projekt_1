deblank <- function (S) {
# deblank(S)
# remove trailing blanks from string
#
# INPUT
# S              a String

# ALU

return(gsub("[[:space:]]*$","",S))
 
}  #end   function   deblank 

