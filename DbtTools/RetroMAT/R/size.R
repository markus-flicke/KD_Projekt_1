size <- function (A) {
# Size <- size(A)
# return the size of the type A as Size[1],Size[2]


TA <-is(A);
TypeOfA=TA[2] ; # print(TypeOfA) # test
Result = c(NaN,NaN)
if (TypeOfA=="vector"){Result = c(length(A),1)}
if (TypeOfA== "array"){Result = dim(A)}

return(Result)
} # end function size 
