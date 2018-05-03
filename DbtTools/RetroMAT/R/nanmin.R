nanmin <- function (Data) {
# spaltenweises minimum  NaN werden ignoriert


## Compute row and column min for a matrix:

if  (length(dim(Data)) ==2) {
    SpaltenMinima <- apply(Data, 2, function(x) min(x,na.rm=TRUE))
    SpaltenInd    <- NaN   # Noch nicht implementiert !!!
    }else{
    SpaltenMinima <- min(Data,na.rm=TRUE)  
    SpaltenInd    <- which( Data==SpaltenMinima)
} #     if  (length(dim(Data) ==2) 

return(SpaltenMinima)#   SpaltenInd    <- NaN   # Noch nicht implementiert !!!
} #   nanmin <- function (Data) 

