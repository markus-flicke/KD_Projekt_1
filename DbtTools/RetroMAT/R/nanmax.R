nanmax <- function (Data) {
# spaltenweises max  NaN werden ignoriert


## Compute row and column MAX for a matrix:

if  (length(dim(Data)) ==2) {
    SpaltenMinima <- apply(Data, 2, function(x) max(x,na.rm=TRUE))
    SpaltenInd    <- NaN   # Noch nicht implementiert !!!
    }else{
    SpaltenMinima <- max(Data,na.rm=TRUE)  
    SpaltenInd    <- which( Data==SpaltenMinima)
} #     if  (length(dim(Data) ==2) 

return(SpaltenMinima)#   SpaltenInd    <- NaN   # Noch nicht implementiert !!!
} #   nanmax<- function (Data) 

