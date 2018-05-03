SwitchCls <- function(cls, c1, c2) {

newCls <- cls
indexOfC1 <- which(cls == c1)
indexOfC2 <- which(cls == c2)
newCls[indexOfC1] <- cls[indexOfC2]
newCls[indexOfC2] <- cls[indexOfC1]
return(newCls)
}