qqcauchy <-
function(x, Name="Data", percentil=2, pstyle=20, filename=""){

# qqcuchy(x,Name,percentil,pstyle,filename)
# Quantile/Quantile = QQ-Plot; Copmpares to: cauchy distribution
#
# INPUT
# x - variable, which will be plotted
#
# OPTIONAL INPUT
# Name - label of y-axis
# -> Default: "data"
# percentil - percentage of data, which will be rejected to reduce noise.
# -> Default: 2
# pstyle - pchtype / linetype (for details see: rplot)
# -> Default: "20" ("dots")
# filename - name for plotfile -> filename.eps

# NaN handling
# find indexes with !is.nan, which aren't nan's. Get those indexes whith "which".
# Use these indexes to create a new vector x from the old vector x, without any
# nan's. Than sort it.
x <- sort(na.last=T,x[c(which(!is.nan(x)))])
n <- length(x)
ex <- round(n*percentil/200) # ex = extremum
x <- x[ex+1:n-ex]
n <- length(x)
X <- (c(1:n)-1/2)/n
Y <- qcauchy(X) # The equivalent of matlabs "cauchyinv()".

if(filename != "") postscript(paste(filename, ".eps", sep=""))
plot(Y, x, pch=pstyle, col="blue", main="QQ-Plot - Cauchy", ylab=Name, xlab="Cauchy")
if(filename != "") dev.off()

}

