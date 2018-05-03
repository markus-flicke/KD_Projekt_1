qqchi2 <-
function(x, Df=2, Name="Data", pstyle=20, filename=""){

# qqchi2(x,df,name,ps)
# Quantile/Quantile = QQ-Plot; Compares to: chi square distribution
#
# ARGUMENT
# x - variable, which will be plotted
#
# OPTIONAL
# Df - degree of freedom for chi square distribution
# -> Default: 2
# name - label of y-axis
# -> Default: "data"
# pstyle - pchtype / linetype (for more details see: rplot)
# -> Default: 20 ("dots")
# filename - name for plotfile -> filename.eps

# NaN handling
# find indexes with !is.nan, which aren't NaN's. Get those indexes whith "which".
# Use these indexes to create a new vector x from the old vector x, without any
# NaN's. Than sort it.
x <- sort(na.last=T,x[c(which(!is.nan(x)))])

# Get dimension of x.
d <- dim(t(matrix(x)))

# random chi^2-Matrix. Calling rchisq(dim[1]*dim[2],...) to make sure every entry
# is different.
chirand <- matrix(rchisq(d[1]*d[2],Df),nrow=d[1],ncol=d[2])

# compute percentile
###### px = percentiles(x) #########
px = quantile(x,seq(0.01,1,0.01))

###### pchi = percentiles(chirand) ##########
pchi = quantile(chirand,seq(0.01,1,0.01), na.rm=TRUE)

if(filename != "") postscript(paste(filename, ".eps", sep=""))
plot(pchi, px, pch=pstyle, col="blue", main="QQ-Plot - Chi^2", ylab=Name, xlab=paste("chi^2, df = ",Df,seq=""))
if(filename != "") dev.off()

 }

