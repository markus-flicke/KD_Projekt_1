\name{dbtplot}
\alias{dbtplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dbt.RetroMAT-package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dbtplot(X, Y, LineSpec = "b.", LineWidth = 1, MarkerSize = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{LineSpec}{
%%     ~~Describe \code{LineSpec} here~~
}
  \item{LineWidth}{
%%     ~~Describe \code{LineWidth} here~~
}
  \item{MarkerSize}{
%%     ~~Describe \code{MarkerSize} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X,Y,LineSpec='b.',LineWidth=1,MarkerSize=1)
{
V= LineSpec2R(LineSpec)       # umcodierung der LineSpec zu R werten
Ltype=V[1]; Lpch=V[2];Lcol=V[3];Llty=as.numeric(V[4]);
print(V)

if(dev.cur()==1) #wenn noch keine Fenster offen ist
  {
  plot(X,Y,type=Ltype,pch=Lpch,col=Lcol,lty=Llty,xlab=' ',ylab=' ',cex=3)
  }
else  #wenn schon ein fenster offen ist.
  {    
  points(X,Y,type=Ltype,pch=Lpch,col=Lcol,lty=Llty,xlab=' ',ylab=' ',cex=3)      
  }
  }  #END FUNCTION
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
