\name{qqnormALU}
\alias{qqnormALU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dbt.RetroMAT-package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qqnormALU(x, Ylabel = " ", PlotSymbol = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{Ylabel}{
%%     ~~Describe \code{Ylabel} here~~
}
  \item{PlotSymbol}{
%%     ~~Describe \code{PlotSymbol} here~~
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
function (x,Ylabel=' ',PlotSymbol='.')
# Quantile/Quantile = QQ-Plot im Vergleich. zur Normalverteilung
# INPUT
# x	   zu zeichnende Vatiable
# OPTIONAL
# Ylabel bezeichnung der Datem-achse
# PlotSymbol   ist der linientyp (.0x+ usw siehe funktio nplot)
#      wenn nicht angegeben: .
# OUTPUT
# qqx,qqy   die Punkte des qq-plots 
{
# x = x(find(~(isnan(x))));       # nan entfernen
qqy = sort(na.last=T,x)
n = length(qqy)
X = ((1:n)-1/2)/n; X
qqx = sqrt(2)*dnorm(2*X-1); 
qqx= 2 * pnorm(x * sqrt(2)) - 1
qqx = sqrt(2)*erfinv(2*X-1);


dbtplot(qqx,qqy,PlotSymbol);
xlabel('Norm');
ylabel(Ylabel);

  }  #END FUNCTION
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
