\name{BackPropProjection}
\alias{BackPropProjection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BackPropProjection}
\description{
BackPropProjection
}
\usage{
BackPropProjection(Data, MLPBP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ ~~Describe \code{Data} here~~ }
  \item{MLPBP}{ ~~Describe \code{MLPBP} here~~ }
}
\details{
 function  ProjectedData = BackPropProjection(Data,MLPBP);
 
 [ProjectedData] = BackPropProjection(Data,MLPBP);
 
  ein MLPBP projektion N -> k -> N
  
 das Netz wiurde auf Identitaet trainiert, siehe Funktion
 
 BackPropProjection(...)
 
 INPUT
 
 Data(1:n,1:d)          cases in rows, variables in columns
 
 MLPBP                  das MLP mit Back-Propagation wie in den Mathlabfunktionen
 
 OUTPUT
 
 ProjectedData           die Projektion = Aktivierung der mittlersten Schicht
 
                        NOTA ist es aber noch nicht !!!!
						
 ProjectedData = 'NOCH NICHT IMPEMENTIERT'

}
\value{
...
}
\references{ ~put references to the literature/web site here ~ }
\author{Michael Thrun}
