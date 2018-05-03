\name{kendall}
\alias{kendall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kendalls Tau}
\description{
Berechnet Kendalls Tau fuer z.B. ein Shepard Diagram
}
\usage{
%- all arguments (funktionsaufruf)
fun(arg1, arg2, ...)
}
\arguments{
  \item{InputDist}{ Matrize der Distanzen des Eingaberaumes}
  \item{OutputDist}{ Matrize der Distanzen des Ausgaberaumes }
}
\details{
%- typical short usage of functions
Typical usages are
  \preformatted{fun(arg1,arg2)}
  
  \code{fun name} long description
}
\value{
%- description of return values
	\item{Tau}{Kendalls Tau, nicht linearer (nur!) STATISTISCHER Zusammenhang}
	\item{pVal}{Signifikanzlevel}
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{ultsch@mathematik.uni-marburg.de}
}
\author{Michael Thrun}