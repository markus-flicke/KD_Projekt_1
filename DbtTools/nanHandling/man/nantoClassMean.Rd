\name{nantoClassMean}
\alias{nantoClassMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NaN to Class Mean }
\description{
  Substitute any NaN values in Data by the mean of each class
}
\usage{
nantoClassMean(Data, Cls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ a matrix }
  \item{Cls}{ a class vector }
}
\details{
  use the function "ClassMean" from the package dbt.ClassAnalysis
}
\value{
  \item{comp1 }{a matrix without NaNs or NA}
}
\author{ Zornitsa Manolova }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mean }
\keyword{ class }% __ONLY ONE__ keyword per line
\keyword{ NaN }
\keyword{ NA }