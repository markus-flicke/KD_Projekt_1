\name{KNNimputation}
\alias{KNNimputation}
\title{ replace NaN in Data with average valus of the k Nearest Neigbours in Data }
\usage{KNNimputation(Data,k)}
\arguments{
\item{Data}{Data set some of which contains NaN, a nonempty subset is  complete, [1:N,1:d]}
\item{k}{Optional: k>=1 number of nearest neigbors to use default =3;}
}
\value{Imputed Data
}
\description{
 replace NaN in Data with average valus of the k Nearest Neigbours in Data }
\author{RG}
