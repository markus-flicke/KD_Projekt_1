\name{Silhouette}
\alias{Silhouette}

\title{
Silhouette plot for clustered data.
}
\description{

   SILHOUETTE(X, CLUST) plots cluster silhouettes for the N-by-P data
   matrix X, with clusters defined by CLUST.  Rows of X correspond to
   points, columns correspond to coordinates.  CLUST is a categorical
   variable, numeric vector, character matrix, or cell array of strings
   with a common value for points in the same cluster. By default, SILHOUETTE uses the squared
   Euclidean distance between points in X.
}
\usage{
Silhouette(X, clust, distance='sqEuclidean', showPlot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{N-by-P data matrix. Rows of X correspond to points, columns correspond to coordinates.}
  \item{clust}{A numeric vector (length N) with a common value for points in the same cluster }
  \item{distance}{Inter-point distance measure. Coices for distance are:
         'euclidean'     - Euclidean distance; 
        {'sqEuclidean'}  - Squared Euclidean distance; 
         'cityblock'     - Sum of absolute differences, a.k.a. L1; 
         'cosine'        - One minus the cosine of the included angle
                          between points (treated as vectors); 
         'chebychev'     ; 
         'jaccard'       - Percentage of non-zero coordinates that differ; 
         'minkowski'     . 
         Default: 'sqEuclidean'
  }
  \item{showPlot}{FALSE to supress the plot. Default:TRUE}
}
\value{

\item{silh}{Silhouette values in a N-by-1 vector}

}

\author{
O. Hansen-Goos
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
% Problem beim Beispiel: Silhouette verwendet size aus RetroMAT, was beim
% übersetzen des Beispiels nicht gefunden wird. TODO
%\examples{
%X = rbind( cbind(rnorm(10,1,1)+1,rnorm(10,1,1)+1) , cbind(rnorm(10,1,1)-1,rnorm(10,1,1)-1) )
%cidx = kmeans(X, 2)
%clust = cidx$cluster
%s1 = Silhouette(X, clust)
%s2 = Silhouette(X, clust, distance='euclidean', showPlot=FALSE)
%}
