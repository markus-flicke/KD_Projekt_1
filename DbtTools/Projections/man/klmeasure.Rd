\name{klmeasure}
\alias{klmeasure}
\title{
Computes smoothed precision and smoothed recall
}
\description{
Compares the projection in pData with the original data in Data and calculates the smoothed recall and precision.
}
\usage{
klmeasure(Data, pData, NeighborhoodSize)
}
\arguments{
  \item{Data}{Matrix of original data}
  \item{pData}{Matrix of projected data}
  \item{NeighborhoodSize}{Sets the 'effective number of neighbors' used to control the width of the Gaussian}
}
\value{
Smoothed recall and smooted precision
}
\references{
Jarkko Venna, Jaakko Peltonen, Kristian Nybo, Helena Aidos, and Samuel Kaski. Information Retrieval Perspective to Nonlinear Dimensionality Reduction for Data Visualization. Journal of Machine Learning Research, 11:451-490, 2010.
Jarkko Venna and Samuel Kaski. Nonlinear Dimensionality Reduction as Information Retrieval. In Marina Meila and Xiaotong Shen, editors, Proceedings of AISTATS 2007, the 11th International Conference on Artificial Intelligence and Statistics. Omnipress, 2007. JMLR Workshop and Conference Proceedings, Volume 2: AISTATS 2007.

}
\author{
Felix Pape
}