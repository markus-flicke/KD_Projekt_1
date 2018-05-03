\name{boxcoxTrans}
\alias{boxcoxTrans}
\title{BoxCox Transformation}
\description{
 This function performs boxcox transformation:
 TransData(Data,Lambda) = (Data^Lambda -1)/Lambda , Lambda >0
  TransData(Data,Lambda) = log(Data)                 Lambda =0
Lambda is such that it maximizes the Log-Likelihood Function (LLF) 
}
\usage{
boxcoxTrans(Data, Lambdawertebereich = seq(-5,5,0.1))
}

\arguments{
  \item{Data}{numeric matrix, each row is boxcox transformed. }
  \item{Lambdawertebereich}{Bereich in denen ein Lambda gesucht werden soll, anzugeben mit seq. Default: seq(-5,5,0.1)}
}
\details{
 Lambda is such that it maximizes the Log-Likelihood Function (LLF) 
 if only one variable then:
 Lambda: Returns a matrix of output with the results of seven different normality tests and artifical covariate method. The first row of the matrix corresponds to the related estimates of lambda. The subsequent rows correspond to the p-values of different normality tests for each estimates of lambda.
 if more than one variable: the mean of the lambdas for every variable,
}
\value{
  \item{transData }{numeric matrix with boxcox transformed data}
  \item{lambda }{Objekt aus boxcoxncData (fals nur 1 variable gegeben) oder Liste aus Objekten aus boxcoxncData
}

}

\author{Michael Thrun}