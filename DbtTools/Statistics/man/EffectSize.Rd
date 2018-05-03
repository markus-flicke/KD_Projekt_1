\name{EffectSize}
\alias{EffectSize}

\title{
EffectSize of a binary classification
}
\description{
 equals Cohen's d for parametric (Gaussian) distributions
 for nongaussian distribitions an analogous formula using medians and
 Adjuested MAD (Median absolute differences) is used
}
\usage{
EffectSize(Data,Cls,IsParametric)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
 n cases,  d variables
}
  \item{Cls}{
cls(i) == ClusterNumber of data(i,:)
                     NOTE it is assumed that the classificaion is Binary i.e. contains only two classes: Cls1 and Cls2
                     if there are more than two classes only two of them are used
}
  \item{IsParametric}{
  ==1 => Cohen's d is calculated, otherwise medians are used, default: IsParametric==0
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{CohensD}{
 = Diff / PooledDev
}
  \item{Cls1,Cls2}{
 the numbers of the two binary Classes in Cls which are used
}
  \item{Diff}{
ClassMeanRobust(D(:,Cls2)) -  ClassMeanRobust(D(:,Cls1)),iff IsParametric ==1
ClassMedian(D(:,Cls2)) -          ClassMedian(D(:,Cls1)),iff IsParametric ~=1
}
  \item{PooledDev}{
sqrt((n1-1)*Dev1^2+(n2-1)*Dev2^2/(n-2))  where n1 is the number of elements (notNaN) in Cls1 , n2 analog
}
  \item{Dev1,Dev2}{
ClassStdRobust(D(Cls1,:))  i.e. the (robust) standard deviation of Cls1 ignoring nan , iff IsParametric ==1
ClassStdRobust(D(Cls2,:))  i.e. the (robust) standard deviation of Cls2 ignoring nan , iff IsParametric ==1
ClassAMAD(D(Cls1,:))  i.e. the adjusted MAD  of Cls1 ignoring nan , iff IsParametric ~=1
ClassAMAD(D(Cls2,:))  i.e. the adjusted MAD  of Cls2 ignoring nan , iff IsParametric ~=1
}
  \item{n1,n2}{
 the number of notNaN elements in the classes
}


}

\author{
Michael Thrun
}

\seealso{
\code{\link{ClassStdRobust}}, \code{\link{ClassAMAD}},  \code{\link{ClassMedian}},  \code{\link{ClassMeanRobust}} in the ClassAnalysis package
}


\keyword{cohen}
\keyword{effektsize }
