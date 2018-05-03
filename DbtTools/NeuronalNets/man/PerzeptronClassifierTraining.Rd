\name{PerzeptronClassifierTraining}
\alias{PerzeptronClassifierTraining}
\title{PerzeptronClassifierTraining}
\description{
Training of a single layer (perceptron) neural network as classifier
}
\usage{
PerzeptronClassifierTraining(Data, Cls, Epochs = 100000, PreTrained = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Matrix of data that will be used to train the Neural Netzwork}
  \item{Cls}{Vector that contains the Pre Classification of the Data}
  \item{Epochs}{Maximum number of epochs the algorithm should run}
  \item{PreTrained}{Previously trained perzeptron neural network}
}
  
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	\item{NNCls}{Classification of given dataset}
	\item{Accuracy}{Accuracy of classification of given dataset}
	\item{nn}{neural net object. see package neuralnet}
}

\author{
	MT/FL
}
