\name{ResilientPropClassifierTraining}
\alias{ResilientPropClassifierTraining}
\title{ResilientPropClassifierTraining}
\description{
  Training of a neural network as classifier by using rprop+ (resilient backpropagation)
}

\arguments{
  \item{Data}{ Matrix that is used for training. One dataset per row. }
  \item{Cls}{ Preclassification matching Data. }
  \item{HiddenNeurons}{ Vector containing the number of neurons that should be used per hidden layer. }
  \item{Epochs}{ Maximum number of epochs the algorithm will be running }
  \item{PreTrainedNN}{ Previously trained neural network. }
  \item{PlotIt}{ Should the neural network be plotted after training? }
}

\value{
  \item{NNCls}{Klassifikation des MLPBP nach dem Training}
  \item{Accuracy}{Performanz als # richtige}
  \item{nn}{das MLP mit Back-Propagation, s. package neuralnets}
}

\author{Michael Thrun \ Florian Lerch}
