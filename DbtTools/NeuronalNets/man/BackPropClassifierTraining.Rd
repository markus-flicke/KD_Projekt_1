\name{BackPropClassifierTraining}
\alias{BackPropClassifierTraining}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BackPropClassifierTraining}
\description{
  Training of a neural network as classifier by using backpropagation
}
\usage{
  BackPropClassifierTraining(Data, Cls, HiddenNeurons, Epochs, PreTrainedMLBP)
}
\arguments{
  \item{Data}{ Matrix that is used for training. One dataset per row. }
  \item{Cls}{ Preclassification matching Data. }
  \item{HiddenNeurons}{ Vector containing the number of neurons that should be used per hidden layer. }
  \item{Learningrate}{ Controls the amount of changes during each epoch }
  \item{Epochs}{ Maximum number of epochs the algorithm will be running }
  \item{PreTrainedMLBP}{ Previously trained neural network. }
}

\value{
  \item{NNCls}{Klassifikation des MLPBP nach dem Training}
  \item{Accuracy}{Performanz als # richtige}
  \item{nn}{das MLP mit Back-Propagation, s. package neuralnets}
}
\author{Michael Thrun}
