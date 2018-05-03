\name{BackPropClassifier}
\alias{NNClassifier}
\title{BackPropClassifier}

\description{Create a classification for a dataset by using an already trained neural network}

\usage{BackPropClassifier(Data, nn)}

\arguments{
  \item{Data}{Matrix with 1 Dataset per row}
  \item{nn}{Object of type nn (see neuralnets package)}
}

\value{
  \item{NNCls}{Vector of classifications matching the dataset}
}

\author{Michael Thrun}
