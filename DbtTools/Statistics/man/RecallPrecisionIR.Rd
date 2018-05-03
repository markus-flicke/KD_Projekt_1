\name{RecallPrecisionIR}
\alias{RecallPrecisionIR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Recall and Precision in Information Retrieval }
\description{
  Calculate Precision and Recall as used in Information Retrieval - OLD version
  New Version is informationRetrieval with IR.PrecisionRecall and IR.AUCandFbetha, see also IR4Terms
}
\usage{
RecallPrecisionIR(IsRelevant, NrOfAllRelevantItems, NrOfAllNonRelevantItems=0)
}
\arguments{
  \item{isRelevant}{ biniary vector indicating eheter the retrieved item is Relevant }
  \item{nrOfAllRelevantItems}{ the total number of all relevant items no matter if they are retrieved or not }
  \item{nrOfAllNonRelevantItems}{ optional; the total number of all not relevant items no matter if they are retrieved or not }
}

\value{
  \item{precision}{ NrOfRelevantItems / NrOfRetrievedItems * 100 == PositivePredictiveValue}
  \item{recall}{ NrOfRelevantItems / NrOfAllRelevantItems * 100 == Sensitivity}
  \item{fmeasure}{  weighted harmonic mean of precision and recall, the traditional F-measure or balanced F-score}
  \item{fallOut}{ NrOfNonRelevantItems/NrOfAllNonRelevantItems*100; ONLY if NrOfAllNonRelevantItems is givem}
}
\author{ Zornitsa Manolova }


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ preciosion }
\keyword{ recall }

