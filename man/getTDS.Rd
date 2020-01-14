\name{getTDS}
\alias{getTDS}
\title{Lookup Townsend Deprivation Score from UK postcode}
\description{
  Convenient function to look up a Townsend Deprivation Score using a UK postcode.
}
\usage{
getTDS(postcode = NULL)
}
\arguments{
  \item{postcode}{Five--seven character UK postcode.}
}
\details{
  \code{getTDS} is a lookup function that queries a dataset of postcodes and Townsend Deprivation Scores.
  This dataset was created by joining a dataset of postcodes and Townsend Deprivation Scores by output area.
  Both data sets are made available by the Office for National Statistics under the Open Government License.
  The function is not sensitive to case or white space.
}
\value{Townsend Deprivation Score.}
\source{
  \url{http://geoportal1-ons.opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0}
  \url{https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores}
}
\note{
  PO Box codes have no associated Townsend Deprivation Scores and will not work as function inputs.
}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\examples{
## Case & white space insensitive
getTDS(postcode = "OX37LF")
getTDS(postcode = "OX3 7LF")
getTDS(postcode = "ox37lf")
getTDS(postcode = "ox3 7lf")

## Serialising
sapply(c("OX26GG", "OX26NW", "OX37LF"), getTDS)
}
\keyword{postcode}
\keyword{townsend}
\keyword{output area}
