\name{getTDS}
\alias{getTDS}
\title{Lookup Townsend Deprivation Score from UK postcode}
\description{
  Convenient function to look up Townsend deprivation scores using UK postcodes.
}
\usage{
getTDS(postcode = NULL)
}
\arguments{
  \item{postcode}{Five--seven character UK postcode.}
}
\details{
  \code{getTDS} is a lookup function that queries a dataset of postcodes and Townsend deprivation scores.
  This dataset was created by joining a dataset of postcodes to a dataset of Townsend deprivation scores, using output area codes.
  Both data sets are made available by the Office for National Statistics under the \href{https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/}{Open Government License}. The postcode dataset was last updated in February 2019, while the dataset of Townsend deprivation scores uses values obtained from the 2011 UK census (matching the Townsend deprivation score dates used by ClinRisk in the derivation of the QDiabetes-2018 algorithms).
}
\value{Townsend Deprivation Score.}
\source{
  UK Postcode-to-output-area data were obtained \href{http://geoportal1-ons.opendata.arcgis.com/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-february-2019-lookup-in-the-uk}{here} from the Office for National Statistics.
  
  Output-area-to-Townsend-deprivation-scores data were obtained \href{https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores}{here} from the Office for National Statistics.
}
\note{
  PO Box codes have no associated Townsend deprivation scores and will not work as function inputs.
  No Northern Ireland postcodes are present in the database searched by \code{getTDS} as their use is governed by a separate, more restrictive license.
  \itemize{
    \item Contains OS data © Crown copyright and database right 2020
    \item Contains Royal Mail data © Royal Mail copyright and database right 2020
    \item Source: Office for National Statistics licensed under the Open Government Licence v.3.0
  }
}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\examples{
## Simple usage
getTDS(postcode = "OX2 6GG")
getTDS(postcode = c("OX2 6NW", "OX3 7LF"))

## Case & white space insensitive
getTDS(postcode = c("OX37LF", "OX3 7LF", "ox37lf", "ox3 7lf"))
}
\keyword{postcode}
\keyword{townsend}
\keyword{output area}
