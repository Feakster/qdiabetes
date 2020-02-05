\name{getTDS}
\alias{getTDS}
\title{Lookup Townsend Deprivation Score from UK postcode}
\description{
  Convenient function to look up Townsend deprivation scores using UK postcodes.
}
\usage{
getTDS(postcode = NULL, type = "full", squash = if(type != "regex") FALSE else TRUE)
}
\arguments{
  \item{postcode}{A vector of UK postcodes, postcode prefixes, postcode suffixes, or a regular expression.}
  \item{type}{A character string indicating the term provided in the \code{postcode} parameter. Possible values are:}
    \itemize{
      \item \code{"full"} --- a full five--seven character postcode.
      \item \code{"prefix"} --- a two--four character postcode prefix.
      \item \code{"suffix"} --- a three character postcode suffix.
      \item \code{"regex"} --- a regular expression, allowing any nature of postcode match.
    }
  \item{squash}{a logical parameter indicating whether a overall median aggregate score should be returned.}
}
\details{
  \code{getTDS} is a lookup function that queries a dataset of postcodes and Townsend deprivation scores.
  This dataset was created by joining a dataset of postcodes to a dataset of Townsend deprivation scores, using output area codes.
  Both data sets are made available by the Office for National Statistics under the \href{https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/}{Open Government License}.
  The postcode dataset was last updated in February 2019, while the dataset of Townsend deprivation scores uses values obtained from the 2011 UK census (matching the Townsend deprivation score dates used by ClinRisk in the derivation of the QDiabetes-2018 algorithms).
}
\value{Townsend Deprivation Score. Where \code{type} is not set to \code{"regex"}, the nature of the value returned is governed by the \code{type} and \code{squash} parameters:
\itemize{
  \item When \code{type} is set to \code{"full"}, setting \code{squash} to \code{FALSE} (the default value) will result in an output vector or equal length to the input vector. As each postcode is only associated with a single output area, no further aggregation is required in the associated Townsend deprivation scores. Setting \code{squash} to \code{FALSE} will produce an numeric output of length one, comprising the median Townsend deprivation score from all matched postcodes.
}
\code{FALSE}, the default setting for \code{squash}, will result in an output vector of length equal to the input vector, containing median-stratified vales. Setting \code{squash} to \code{FALSE} will result in an output of length equal to one, comprising an overall aggregate median value.}
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
