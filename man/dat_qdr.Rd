\name{dat_qdr}
\alias{dat_qdr}
\docType{data}
\title{QDiabetes Sample Dataset}
\description{
  A simulated sample dataset for exploring the use of the \code{QDRA}, \code{QDRB} and \code{QDRC} type 2 diabetes risk prediction functions of this package.
}
\usage{data("dat_qdr")}
\format{
  A data frame with 45 observations on the following 20 variables:
  \describe{
    \item{\code{gender}}{a factor vector of genders (possible values: \code{"Female"} or \code{"Male"})}
    \item{\code{age}}{an integer vector of ages (years)}
    \item{\code{height}}{a double vector of heights (m)}
    \item{\code{weight}}{a double vector of weights (kg)}
    \item{\code{bmi}}{a double vector ofbody-mass indexes (kg/m^2)}
    \item{\code{ethnicity}}{a factor vector of ethnicities (possible values: \code{"WhiteNA"}, \code{"Indian"}, \code{"Pakistani"}, \code{"Bangladeshi"}, \code{"OtherAsian"}, \code{"BlackCaribbean"}, \code{"BlackAfrican"}, \code{"Chinese"} or \code{"Other"})}
    \item{\code{smoking}}{a factor vector of smoking statuses (possible values: \code{"Non"}, \code{"Ex"}, \code{"Light"}, \code{"Moderate"} or \code{"Heavy"})}
    \item{\code{townsend}}{a double vector of Townsend deprivation scores}
    \item{\code{fpg}}{a double vector of fasting plasma glucose test results (mmol/L)}
    \item{\code{hba1c}}{a double vector of glycated haemoglobin test results (mmol/mol)}
    \item{\code{antipsy}}{a logical vector indicating history of 2nd generation antipsychotic use}
    \item{\code{statins}}{a logical vector indicating history of statin use}
    \item{\code{steroids}}{a logical vector indicating history of corticosteroid use}
    \item{\code{cvd}}{a logical vector indicating history of cardiovascular disease}
    \item{\code{gestdiab}}{a logical vector indicating history of gestational diabetes}
    \item{\code{learndiff}}{a logical vector indicating history of learning difficulties}
    \item{\code{schizobipo}}{a logical vector indicating history of shizophrenia or bipolar affective disorder}
    \item{\code{pcos}}{a logical vector indicating history of polycystic ovary syndrome}
    \item{\code{hypertension}}{a logical vector indicating history of hypertension}
    \item{\code{fh_diab}}{a logical vector indicating family history of diabetes}
  }
}
\details{
  A simulated dataset containing the characteristics of 45 subjects (one per row).
  No relationships have been simulated between variables.
  The dataset contains no values or combination of values that would result in warnings or errors from any of the risk prediction functions in this package. It exists purely to allow for testing and exploration of the use of the these functions.
}
\source{Simulated dataset.}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\examples{
data(dat_qdr)
str(dat_qdr)
}
\keyword{datasets}
