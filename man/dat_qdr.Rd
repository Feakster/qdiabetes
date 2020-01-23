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
    \item{\code{gender} A factor vector of genders. Possible values: \code{"Female"} or \code{"Male"}.}
    \item{\code{age}}{An integer vector of ages (years).}
    \item{\code{height}}{A double vector of heights (m).}
    \item{\code{weight}}{A double vector of weights (kg).}
    \item{\code{bmi}}{A double vector ofbody-mass indexes (kg/m^2).}
    \item{\code{ethnicity}}{A factor vector of ethnicities. Possible values:
      \itemize{
        \item \code{"WhiteNA"} (White or not stated)
        \item \code{"Indian"} (Indian)
        \item \code{"Pakistani"} (Pakistani)
        \item \code{"Bangladeshi"} (Bangladeshi)
        \item \code{"OtherAsian"} (Other Asian)
        \item \code{"BlackCaribbean"} (Black Caribbean)
        \item \code{"BlackAfrican"} (Black African)
        \item \code{"Chinese"} (Chinese)
        \item \code{"Other"} (Other ethnic group).
      }
    }
    \item{\code{smoking}}{A factor vector of smoking statuses. Possible values:
      \itemize{
        \item \code{"Non"} (Non-smoker)
        \item \code{"Ex"} (Ex-smoker)
        \item \code{"Light"} (Light smoker - less than 10 cigarettes per day)
        \item \code{"Moderate"} (Moderate smoker - 10 to 19 cigarettes per day)
        \item \code{"Heavy"} (Heavy smoker - 20 or more cigarettes per day).
      }
    }
    \item{\code{townsend}}{A double vector of Townsend deprivation scores.}
    \item{\code{fpg}}{A double vector of fasting plasma glucose test results (mmol/L).}
    \item{\code{hba1c}}{A double vector of glycated haemoglobin test results (mmol/mol).}
    \item{\code{antipsy}}{A logical vector indicating history of 2nd generation antipsychotic use.}
    \item{\code{statins}}{A logical vector indicating history of statin use.}
    \item{\code{steroids}}{A logical vector indicating history of corticosteroid use.}
    \item{\code{cvd}}{A logical vector indicating history of cardiovascular disease.}
    \item{\code{gestdiab}}{A logical vector indicating history of gestational diabetes.}
    \item{\code{learndiff}}{A logical vector indicating history of learning difficulties.}
    \item{\code{schizobipo}}{A logical vector indicating history of shizophrenia or bipolar affective disorder.}
    \item{\code{pcos}}{A logical vector indicating history of polycystic ovary syndrome.}
    \item{\code{hypertension}}{A logical vector indicating history of hypertension.}
    \item{\code{fh_diab}}{A logical vector indicating family history of diabetes.}
  }
}
\details{
  A simulated dataset containing the characteristics of 45 subjects (one per row).
  No relationships have been simulated between variables.
  The dataset contains no values or combinations of values that would result in warnings or errors from any of the risk prediction functions in this package.
  It exists purely to allow for testing and exploration of these functions.
}
\source{Simulated dataset.}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\examples{
data(dat_qdr)
str(dat_qdr)
}
\keyword{datasets}
