\name{dat_qdr}
\alias{dat_qdr}
\docType{data}
\title{QDiabetes Sample Dataset}
\description{
  A simulated sample dataset for exploring the use of the \code{QDR2013}, \code{QDR2018A}, \code{QDR2018B} and \code{QDR2018C} type 2 diabetes risk prediction functions of this package.
}
\usage{data("dat_qdr")}
\format{
  A data frame with 50 observations on the following 21 variables:
  \describe{
    \item{\code{sex}}{A factor vector of genders. Possible values: \code{"Female"} or \code{"Male"}.}
    \item{\code{age}}{An integer vector of ages (years).}
    \item{\code{bmi}}{A double vector ofbody-mass indexes (kg/m^2).}
    \item{\code{ht}}{A double vector of heights (m).}
    \item{\code{wt}}{A double vector of weights (kg).}
    \item{\code{fpg}}{A double vector of fasting plasma glucose test results (mmol/L).}
    \item{\code{hba1c}}{A double vector of glycated haemoglobin test results (mmol/mol).}
    \item{\code{ethn}}{A factor vector of ethnicities. Possible values:
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
    \item{\code{smoke}}{A factor vector of smoking statuses. Possible values:
      \itemize{
        \item \code{"Non"} (Non-smoker)
        \item \code{"Ex"} (Ex-smoker)
        \item \code{"Light"} (Light smoker - less than 10 cigarettes per day)
        \item \code{"Moderate"} (Moderate smoker - 10 to 19 cigarettes per day)
        \item \code{"Heavy"} (Heavy smoker - 20 or more cigarettes per day).
      }
    }
    \item{\code{postcode}}{A character vector of UK postcodes.}
    \item{\code{tds}}{A double vector of Townsend deprivation scores.}
    \item{\code{fhdm}}{A logical vector indicating family history of diabetes.}
    \item{\code{htn}}{A logical vector indicating history of hypertension.}
    \item{\code{cvd}}{A logical vector indicating history of cardiovascular disease.}
    \item{\code{gdm}}{A logical vector indicating history of gestational diabetes.}
    \item{\code{pcos}}{A logical vector indicating history of polycystic ovary syndrome.}
    \item{\code{learn}}{A logical vector indicating history of learning difficulties.}
    \item{\code{psy}}{A logical vector indicating history of shizophrenia or bipolar affective disorder.}
    \item{\code{ster}}{A logical vector indicating history of corticosteroid use.}
    \item{\code{stat}}{A logical vector indicating history of statin use.}
    \item{\code{apsy}}{A logical vector indicating history of 2nd generation antipsychotic use.}
  }
}
\details{
  A simulated dataset containing the characteristics of 50 subjects (one per row).
  No relationships have been simulated between variables.
  The dataset contains no values or combinations of values that would result in warnings or errors from any of the risk prediction functions in this package.
  It exists purely to allow for testing and exploration of these functions.
}
\source{
  The dataset is simulated, except for the \code{postcode} and \code{tds} variables, which are key-value pairs randomly drawn from an Office for National Statistics dataset.
  For more information on this dataset, see the documentation for the \code{\link{getTDS}} function.
  \itemize{
    \item Contains OS data (C) Crown copyright and database right 2020
    \item Contains Royal Mail data (C) Royal Mail copyright and database right 2020
    \item Source: Office for National Statistics licensed under the Open Government Licence v.3.0
  }
}
\author{Benjamin G. Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\examples{
data(dat_qdr)
str(dat_qdr)
}
\concept{risk}
\concept{diabetes}
\keyword{datasets}
