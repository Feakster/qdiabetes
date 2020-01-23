\name{QDRB}
\alias{QDRB}
\title{QDiabetes-2018 Risk Calculator (B-Variant)}
\description{
  Calculate the 10-year risk of developing type 2 diabetes, using the B-variant of the QDiabetes-2018 algorithm.
  This variant includes all risk predictors present in the A-variant, with the addition of fasting plasma glucose.
}
\usage{
QDRB(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL,
     fpg = NULL, ethnicity = "WhiteNA", smoking = "Non",
     townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE,
     gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE,
     pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE)
}
\arguments{
  \item{gender}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years. Must be \eqn{\ge}25 and <85.}
  \item{bmi}{Body-mass index, in kg/m^2. Must be \eqn{\ge}40/2.1^2 and \eqn{\le}180/1.4^2. Within this range, values >40 are set to 40, while values <20 are set to 20.}
  \item{height}{Height, in m. Must be \eqn{\ge}1.4 and \eqn{\le}2.1.}
  \item{weight}{Weight, in kg. Must be \eqn{\ge}40 and \eqn{\le}180.}
  \item{fpg}{Fasting plasma glucose level, in mmol/L. Must be \eqn{\ge}2 and <7.}
  \item{ethnicity}{Ethnicity. Must be one of:
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
  \item{smoking}{Smoking status. Must be one of:
  \itemize{
    \item \code{"Non"} (Non-smoker)
    \item \code{"Ex"} (Ex-smoker)
    \item \code{"Light"} (Light smoker - less than 10 cigarettes per day)
    \item \code{"Moderate"} (Moderate smoker - 10 to 19 cigarettes per day)
    \item \code{"Heavy"} (Heavy smoker - 20 or more cigarettes per day).
    }
  }
  \item{townsend}{Townsend deprivation score. Must be \eqn{\ge}-7.028634577 and \eqn{\le}13.3114711.}
  \item{antipsy}{History of use of 2nd generation antipsychotics.}
  \item{steroids}{History of use of corticosteroids.}
  \item{cvd}{History of cardiovascular disease.}
  \item{gestdiab}{History of gestational diabetes.}
  \item{learndiff}{History of one or more conditions conveying learning difficulties.}
  \item{schizobipo}{History of schizophrenia or bipolar affective disorder.}
  \item{pcos}{History of polycystic ovary syndrome.}
  \item{statins}{History of use of statins.}
  \item{hypertension}{History of hypertension.}
  \item{fh_diab}{Family history of diabetes.}
}
\value{Risk (\%) of type 2 diabetes.}
\source{\url{https://qdiabetes.org/2018/src.php}}
\references{
  Hippisley-Cox, J. and Coupland, C. (2017).
  Development and validation of QDiabetes-2018 risk prediction algorithm to estimate future risk of type 2 diabetes: cohort study.
  \emph{BMJ}
  \bold{359}, j5019
}
\note{
  The limits set for Townsend deprivation scores reflect the minimum and maximum values found among \href{https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores/resource/938a1b3b-7d73-42fc-b19e-6c61bb5d2b73}{output areas} and \href{https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores/resource/0083b0bf-9241-4d73-bfdb-da33de2bd5cc}{lower layer super output areas} collated from the 2011 UK census.
}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{\code{\link{QDRA}}, \code{\link{QDRC}}}
\examples{
QDRB(gender = "Female", age = 76, height = 1.65, weight = 70, fpg = 4)
QDRB(gender = "Male", age = 55, bmi = 42, fpg = 6, ethnicity = "BlackCaribbean")
QDRB(gender = "Female", age = 65, bmi = 35, fpg = 5, smoking = "Ex", cvd = TRUE)
}
\keyword{risk}
\keyword{diabetes}
\keyword{qresearch}
\keyword{clinrisk}
