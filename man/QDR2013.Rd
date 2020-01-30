\name{QDR2013}
\alias{QDR2013}
\title{QDiabetes-2013 Risk Calculator}
\description{
  Calculate the risk of developing type 2 diabetes, using the QDiabetes-2013 algorithm.
}
\usage{
QDR2013(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL,
        ethnicity = "WhiteNA", smoking = "Non", townsend = 0, steroids = FALSE,
        cvd = FALSE, hypertension = FALSE, fh_diab = FALSE, surv = 10L)
}
\arguments{
  \item{gender}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years. Must be \eqn{\ge}25 and <85.}
  \item{bmi}{Body-mass index, in kg/m^2. Must be \eqn{\ge}40/2.1^2 and \eqn{\le}180/1.4^2. Within this range, values >40 are set to 40, while values <20 are set to 20.}
  \item{height}{Height, in m. Must be \eqn{\ge}1.4 and \eqn{\le}2.1.}
  \item{weight}{Weight, in kg. Must be \eqn{\ge}40 and \eqn{\le}180.}
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
  \item{townsend}{Townsend deprivation score. Must be \eqn{\ge}-7 and \eqn{\le}11.}
  \item{steroids}{History of use of corticosteroids.}
  \item{cvd}{History of cardiovascular disease.}
  \item{hypertension}{History of hypertension.}
  \item{fh_diab}{Family history of diabetes in 1st degree relative.}
  \item{surv}{Time point at which the Kaplan-Meier cumulative survival estimate is used to calculate risk. Must be an integer value between 1 and 10.}
}
\value{Risk (\%) of type 2 diabetes.}
\source{\url{https://svn.clinrisk.co.uk/opensource}}
\references{
  Hippisley-Cox, J., Coupland, C., Robson, J., Sheikh, A. and Brindle, P. (2009).
  Predicting risk of type 2 diabetes in England and Wales: prospective derivation and validation of QDScore.
  \emph{BMJ}
  \bold{338}, b880
}
\author{Benjamin G Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{\code{\link{QDR2018A}}, \code{\link{QDR2018B}}, \code{\link{QDR2018C}}}
\examples{
## Simple usage
QDR2013(gender = "Female", age = 76, height = 1.65, weight = 70)
QDR2013(gender = "Male", age = seq(25, 80, 5), bmi = 40, ethnicity = "Other", townsend = 5)
QDR2013(gender = "Female", age = 65, bmi = 35, smoking = c("Non", "Ex", "Light"), fh_diab = TRUE)

## Using postcodes to estimate Townsend deprivation scores
QDR2013(gender = "Male", age = 65, bmi = 40, townsend = getTDS("OX3 7LF"))
QDR2013(gender = "Female", age = 60, bmi = 35, townsend = getTDS(c("OX2 6NW", "OX2 6GG")))
}
\keyword{risk}
\keyword{diabetes}
\keyword{qresearch}
\keyword{clinrisk}
