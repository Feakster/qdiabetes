\name{QDRA}
\alias{QDRA}
\title{QDiabetes-2018 Risk Calculator (A-Variant)}
\description{
  Calculate the 10-year risk of an individual developing type 2 diabetes, using the A-variant of the QDiabetes-2018 algorithm.
  This variant does not use fasting plasma glucose or glcated haemoglobin A1c.
}
\usage{
QDRA(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL,
     ethnicity = "WhiteNA", smoking = "Non",
     townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE,
     gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE,
     pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE)
}
\arguments{
  \item{gender}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years.}
  \item{bmi}{Body-mass index, in kg/m^2.}
  \item{height}{Height, in m.}
  \item{weight}{Weight, in kg.}
  \item{ethnicity}{Ethnicity. Must be one of: \code{"WhiteNA"}, \code{"Indian"}, \code{"Pakistani"}, \code{"Bangladeshi"}, \code{"OtherAsian"}, \code{"BlackCaribbean"}, \code{"BlackAfrican"}, \code{"Chinese"} or \code{"Other"}.}
  \item{smoking}{Smoking status. Must be one of: \code{"Non"}, \code{"Ex"}, \code{"Light"}, \code{"Moderate"} or \code{"Heavy"}.}
  \item{townsend}{Townsend deprivation score.}
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
\author{Benjamin Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{\code{\link{QDR}}, \code{\link{QDRB}}, \code{\link{QDRC}}}
\examples{
QDRA(gender = "Female", age = 76, height = 1.65, weight = 70)
QDRA(gender = "Male", age = 55, bmi = 42, ethnicity = "BlackCaribbean", townsend = 5)
QDRA(gender = "Female", age = 65, bmi = 35, smoking = "Ex", cvd = TRUE, fh_diab = TRUE)
}
\keyword{risk}
\keyword{diabetes}
\keyword{qresearch}
\keyword{clinrisk}
