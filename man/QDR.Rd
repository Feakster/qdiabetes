\name{QDR}
\alias{QDR}
\title{QDiabetes-2018 Risk Calculator}
\description{
  Calculate the 10-year risk of an individual developing type 2 diabetes, using the QDiabetes-2018 algorithm.
}
\usage{
QDR(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL,
    fpg = NULL, hba1c = NULL, ethnicity = "WhiteNA", smoking = "Non",
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
  \item{fpg}{Fasting plasma glucose level, in mmol/L.}
  \item{hba1c}{Glycated haemoglobin A1c level, in mmol/mol.}
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
\details{
%%  ~~ If necessary, more details than the description above ~~
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
The \code{QDR} function replicates the behaviour of the interactive risk calculator present on the \href{https://qdiabetes.org/2018/}{QDiabetes-2018 website}. As such, the \sQuote{best} algorithm is selected for any given set of parameters. Hence, as per the website:
  \itemize{
    \item{Where neither fasting plasma glucose nor glcated haemogobin A1c are provided, the A-variant of the algorithm (\code{QDRA}) is used.}
    \item{Where fasting plasma glucose is provided but glcated haemoglobin A1c not provided, the B-variant of the algorithm (\code{QDRB}) is used.}
    \item{Where glycated haemoglobin A1c is provided but fasting plasma glucose not provided, the C-variant of the algorithm (\code{QDRC}) is used.}
  }
Should both fasting plasma glucose and glycated haemoglobin A1c be provided then the B-variant of the algorithm (\code{QDRB}) is used. However, unlike the website, a warning message will be displayed alongside the value.
}
\author{Benjamin Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{\code{\link{QDRA}}, \code{\link{QDRB}}, \code{\link{QDRC}}}
\examples{
QDR(gender = "Female", age = 76, height = 1.65, weight = 70)
QDR(gender = "Male", age = 55, bmi = 42, ethnicity = "BlackCaribbean", townsend = 5)
QDR(gender = "Female", age = 65, bmi = 35, smoking = "Ex", cvd = TRUE, fh_diab = TRUE)
}
\keyword{risk}
\keyword{diabetes}
\keyword{qresearch}
\keyword{clinrisk}
