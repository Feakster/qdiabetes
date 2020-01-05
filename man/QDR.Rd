\name{QDR}
\alias{QDR}
\title{QDiabetes Risk}
\description{
Calculate the 10-year risk of an individual developing type 2 diabetes.
}
\usage{
QDR(gender, age, bmi, height, weight, fpg, hba1c, ethnicity = "WhiteNA",
    smoking = "Non", townsend = 0, antipsy = FALSE, steroids = FALSE,
    cvd = FALSE, gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE,
    pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE)
}
\arguments{
\item{gender}{Gender. Must be \code{"Male"} or \code{"Female"}.}
\item{age}{Age, in years.}
\item{bmi}{Body-mass index (BMI), in Kg/m^2.}
\item{height}{Height, in m.}
\item{weight}{Weight, in kg.}
\item{fpg}{Fasting plasma glucose (FPG) level, in mmol/L.}
\item{hba1c}{Glycated haemoglobin (HbA1c) level, in mmol/mol.}
\item{ethnicity}{Ethnicity. Must be one of: \code{"WhiteNA"}, \code{"Indian"}, \code{"Pakistani"}, \code{"Bangladeshi"}, \code{"OtherAsian"}, \code{"BlackCaribbean"}, \code{"BlackAfrican"}, \code{"Chinese"} or \code{"Other"}.}
\item{smoking}{Smoking status. Must be one of: \code{"Never"}, \code{"Ex"} or \code{"Current"}.}
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
\value{
Risk (\%) of type 2 diabetes.
}
\author{Benjamin Feakins}
\examples{
QDR(gender = "Female", age = 76, height = 1.65, weight = 70)
QDR(gender = "Male", age = 55, bmi = 42, ethnicity = "BlackCaribbean", Smoking = "Current", Townsend = 5)
QDR(gender = "Female", age = 65, bmi = 35, cvd = TRUE, fh_diab = TRUE)
}
\references{
    Hippisley-Cox J. and Coupland C. (2017)
    Development and validation of QDiabetes-2018 risk prediction algorithm to estimate future risk of type 2 diabetes: cohort study.
    \emph{BMJ},
    \bold{359}:j5019
}
\keyword{risk, diabetes, qresearch}
