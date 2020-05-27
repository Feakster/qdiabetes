\name{QDiabetes-package}
\alias{QDiabetes-package}
\alias{QDiabetes}
\docType{package}
\title{Type 2 Diabetes Risk Calculator}
\description{
  This package calculates the risk of developing type 2 diabetes using risk prediction algorithms, which were initially derived by \href{https://clinrisk.co.uk/ClinRisk/Welcome.html}{ClinRisk}. Currently, these include QDiabetes-2013 and \href{https://qdiabetes.org/}{QDiabetes-2018}, although older (and eventually more recent) versions of QDiabetes will be included in future releases.
}
\details{
  The package consistes of four risk prediction functions for use in estimating the risk of developing type 2 diabetes:
  \itemize{
    \item{\code{\link{QDR2013}} - For estimating the 1--10-year risk using QDiabetes-2013.}
    \item{\code{\link{QDR2018A}} - For estimating the 10-year risk using QDiabetes-2018 (Model A).}
    \item{\code{\link{QDR2018B}} - For estimating the 10-year risk using QDiabetes-2018 (Model B).}
    \item{\code{\link{QDR2018C}} - For estimating the 10-year risk using QDiabetes-2018 (Model C).}
  }

  The following variables are used by each risk prediction function present in this package:

  \tabular{lrrrrrr}{
    \bold{Description}                                      \tab  \bold{Variable} \tab  \bold{Type} \tab  \bold{\code{QDR2013}} \tab  \bold{\code{QDR2018A}}  \tab  \bold{\code{QDR2018B}}  \tab  \bold{\code{QDR2018C}}  \cr
    Gender                                                  \tab  \code{sex}      \tab  character   \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Age                                                     \tab  \code{age}      \tab  double      \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Body mass index (BMI)                                   \tab  \code{bmi}      \tab  double      \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Ethnicity                                               \tab  \code{ethn}     \tab  character   \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Smoking status                                          \tab  \code{smoke}    \tab  character   \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Deprivation                                             \tab  \code{tds}      \tab  double      \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    Fasting plasma glucose (FPG)                            \tab  \code{fpg}      \tab  double      \tab                        \tab                          \tab  x                       \tab                          \cr
    Glycated haemoglobin (HbA1c)                            \tab  \code{hba1c}    \tab  double      \tab                        \tab                          \tab                          \tab  x                       \cr
    Family history of diabetes                              \tab  \code{fhdm}     \tab  logical     \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    History of treated hypertension                         \tab  \code{htn}      \tab  logical     \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    History of cardiovascular disease                       \tab  \code{cvd}      \tab  logical     \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    History of gestational Diabetes                         \tab  \code{gdm}      \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    History of polycystic ovary syndrome                    \tab  \code{pcos}     \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    History of learning difficulties                        \tab  \code{learn}    \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    History of schizophrenia or bipolar affective disorder  \tab  \code{psy}      \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    History of corticosteroid use                           \tab  \code{ster}     \tab  logical     \tab  x                     \tab  x                       \tab  x                       \tab  x                       \cr
    History of statin use                                   \tab  \code{stat}     \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    History of use of 2nd generation antipsychotics         \tab  \code{apsy}     \tab  logical     \tab                        \tab  x                       \tab  x                       \tab  x                       \cr
    Survival time                                           \tab  \code{surv}     \tab  integer     \tab  x                     \tab                          \tab                          \tab
  }

  \emph{NB: height (\code{ht}) and weight (\code{wt}) may be specified in place of body mass index (\code{bmi}) in any of the above functions.}
  
  As per \R's general coding sytax, factor or character values may be passed to any risk prediction function parameter where a character value is expected, so long as the factor variable label matches one of the expected character strings. Similarly, \code{0} or \code{1} many be used in place of \code{FALSE} or \code{TRUE} for any function parameter where a logical value is expected.
}
\author{Benjamin G. Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\references{
  Hippisley-Cox, J., Coupland, C., Robson, J., Sheikh, A. and Brindle, P. (2009).
  Predicting risk of type 2 diabetes in England and Wales: prospective derivation and validation of QDScore.
  \emph{BMJ}
  \bold{338}, b880.
  \doi{10.1136/bmj.b880}

  Hippisley-Cox, J. and Coupland, C. (2017).
  Development and validation of QDiabetes-2018 risk prediction algorithm to estimate future risk of type 2 diabetes: cohort study.
  \emph{BMJ}
  \bold{359}, j5019.
  \doi{10.1136/bmj.j5019}
}
\section{Disclaimer}{
  ClinRisk do not support of endorse this code. End users should see the original C source as the 'gold standard' open source implementation. Please note that the QDiabetes \R package has been created as a research tool for scientific purposes only. The QDiabetes \R package has not been granted Medicines and Healthcare products Regulatory Agency (MHRA) approval as a medical device, and hence, should not be used as part of any individualised risk assessment.
}
\section{Funding}{
  This project was funded by the National Institute for Health Research (NIHR) School for Primary Care Research (SPCR) [project number: \href{https://www.spcr.nihr.ac.uk/projects/412-quantifying-the-risk-of-type-2-diabetes-across-the-uk}{412}]. The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care.
}
\note{
  Many of the default values used in the risk prediction functions of this package were selected to be representative of a UK population. These values are only intended to minimise the amount of typing required when using the risk prediction functions in an exploratory manner. They are unlikely to be useful in a research setting, and you would need to know the exact values to assign to all function parameters in order to make accurate risk predictions. Hence, while you can get risk preditions from the \code{\link{QDR2013}} and \code{\link{QDR2018A}} functions through the specification of only \code{sex}, \code{age}, and \code{bmi}, you would be assuming White or missing ethnicity, non-smoking status, a Townsend deprivation score of 0, and the complete absence of any relevant medical history/conditions and concommitant drug therapies. In the case of \code{\link{QDR2013}}, you would also be assuming that a 10-year risk window is desired.
}
\seealso{
  \code{\link{getTDS}} - For looking up Townsend deprivation scores from UK postcodes.
  
  \code{\link{dat_qdr}} - QDiabetes sample dataset.
}
\examples{
### Simple usage
## QDiabetes-2013
QDR2013(sex = "Female", age = 76, ht = 1.65, wt = 70)
QDR2013(sex = "Male", age = seq(25, 80, 5), bmi = 40, ethn = "Other", tds = 5)
## QDiabetes-2018
# Model A
QDR2018A(sex = "Female", age = 76, ht = 1.65, wt = 70)
QDR2018A(sex = "Male", age = seq(25, 80, 5), bmi = 40, ethn = "Other", tds = 5)
# Model B (inc. FPG)
QDR2018B(sex = "Female", age = 76, ht = 1.65, wt = 70, fpg = 4)
QDR2018B(sex = "Male", age = 55, bmi = 40, fpg = 2:6, ethn = "BlackCaribbean")
# Model C (inc. HbA1c)
QDR2018C(sex = "Female", age = 76, ht = 1.65, wt = 70, hba1c = 25)
QDR2018C(sex = "Male", age = 55, bmi = 40, hba1c = seq(15, 40, 5), ethn = "Chinese")

### Using postcodes to estimate Townsend deprivation scores
## QDiabetes-2013
QDR2013(sex = "Male", age = 65, bmi = 40, tds = getTDS("OX3 7LF"))
QDR2013(sex = "Female", age = 60, bmi = 35, tds = getTDS(c("OX2 6NW", "OX2 6GG")))
## QDiabetes-2018
# Model A
QDR2018A(sex = "Male", age = 65, bmi = 40, tds = getTDS("OX3 7LF"))
QDR2018A(sex = "Female", age = 60, bmi = 35, tds = getTDS(c("OX2 6NW", "OX2 6GG")))
# Model B (inc. FPG)
QDR2018B(sex = "Male", age = 65, bmi = 40, fpg = 6, tds = getTDS("OX3 7LF"))
QDR2018B(sex = "Female", age = 60, bmi = 35, fpg = 6, tds = getTDS(c("OX2 6NW", "OX2 6GG")))
# Model C (inc. HbA1c)
QDR2018C(sex = "Male", age = 65, bmi = 40, hba1c = 42, tds = getTDS("OX3 7LF"))
QDR2018C(sex = "Female", age = 60, bmi = 35, hba1c = 42, tds = getTDS(c("OX2 6NW", "OX2 6GG")))
}
\concept{risk}
\concept{diabetes}
\concept{QResearch}
\concept{ClinRisk}
\keyword{package}
