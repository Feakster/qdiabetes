\name{QDR2018C}
\alias{QDR2018C}
\alias{QDiabetes-2018C}
\title{QDiabetes-2018 Risk Calculator (C-Variant)}
\description{
  Calculate the 10-year risk of developing type 2 diabetes, using the C-variant of the QDiabetes-2018 algorithm.
  This variant includes all risk predictors present in the A-variant, with the addition of glycated haemoglobin A1c.
}
\usage{
QDR2018C(sex = NULL, age = NULL, bmi = NULL, ht = NULL, wt = NULL, hba1c = NULL,
         ethn = "WhiteNA", smoke = "Non", tds = 0, fhdm = FALSE, htn = FALSE,
         cvd = FALSE, gdm = FALSE, pcos = FALSE, learn = FALSE, psy = FALSE,
         ster = FALSE, stat = FALSE, apsy = FALSE)
}
\arguments{
  \item{sex}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years. Must be \eqn{\ge}25 and <85.}
  \item{bmi}{Body-mass index, in kg/m^2. Must be \eqn{\ge}40/2.1^2 and \eqn{\le}180/1.4^2. Within this range, values >40 are set to 40, while values <20 are set to 20.}
  \item{ht}{Height, in m. Must be \eqn{\ge}1.4 and \eqn{\le}2.1.}
  \item{wt}{Weight, in kg. Must be \eqn{\ge}40 and \eqn{\le}180.}
  \item{hba1c}{Glycated haemoglobin A1c level, in mmol/mol. Must be \eqn{\ge}15 and <48.}
  \item{ethn}{Ethnicity. Must be one of:
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
  \item{smoke}{Smoking status. Must be one of:
  \itemize{
    \item \code{"Non"} (Non-smoker)
    \item \code{"Ex"} (Ex-smoker)
    \item \code{"Light"} (Light smoker - less than 10 cigarettes per day)
    \item \code{"Moderate"} (Moderate smoker - 10 to 19 cigarettes per day)
    \item \code{"Heavy"} (Heavy smoker - 20 or more cigarettes per day).
    }
  }
  \item{tds}{Townsend deprivation score. Must be \eqn{\ge}-8 and \eqn{\le}14.}
  \item{fhdm}{Family history of diabetes in 1st degree relative.}
  \item{htn}{History of hypertension.}
  \item{cvd}{History of cardiovascular disease.}
  \item{gdm}{History of gestational diabetes.}
  \item{pcos}{History of polycystic ovary syndrome.}
  \item{learn}{History of one or more conditions conveying learning difficulties.}
  \item{psy}{History of schizophrenia or bipolar affective disorder.}
  \item{ster}{History of use of corticosteroids.}
  \item{stat}{History of use of statins.}
  \item{apsy}{History of use of 2nd generation antipsychotics.}
}
\value{Risk (\%) of type 2 diabetes.}
\source{\url{https://qdiabetes.org/2018/src.php}}
\references{
  Hippisley-Cox, J. and Coupland, C. (2017).
  Development and validation of QDiabetes-2018 risk prediction algorithm to estimate future risk of type 2 diabetes: cohort study.
  \emph{BMJ}
  \bold{359}, j5019.
  \doi{10.1136/bmj.j5019}
}
\note{
  The QDiabetes \R package has been created as a research tool for scientific purposes only.
  The QDiabetes \R package has not been granted Medicines and Healthcare products Regulatory Agency (MHRA) approval as a medical device, and hence, should not be used as part of any individualised risk assessment.
}
\author{Benjamin G. Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{
\code{\link{QDR2013}} - For estimating the 1–10-year risk using QDiabetes-2013.

\code{\link{QDR2018A}} - For estimating the 10-year risk using QDiabetes-2018 (Model A).

\code{\link{QDR2018B}} - For estimating the 10-year risk using QDiabetes-2018 (Model B).
}
\examples{
## Simple usage
QDR2018C(sex = "Female", age = 76, ht = 1.65, wt = 70, hba1c = 25)
QDR2018C(sex = "Male", age = 55, bmi = 40, hba1c = seq(15, 40, 5), ethn = "Chinese")
QDR2018C(sex = "Female", age = 65, bmi = 35, hba1c = 30, smoke = "Ex", fhdm = c(FALSE, TRUE))

## Using postcodes to estimate Townsend deprivation scores
QDR2018C(sex = "Male", age = 65, bmi = 40, hba1c = 42, tds = getTDS("OX3 7LF"))
QDR2018C(sex = "Female", age = 60, bmi = 35, hba1c = 42, tds = getTDS(c("OX2 6NW", "OX2 6GG")))

## Data frame usage
data(dat_qdr)
with(dat_qdr, QDR2018C(sex = sex, age = age, bmi = bmi, hba1c = hba1c))

## Plotting outputs
hba1c <- seq(15, 42, length.out = 10)
risk_m <- QDR2018C(sex = "Male", age = 65, bmi = 40, hba1c = hba1c)
risk_f <- QDR2018C(sex = "Female", age = 65, bmi = 40, hba1c = hba1c)

par(cex = 0.8)
plot.new()
plot.window(xlim = range(hba1c), ylim = range(pretty(c(risk_m, risk_f))))
axis(1, at = hba1c)
axis(2, at = pretty(c(risk_m, risk_f)))
title(xlab = expression("Haemoglobin A"["1c"]~"(mmol/mol)"), ylab = "Risk (\%)")
lines(hba1c, risk_m, type = "b", col = "navy", lwd = 1.5)
lines(hba1c, risk_f, type = "b", col = "red3", lwd = 1.5)
legend("bottomright", legend = c("Male", "Female"), col = c("navy", "red3"), lty = 1, bty = "n")
}
\concept{risk}
\concept{diabetes}
\concept{QResearch}
\concept{ClinRisk}
\keyword{misc}
