\name{QDR2018B}
\alias{QDR2018B}
\alias{QDiabetes-2018B}
\title{QDiabetes-2018 Risk Calculator (B-Variant)}
\description{
  Calculate the 10-year risk of developing type 2 diabetes, using the B-variant of the QDiabetes-2018 algorithm.
  This variant includes all risk predictors present in the A-variant, with the addition of fasting plasma glucose.
}
\usage{
QDR2018B(sex, age, bmi, ht, wt, fpg, ethn = "WhiteNA", smoke = "Non", tds = 0,
         fhdm = FALSE, htn = FALSE, cvd = FALSE, gdm = FALSE, pcos = FALSE,
         learn = FALSE, psy = FALSE, ster = FALSE, stat = FALSE, apsy = FALSE)
}
\arguments{
  \item{sex}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years. Must be \eqn{\ge}25 and <85.}
  \item{bmi}{Body-mass index, in kg/m^2. Must be \eqn{\ge}40/2.1^2 and \eqn{\le}180/1.4^2. Within this range, values >40 are set to 40, while values <20 are set to 20.}
  \item{ht}{Height, in m. Must be \eqn{\ge}1.4 and \eqn{\le}2.1.}
  \item{wt}{Weight, in kg. Must be \eqn{\ge}40 and \eqn{\le}180.}
  \item{fpg}{Fasting plasma glucose level, in mmol/L. Must be \eqn{\ge}2 and <7.}
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
\code{\link{QDR2013}} - For estimating the 1--10-year risk using QDiabetes-2013.

\code{\link{QDR2018A}} - For estimating the 10-year risk using QDiabetes-2018 (Model A).

\code{\link{QDR2018C}} - For estimating the 10-year risk using QDiabetes-2018 (Model C).
}
\examples{
## Simple usage
QDR2018B(sex = "Female", age = 76, ht = 1.65, wt = 70, fpg = 4)
QDR2018B(sex = "Male", age = 55, bmi = 40, fpg = 2:6, ethn = "BlackCaribbean")
QDR2018B(sex = "Female", age = 65, bmi = 35, fpg = 5, smoke = "Ex", cvd = c(FALSE, TRUE))

## Using postcodes to estimate Townsend deprivation scores
QDR2018B(sex = "Male", age = 65, bmi = 40, fpg = 6, tds = getTDS("OX3 7LF"))
QDR2018B(sex = "Female", age = 60, bmi = 35, fpg = 6, tds = getTDS(c("OX2 6NW", "OX2 6GG")))

## Data frame usage
data(dat_qdr)
with(dat_qdr, QDR2018B(sex = sex, age = age, bmi = bmi, fpg = fpg))

## Plotting outputs
fpg <- seq(2, 6.5, length.out = 10)
risk_m <- QDR2018B(sex = "Male", age = 65, bmi = 40, fpg = fpg)
risk_f <- QDR2018B(sex = "Female", age = 65, bmi = 40, fpg = fpg)

\dontshow{oldpar <- par(no.readonly = TRUE)}
par(cex = 0.8, cex.sub = 0.8)
plot.new()
plot.window(xlim = range(fpg), ylim = range(pretty(c(risk_m, risk_f))))
axis(1, at = fpg)
axis(2, at = pretty(c(risk_m, risk_f)))
title(
  main = "Diabetes 10-year risk with fasting plasma glucose level:\\nQDiabetes-2018 (B-Variant)",
  sub = expression("Age set to 65 years, BMI set to"~40*kg/m^2*", other variables set to defaults"),
  xlab = "Fasting Plasma Glucose (mmol/L)", ylab = "Risk (\%)"
)
lines(fpg, risk_m, type = "b", col = "navy", lwd = 1.5)
lines(fpg, risk_f, type = "b", col = "red3", lwd = 1.5)
legend("bottomright", legend = c("Male", "Female"), col = c("navy", "red3"), lty = 1, bty = "n")
\dontshow{par(oldpar)}
}
\concept{risk}
\concept{diabetes}
\concept{QResearch}
\concept{ClinRisk}
\keyword{misc}
