\name{QDR2013}
\alias{QDR2013}
\alias{QDiabetes-2013}
\title{QDiabetes-2013 Risk Calculator}
\description{
  Calculate the risk of developing type 2 diabetes, using the QDiabetes-2013 algorithm.
}
\usage{
QDR2013(sex, age, bmi, ht, wt, ethn = "WhiteNA", smoke = "Non", tds = 0,
        fhdm = FALSE, htn = FALSE, cvd = FALSE, ster = FALSE, surv = 10L)
}
\arguments{
  \item{sex}{Gender. Must be \code{"Male"} or \code{"Female"}.}
  \item{age}{Age, in years. Must be \eqn{\ge}25 and <85.}
  \item{bmi}{Body-mass index, in kg/m^2. Must be \eqn{\ge}40/2.1^2 and \eqn{\le}180/1.4^2. Within this range, values >40 are set to 40, while values <20 are set to 20.}
  \item{ht}{Height, in m. Must be \eqn{\ge}1.4 and \eqn{\le}2.1.}
  \item{wt}{Weight, in kg. Must be \eqn{\ge}40 and \eqn{\le}180.}
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
  \item{tds}{Townsend deprivation score. Must be \eqn{\ge}-7 and \eqn{\le}11.}
  \item{fhdm}{Family history of diabetes in 1st degree relative.}
  \item{htn}{History of hypertension.}
  \item{cvd}{History of cardiovascular disease.}
  \item{ster}{History of use of corticosteroids.}
  \item{surv}{Time point at which the Kaplan-Meier cumulative survival estimate is used to calculate risk. Must be an integer value between 1 and 10.}
}
\value{Risk (\%) of type 2 diabetes.}
\source{\url{https://svn.clinrisk.co.uk/opensource/qdiabetes/standard/}}
\note{
  The QDiabetes \R package has been created as a research tool for scientific purposes only.
  The QDiabetes \R package has not been granted Medicines and Healthcare products Regulatory Agency (MHRA) approval as a medical device, and hence, should not be used as part of any individualised risk assessment.
}
\author{Benjamin G. Feakins \email{benjamin.feakins@ndph.ox.ac.uk}}
\seealso{
\code{\link{QDR2018A}} - For estimating the 10-year risk using QDiabetes-2018 (Model A).

\code{\link{QDR2018B}} - For estimating the 10-year risk using QDiabetes-2018 (Model B).

\code{\link{QDR2018C}} - For estimating the 10-year risk using QDiabetes-2018 (Model C).
}
\examples{
## Simple usage
QDR2013(sex = "Female", age = 76, ht = 1.65, wt = 70)
QDR2013(sex = "Male", age = seq(25, 80, 5), bmi = 40, ethn = "Other", tds = 5)
QDR2013(sex = "Female", age = 65, bmi = 35, smoke = c("Non", "Ex", "Light"), fhdm = TRUE)

## Using postcodes to estimate Townsend deprivation scores
QDR2013(sex = "Male", age = 65, bmi = 40, tds = getTDS("OX3 7LF"))
QDR2013(sex = "Female", age = 60, bmi = 35, tds = getTDS(c("OX2 6NW", "OX2 6GG")))

## Data frame usage
data(dat_qdr)
with(dat_qdr, QDR2013(sex = sex, age = age, bmi = bmi))

## Plotting outputs
age <- seq(25, 80, 5)
risk_m <- QDR2013(sex = "Male", age = age, bmi = 40)
risk_f <- QDR2013(sex = "Female", age = age, bmi = 40)

oldpar <- par(no.readonly = TRUE)
par(cex = 0.8, cex.sub = 0.8)
plot.new()
plot.window(xlim = range(age), ylim = range(pretty(c(risk_m, risk_f))))
axis(1, at = age)
axis(2, at = pretty(c(risk_m, risk_f)))
title(
  main = "Diabetes 10-year risk with age:\\nQDiabetes-2013",
  sub = expression("BMI set to"~40*kg/m^2*", other variables set to defaults"),
  xlab = "Age (years)", ylab = "Risk (\%)"
)
lines(age, risk_m, type = "b", col = "navy", lwd = 1.5)
lines(age, risk_f, type = "b", col = "red3", lwd = 1.5)
legend("bottomright", legend = c("Male", "Female"), col = c("navy", "red3"), lty = 1, bty = "n")
par(oldpar)
}
\concept{risk}
\concept{diabetes}
\concept{QResearch}
\concept{ClinRisk}
\keyword{misc}
