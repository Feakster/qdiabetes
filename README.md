# QDiabetes <img src="man/figures/logo.png" height="139px" align="right" />

<!-- Badges -->
[![license](https://img.shields.io/badge/license-AGPL--3-blue)](https://choosealicense.com/licenses/agpl-3.0/)
[![platform-support](https://img.shields.io/badge/R-%3E%3D2.10-blue)](https://www.r-project.org/)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![github-workflow](https://github.com/Feakster/qdiabetes/workflows/R-CMD-check/badge.svg)](https://github.com/Feakster/qdiabetes/actions)
<!--[![CRANstatus](https://www.r-pkg.org/badges/version/qdiabetes)](https://cran.r-project.org/package=qdiabetes)-->
<!--[![CRANdownloads](https://cranlogs.r-pkg.org/badges/grand-total/qdiabetes)](https://cran.r-project.org/package=qdiabetes)-->

## General info

This project is an R package for calculating the risk of developing type 2 diabetes. The package uses R implementations of the QDiabetes algorithms, which were initially derived by [ClinRisk](https://clinrisk.co.uk/ClinRisk/Welcome.html) using the C++ programming language. The QDiabetes package comprises QDiabetes-2013 and [QDiabetes-2018](https://qdiabetes.org/), although older (and eventually more recent) versions of QDiabetes may be included in future releases.

## Disclaimer

ClinRisk do not support of endorse this code. End users should see the original C++ source as the &lsquo;gold standard&rsquo; open source implementation. Please note that the _QDiabetes_ R package has been created as a research tool for scientific purposes only. The _QDiabetes_ R package has not been granted Medicines and Healthcare products Regulatory Agency (MHRA) approval as a medical device, and hence, should not be used as part of any individualised risk assessment.

## History

The first QDiabetes algorithm (termed &ldquo;QDScore&rdquo; at the time) was published in 2009<sup>[1](#ref1)</sup>. In 2011, two substantial changes were made, whereby the age range permissible by the algorithm was expanded from 25&ndash;79 to 25&ndash;84, and the smoking status variable was expanded from two levels [current smoker and non-smoker] to five levels [non-smoker, ex-smoker, light smoker (1&ndash;9/day), moderate smoker (10&ndash;19/day) and heavy smoker (&ge;20/day)]. In 2012, QDiabetes-2013 was released, and from 2013 to 2016 the algorithm coefficients were periodically updated, until 2017, when the more expansive QDiabetes-2018 algorithm was published<sup>[2](#ref2)</sup>. At some point along the way, QDiabetes switched from sourcing Townsend deprivation data from the 2001 to the 2011 UK Census, however, the point at which this occurred is not clear. For now, all the can be said for certain is that the original 2009 QDScore algorithm used Townsend data from the 2001 UK Census, while QDiabetes-2018 uses Townsend data from the 2011 UK Census. More details about the variables used in QDiabetes-2013 and QDiabetes-2018 can be found in the following sections.

## QDiabetes-2013

The QDiabetes-2013 algorithm consists of two separate risk prediction models (one per gender), in which the following 11 variables are used to calculate risk:

* **Gender** [`sex`]
    - Female &mdash; `"Female"`
    - Male &mdash; `"Male"`
* **Age** [`age`], in years
* **Body mass index** [`bmi`], in kg/m<sup>2</sup>
* **Ethnicity** [`ethn`], nine categories:
    - White or not stated &mdash; `"WhiteNA"`
    - Indian &mdash; `"Indian"`
    - Pakistani &mdash; `"Pakistani"`
    - Bangladeshi &mdash; `"Bangladeshi"`
    - Black Caribbean &mdash; `"BlackCaribbean"`
    - Black African &mdash; `"BlackAfrican"`
    - Chinese &mdash; `"Chinese"`
    - Other Asian &mdash; `"OtherAsian"`
    - Other ethnic group &mdash; `"Other"`
* **Smoking status** [`smoke`], five levels:
    - Non-smoker &mdash; `"Non"`
    - Ex-smoker &mdash; `"Ex"`
    - Light smoker (1&ndash;9/day) &mdash; `"Light"`
    - Moderate smoker (10&ndash;19/day) &mdash; `"Moderate"`
    - Heavy smoker (&ge;20/day) &mdash; `"Heavy"`
* **Deprivation** [`tds`], as measured by Townsend scores, where higher values indicate higher levels of deprivation
* **Family history of diabetes in first degree relative** [`fhdm`]
* **History of treated hypertension** [`htn`], being diagnosis of hypertension and treatment with at least one hypertensive drug
* **History of cardiovascular disease** [`cvd`], defined as: ischaemic heart disease, stroke, or transient ischaemic attack
* **History of use of corticosteroids** [`ster`] listed in British National Formulary chapter 6.3.2, including oral or injections of systemic: [prednisolone](https://en.wikipedia.org/wiki/Prednisolone), [betamethasone](https://en.wikipedia.org/wiki/Betamethasone), [cortisone](https://en.wikipedia.org/wiki/Cortisone), depo-medrone, [dexamethasone](https://en.wikipedia.org/wiki/Dexamethasone), [deflazacort](https://en.wikipedia.org/wiki/Deflazacort), efcortesol, [hydrocortisone](https://en.wikipedia.org/wiki/Hydrocortisone), [methylprednisolone](https://en.wikipedia.org/wiki/Methylprednisolone), or [triamcinolone](https://en.wikipedia.org/wiki/Triamcinolone)
* **Survival time** [`surv`], being the time period over which risk of developing type-2 diabetes is to be calculated.

The QDiabetes-2013 algorithm is implemented within the `QDR2013()` function of the _QDiabetes_ package.

## QDiabetes-2018

The QDiabetes-2018 algorithm is actually six separate risk predictions models (three sub-models, subdivided by gender).

### Model A

The basic (core) model, &lsquo;model A&rsquo;, uses the same risk predictors as QDiabetes-2013, with the omission of the **Survival time** variable (in favour of a fixed 10-year survival window), and the addition of the following 6 variables:

* _**History of gestational diabetes**_ [`gdm`] _(women only)_
* _**History of polycystic ovary syndrome**_ [`pcos`] _(women only)_
* **History of learning disabilities** [`learn`]
* **History of schizophrenia or bipolar affective disorder** [`psy`]
* **History of use of statins** [`stat`]
* **History of use of second generation "atypical" antipsychotics** [`apsy`], including: [amisulpride](https://en.wikipedia.org/wiki/Amisulpride), [aripiprazole](https://en.wikipedia.org/wiki/Aripiprazole), [clozapine](https://en.wikipedia.org/wiki/Clozapine), [lurasidone](https://en.wikipedia.org/wiki/Lurasidone), [olanzapine](https://en.wikipedia.org/wiki/Olanzapine), [paliperidone](https://en.wikipedia.org/wiki/Paliperidone), [quetiapine](https://en.wikipedia.org/wiki/Quetiapine), [risperidone](https://en.wikipedia.org/wiki/Risperidone), [sertindole](https://en.wikipedia.org/wiki/Sertindole), and [zotepine](https://en.wikipedia.org/wiki/Zotepine)

Model A of the QDiabetes-2018 algorithm is implemented within the `QDR2018A()` function of the _QDiabetes_ package.

### Model B

&lsquo;Model B&rsquo;, uses the same variables as model A, with the addition of:

* **Fasting plasma glucose level** [`fpg`], in mmol/L

Model B of the QDiabetes-2018 algorithm is implemented within the `QDR2018B()` function of the _QDiabetes_ package.

### Model C

&lsquo;Model C&rsquo;, uses the same variables as model A, with the addition of:

* **Glycated haemoglobin A<sub>1c</sub> value** [`hba1c`], in mmol/mol

Model C of the QDiabetes-2018 algorithm is implemented within the `QDR2018C()` function of the _QDiabetes_ package.

## Installation

You can install the released version of _QDiabetes_ from [CRAN](https://cran.r-project.org/web/packages/QDiabetes/index.html) with:

```r
install.packages("QDiabetes")
```

Alternatively, the development version may be installed from [GitHub](https://github.com/Feakster/qdiabetes) with:

```r
if (!{"remotes" %in% installed.packages()}) install.packages("remotes")
remotes::install_github("Feakster/qdiabetes")
```

## Package ethos

In building this package, we wanted to make something that was simple to write and easy to maintain ([KISS principles](https://en.wikipedia.org/wiki/KISS_principle)), performant, but compatible with the latest and older versions of R. With this in mind, we have written this package to be as faithful to R&rsquo;s core language as possible, using minimal dependencies. Hence, you will not find any _Rcpp_ here. Instead, all functions have been written entirely in _base_ R; the only exception being the `getTDS()` function, which uses the `median()` function from the _stats_ package (although we may re-write this at some point). All other packages listed under &ldquo;Suggests&rdquo; in the DESCRIPTION file only serve to illustrate the use of _QDiabetes_ in examples or vignettes, or in testing the package. The primary factor limiting the package&rsquo;s compatibility with older versions of R is the data storage method CRAN require us to use for the data frame backend of the `getTDS()` function. Owing to the memory footprint of this object (&asymp;200MB), we need to make use of XZ compression to reduce the overall size of the package as much as possible. XZ compression was first implemented in [R version 2.10](https://cran-archive.r-project.org/bin/windows/base/old/2.10.0/NEWS.R-2.10.0).

## Note

Many of the default values used in the risk prediction functions of this package were selected to be representative of a UK population. These values are only intended to minimise the amount of typing required when using the risk prediction functions in an exploratory manner. They are unlikely to be useful in a research setting, and you would need to know the exact values to assign to all function parameters in order to make an accurate risk prediction. Hence, while you can get risk predictions from the `QDR2013()` and `QDR2018A()` functions through the specification of only `sex`, `age`, and `bmi`, you would be assuming White or missing ethnicity, non-smoking status, a Townsend deprivation score of 0, and the complete absence of any relevant medical history/conditions and concomitant drug therapies. In the case of `QDR2013()`, you would also be assuming that a 10-year risk window is desired.

## Examples

Below are some very simple examples using the _QDiabetes_ package. Note that for convenience, either BMI [`bmi`] or height [`ht`] and weight [`wt`] may be specified in any of the risk prediction functions in this package.

In the interest of making life a little easier, a `getTDS()` helper function has been added to the package, which uses a lookup table to obtain Townsend deprivation scores from full or partial UK postcodes.

```r
### Load Package Namespace ###
library(QDiabetes)

### Simple Usage ###
QDR2013(sex = "Female", age = 35, bmi = 25)
# [1] 0.6324508
QDR2018A(sex = "Male", age = 45, bmi = 35)
# [1] 9.88593
QDR2018B(sex = "Female", age = 65, bmi = 30, fpg = 6)
# [1] 18.43691
QDR2018C(sex = "Male", age = 25, bmi = 40, hba1c = 42)
# [1] 8.226301

### Making Use of the getTDS() Helper Function ###
getTDS("OX2 6GG")
# [1] 2.022583
QDR2013(sex = "Female", age = 41, ht = 1.65, wt = 60, tds = getTDS("OX3 9DU"))
# [1] 0.5004499
QDR2018A(sex = "Male", age = 33, bmi = 26, tds = getTDS("OX3 7LF"))
# [1] 0.6472644

### Making Use of Vectorisation ###
getTDS(c("OX3 7LF", "OX2 6NW", "OX2 6GG", "OX1 4AR"))
#    OX37LF    OX26NW    OX26GG    OX14AR
# -1.032394  1.640422  2.022583  2.309777
QDR2013(sex = "Female", age = 35, bmi = seq(20, 40, 5))
#        20        25        30        35        40
# 0.1801226 0.6324508 1.7885233 3.8983187 6.2964702
QDR2018A(sex = "Female", age = seq(25, 75, 10), bmi = 35)
#       25        35        45        55        65        75
# 1.085179  2.921454  5.893499  9.082108 10.713717  9.567516
QDR2018B(sex = "Male", age = 65, bmi = 35, fpg = 2:6)
#         2          3          4          5          6
# 0.9123063  0.5911511  1.8416081  7.8554831 30.8096968
QDR2018C(sex = "Female", age = 80, bmi = 28, hba1c = seq(15, 45, 5))
#          15           20           25           30           35           40           45
# 0.008084487  0.033019655  0.121238952  0.412396004  1.320727239  4.005759509 11.409509026

### Data Frame Usage ###
data(dat_qdr) # Synthetic sample data

## Using base R ##
dat_qdr[["risk"]] <- with(dat_qdr, QDR2013(sex = sex,
                                           age = age,
                                           ht = ht,
                                           wt = wt,
                                           ethn = ethn,
                                           smoke = smoke,
                                           tds = tds,
                                           htn = htn,
                                           cvd = cvd,
                                           ster = ster))

## Using dplyr ##
library(dplyr)
df_qdr <- as_tibble(dat_qdr)
df_qdr <- df_qdr %>%
  mutate(risk = QDR2013(sex = sex,
                        age = age,
                        ht = ht,
                        wt = wt,
                        ethn = ethn,
                        smoke = smoke,
                        tds = tds,
                        htn = htn,
                        cvd = cvd,
                        ster = ster))

## Using data.table ##
library(data.table)
dt_qdr <- as.data.table(dat_qdr)
dt_qdr[, risk := QDR2013(sex = sex,
                         age = age,
                         ht = ht,
                         wt = wt,
                         ethn = ethn,
                         smoke = smoke,
                         tds = tds,
                         htn = htn,
                         cvd = cvd,
                         ster = ster)]
```

## Known issues

See [Issues](https://github.com/Feakster/qdiabetes/issues) on the QDiabetes GitHub repository.

## Similar packages

* [QRISK3](https://github.com/YanLiUK/QRISK3): An R implementation of ClinRisk&rsquo;s [QRISK3](https://www.qrisk.org/three/) risk prediction algorithms.

## Funding

This project was funded by the National Institute for Health Research (NIHR) School for Primary Care Research (SPCR) [project number: [412](https://www.spcr.nihr.ac.uk/projects/412-quantifying-the-risk-of-type-2-diabetes-across-the-uk)]. The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care.

<img src="man/figures/nihr-logo.png" height="40px" align="float:left" />

## References

<a name="ref1">1</a>: [Hippisley-Cox J, Coupland C, Robson J, Sheikh A & Brindle P. (2009). Predicting risk of type 2 diabetes in England and Wales: prospective derivation and validation of QDScore. _BMJ_ **338**, b880](https://doi.org/10.1136/bmj.b880)

<a name="ref2">2</a>: [Hippisley-Cox J & Coupland C. (2017). Development and validation of QDiabetes-2018 risk prediction algorithm to estimate future risk of type 2 diabetes: cohort study. _BMJ_ **359**, j5019](https://doi.org/10.1136/bmj.j5019)
