#========================#
#                        #
#### QDR2018B() TESTS ####
#                        #
#========================#

### Notes ###
# - 0.1 tolerance allowed in risk outputs to account for different rounding standards.
# - Townsend score currently not easily testable.

### Parameters ###
tol <- 0.1 # Tolerance
tiny <- 1e-8 # Small number

### Test Data ###
dat_test <- expand.grid(
  age = seq(25, 85 - tiny, length.out = 50),
  bmi = seq(20, 40, length.out = 50),
  fpg = seq(2, 7 - tiny, length.out = 50),
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2018B <- function(...){round(QDR2018B(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018B <- function(...){rQDR2018B(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5), "double")
expect_length(gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018B(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  townsend = -7.028634577))

dat_test[["risk_max"]] <- with(dat_test, QDR2018B(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  ethnicity = "Bangladeshi",
                                                  smoking = "Heavy",
                                                  townsend = 13.3114711,
                                                  antipsy = T,
                                                  steroids = T,
                                                  cvd = T,
                                                  gestdiab = T,
                                                  learndiff = T,
                                                  schizobipo = T,
                                                  pcos = T,
                                                  statins = T,
                                                  hypertension = T,
                                                  fh_diab = T))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018B-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018B-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018B-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018B-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018B(age = 60, fpg = 4.5),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Female [is.null(gender)]")

## Age ##
expect_error(gQDR2018B(fpg = 4.5),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Female [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(tQDR2018B(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018B-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018B(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018B-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018B(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018B-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018B(),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Female [is.null(fpg)]")
rm(tQDR2018B)

## HbA1c ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDRA-Female [!is.null(hba1c)]")
rm(tQDR2018B)

### Boundaries ###
## Age ##
tQDR2018B <- function(...){gQDR2018B(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDR2018B(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018B-Female [age < 25]")
expect_error(tQDR2018B(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018B-Female [age >= 85]]")
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(tQDR2018B(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018B-Female [bmi < 40/2.1^2]")
expect_error(tQDR2018B(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018B-Female [bmi > 180/1.4^2]")
expect_warning(tQDR2018B(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018B-Female [bmi < 20]")
expect_warning(tQDR2018B(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018B-Female [bmi > 40]")
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(...){gQDR2018B(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018B-Female [height < 1.4]")
expect_error(tQDR2018B(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018B-Female [height > 2.1]")
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDR2018B(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018B-Female [weight < 40]")
expect_error(tQDR2018B(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018B-Female [weight > 180]")
rm(tQDR2018B)

## Townsend ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018B-Female [townsend < -7.028634577]")
expect_error(tQDR2018B(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018B-Female [townsend > 13.3114711]")
rm(tQDR2018B)

## Binary Variables ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(gestdiab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(pcos = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018B)

### Numerical Values ###
## Age ##
tQDR2018B <- function(x){gQDR2018B(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.8, 0.9, 1, 1.1, 1, 0.9, 0.7)
names(risk_web) <- vec_age

risk_fun <- tQDR2018B(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(x){gQDR2018B(bmi = x, age = 60, fpg = 4.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(2.8, 2.8, 2.3, 1.6, 1.1, 0.8, 0.6, 0.4)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018B(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(x){gQDR2018B(height = x, age = 60, weight = 90, fpg = 4.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(2.8, 2.8, 2.3, 1.6, 1.1, 0.8, 0.6, 0.4)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018B(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(x){gQDR2018B(weight = x, age = 60, height = 1.83, fpg = 4.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.4, 0.4, 0.7, 1.4, 2.4, 2.8, 2.8, 2.8)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018B(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(x){gQDR2018B(fpg = x, age = 60, height = 1.83, weight = 90)}
vec_fpg <- 2:6
risk_web <- c(0.5, 0.1, 0.4, 2.5, 13)
names(risk_web) <- vec_fpg

risk_fun <- tQDR2018B(vec_fpg)
names(risk_fun) <- vec_fpg

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(fpg)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

### Categorical Variables ###
## Ethnicity ##
tQDR2018B <- function(x){gQDR2018B(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDR2018B(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018B-Female [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1, 2.7, 3.5, 4.5, 2.7, 1.7, 1.7, 2.2, 1.5)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018B(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(etnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Smoking ##
tQDR2018B <- function(x){gQDR2018B(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDR2018B(x = "Maybe"),
             regexpr = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018B-Female [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1, 1.1, 1.3, 1.4, 1.6)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018B(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

### Binary Variables ###
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDR2018B(antipsy = T), 1.4, tolerance = tol, label = "QDR2018B-Female [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(steroids = T), 1.5, tolerance = tol, label = "QDR2018B-Female [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(cvd = T), 1.3, tolerance = tol, label = "QDR2018B-Female [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(gestdiab = T), 3, tolerance = tol, label = "QDR2018B-Female [gestdiab = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(learndiff = T), 1.3, tolerance = tol, label = "QDR2018B-Female [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(schizobipo = T), 1.2, tolerance = tol, label = "QDR2018B-Female [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(pcos = T), 1.5, tolerance = tol, label = "QDR2018B-Female [pcos = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(statins = T), 1.3, tolerance = tol, label = "QDR2018B-Female [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(hypertension = T), 1.4, tolerance = tol, label = "QDR2018B-Female [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(fh_diab = T), 1.5, tolerance = tol, label = "QDR2018B-Female [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018B)

### Tidy Up ###
rm(gQDR2018B)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018B <- function(...){rQDR2018B(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5), "double")
expect_length(gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018B(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non",
                                                  townsend = -7.028634577))
dat_test[["risk_max"]] <- with(dat_test, QDR2018B(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  ethnicity = "Bangladeshi",
                                                  smoking = "Heavy",
                                                  townsend = 13.3114711,
                                                  antipsy = T,
                                                  steroids = T,
                                                  cvd = T,
                                                  learndiff = T,
                                                  schizobipo = T,
                                                  statins = T,
                                                  hypertension = T,
                                                  fh_diab = T))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018B-Male [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018B-Male [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018B-Male [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018B-Male [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018B(age = 60, fpg = 4.5),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Male [is.null(gender)]")

## Age ##
expect_error(gQDR2018B(fpg = 4.5),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Male [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(tQDR2018B(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018B-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018B(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018B-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018B(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018B-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018B(),
             regexp = "gender, age & fpg must be specified",
             label = "QDR2018B-Male [is.null(fpg)]")
rm(tQDR2018B)

## HbA1c ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDR2018B-Male [!is.null(hba1c)]")
rm(tQDR2018B)

## Gestational Diabetes & PCOS ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(gestdiab = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018B-Male [gestdiab = T]")
expect_error(tQDR2018B(pcos = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018B-Male [pcos = T]")
rm(tQDR2018B)

### Boundaries ###
## Age ##
tQDR2018B <- function(...){gQDR2018B(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDR2018B(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018B-Male [age < 25]")
expect_error(tQDR2018B(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018B-Male [age >= 85]")
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(tQDR2018B(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018B-Male [bmi < 40/2.1^2]")
expect_error(tQDR2018B(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018B-Male [bmi > 180/1.4^2]")
expect_warning(tQDR2018B(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018B-Male [bmi < 20]")
expect_warning(tQDR2018B(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018B-Male [bmi > 40]")
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(...){gQDR2018B(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018B-Male [height < 1.4]")
expect_error(tQDR2018B(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018B-Male [height > 2.1]")
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDR2018B(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018B-Male [weight < 40]")
expect_error(tQDR2018B(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018B-Male [weight > 180]")
rm(tQDR2018B)

## Townsend ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018B-Male [townsend < -7.028634577]")
expect_error(tQDR2018B(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018B-Male [townsend > 13.3114711]")
rm(tQDR2018B)

## Binary Variables ##
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDR2018B(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018B(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018B-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018B)

### Numerical Values ###
## Age ##
tQDR2018B <- function(x){gQDR2018B(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.1, 1.3, 1.5, 1.5, 1.5, 1.3, 1.1)
names(risk_web) <- vec_age

risk_fun <- tQDR2018B(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(x){gQDR2018B(bmi = x, age = 60, fpg = 4.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(5.1, 5.1, 4, 2.6, 1.7, 1.1, 0.8, 0.5)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018B(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(x){gQDR2018B(height = x, age = 60, weight = 90, fpg = 4.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(5.1, 5.1, 4, 2.6, 1.7, 1.1, 0.8, 0.5)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018B(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(x){gQDR2018B(weight = x, age = 60, height = 1.83, fpg = 4.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.5, 0.5, 0.9, 2.3, 4.2, 5.1, 5.1, 5.1)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018B(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(x){gQDR2018B(fpg = x, age = 60, height = 1.83, weight = 90)}
vec_fpg <- 2:6
risk_web <- c(0.4, 0.4, 0.7, 3.1, 13.1)
names(risk_web) <- vec_fpg

risk_fun <- tQDR2018B(vec_fpg)
names(risk_fun) <- vec_fpg

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male Risk (fpg)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

### Categorical Variables ###
## Ethnicity ##
tQDR2018B <- function(x){gQDR2018B(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDR2018B(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018B-Male [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1.5, 4, 5.5, 6.4, 4.1, 2.5, 3.5, 2.8, 2.4)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018B(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

## Smoking ##
tQDR2018B <- function(x){gQDR2018B(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDR2018B(x = "Maybe"),
             regexp = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018B-Male [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1.5, 1.7, 2, 2.1, 2.3)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018B(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018B-Male [range(smoke)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018B)

### Binary Variables ###
tQDR2018B <- function(...){gQDR2018B(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDR2018B(antipsy = T), 1.8, tolerance = tol, label = "QDR2018B-Male [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(steroids = T), 2.1, tolerance = tol, label = "QDR2018B-Male [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(cvd = T), 1.8, tolerance = tol, label = "QDR2018B-Male [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(learndiff = T), 1.9, tolerance = tol, label = "QDR2018B-Male [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(schizobipo = T), 1.8, tolerance = tol, label = "QDR2018B-Male [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(statins = T), 1.8, tolerance = tol, label = "QDR2018B-Male [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(hypertension = T), 1.9, tolerance = tol, label = "QDR2018B-Male [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018B(fh_diab = T), 2.3, tolerance = tol, label = "QDR2018B-Male [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018B)

### Tidy Up ###
rm(gQDR2018B)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDR2018B)
