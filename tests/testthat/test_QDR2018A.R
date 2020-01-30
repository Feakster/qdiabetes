#========================#
#                        #
#### QDR2018A() TESTS ####
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
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2018A <- function(...){round(QDR2018A(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018A <- function(...){rQDR2018A(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDR2018A(age = 60, height = 1.83, weight = 90), "double")
expect_length(gQDR2018A(age = 60, height = 1.83, weight = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018A(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  townsend = -7.028634577))

dat_test[["risk_max"]] <- with(dat_test, QDR2018A(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018A-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018A-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018A-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018A-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018A(age = 60),
             regexp = "gender & age must be specified",
             label = "QDR2018A-Female [is.null(gender)]")

## Age ##
expect_error(gQDR2018A(),
             regexp = "gender & age must be specified",
             label = "QDR2018A-Female [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(tQDR2018A(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018A-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018A(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018A-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018A(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018A-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018A)

## FPG & HbA1c ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(fpg = 4.5),
             regexp = "unused argument",
             label = "QDR2018A-Female [!is.null(fpg)]")
expect_error(tQDR2018A(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDR2018A-Female [!is.nulll(hba1c)]")
rm(tQDR2018A)

### Boundaries ###
## Age ##
tQDR2018A <- function(...){gQDR2018A(bmi = 30, ...)}
expect_error(tQDR2018A(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018A-Female [age < 25]")
expect_error(tQDR2018A(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018A-Female [age >= 85]]")
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(tQDR2018A(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018A-Female [bmi < 40/2.1^2]")
expect_error(tQDR2018A(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018A-Female [bmi > 180/1.4^2]")
expect_warning(tQDR2018A(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018A-Female [bmi < 20]")
expect_warning(tQDR2018A(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018A-Female [bmi > 40]")
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(...){gQDR2018A(age = 60, weight = 90, ...)}
expect_error(tQDR2018A(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018A-Female [height < 1.4]")
expect_error(tQDR2018A(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018A-Female [height > 2.1]")
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, ...)}
expect_error(tQDR2018A(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018A-Female [weight < 40]")
expect_error(tQDR2018A(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018A-Female [weight > 180]")
rm(tQDR2018A)

## Townsend ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018A-Female [townsend < -7.028634577]")
expect_error(tQDR2018A(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018A-Female [townsend > 13.3114711]")
rm(tQDR2018A)

## Binary Variables ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(gestdiab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(pcos = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018A)

### Numerical Values ###
## Age ##
tQDR2018A <- function(x){gQDR2018A(age = x, height = 1.83, weight = 90)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.2, 0.4, 0.7, 1.1, 1.7, 2.3, 3.0, 3.7, 4.2, 4.4, 4.4, 4.1)
names(risk_web) <- vec_age

risk_fun <- tQDR2018A(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(x){gQDR2018A(bmi = x, age = 60)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018A(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(x){gQDR2018A(height = x, age = 60, weight = 90)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018A(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(x){gQDR2018A(weight = x, age = 60, height = 1.83)}
vec_wt <- seq(40, 180, 10)
risk_web <- c(1, 1, 1, 1.2, 2.2, 3.7, 5.7, 8.2, 11, 13.5, 14.2, 14.2, 14.2, 14.2, 14.2)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018A(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

### Categorical Variables ###
## Ethnicity ##
tQDR2018A <- function(x){gQDR2018A(ethnicity = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDR2018A(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018A-Female [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(3.7, 10.3, 13.3, 20.2, 10.9, 5.5, 4.8, 8.6, 5.2)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018A(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(etnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Smoking ##
tQDR2018A <- function(x){gQDR2018A(smoking = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDR2018A(x = "Maybe"),
             regexpr = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018A-Female [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(3.7, 3.9, 4.8, 5.2, 6.2)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018A(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

### Binary Variables ###
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_equal(tQDR2018A(antipsy = T), 5.1, tolerance = tol, label = "QDR2018A-Female [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(steroids = T), 4.8, tolerance = tol, label = "QDR2018A-Female [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(cvd = T), 4.4, tolerance = tol, label = "QDR2018A-Female [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(gestdiab = T), 15.7, tolerance = tol, label = "QDR2018A-Female [gestdiab = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(learndiff = T), 4, tolerance = tol, label = "QDR2018A-Female [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(schizobipo = T), 4.7, tolerance = tol, label = "QDR2018A-Female [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(pcos = T), 5.1, tolerance = tol, label = "QDR2018A-Female [pcos = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(statins = T), 4.9, tolerance = tol, label = "QDR2018A-Female [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(hypertension = T), 5.6, tolerance = tol, label = "QDR2018A-Female [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(fh_diab = T), 5.9, tolerance = tol, label = "QDR2018A-Female [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018A)

### Tidy Up ###
rm(gQDR2018A)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018A <- function(...){rQDR2018A(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDR2018A(age = 60, height = 1.83, weight = 90), "double")
expect_length(gQDR2018A(age = 60, height = 1.83, weight = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018A(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non",
                                                  townsend = -7.028634577))
dat_test[["risk_max"]] <- with(dat_test, QDR2018A(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018A-Male [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018A-Male [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018A-Male [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018A-Male [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018A(age = 60),
             regexp = "gender & age must be specified",
             label = "QDR2018A-Male [is.null(gender)]")

## Age ##
expect_error(gQDR2018A(),
             regexp = "gender & age must be specified",
             label = "QDR2018A-Male [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(tQDR2018A(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018A-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018A(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018A-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018A(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018A-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018A)

## FPG & HbA1c ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(fpg = 4.5),
             regexp = "unused argument",
             label = "QDR2018A-Male [!is.null(fpg)]")
expect_error(tQDR2018A(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDR2018A-Male [!is.null(hba1c)]")
rm(tQDR2018A)

## Gestational Diabetes & PCOS ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(gestdiab = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018A-Male [gestdiab = T]")
expect_error(tQDR2018A(pcos = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018A-Male [pcos = T]")
rm(tQDR2018A)

### Boundaries ###
## Age ##
tQDR2018A <- function(...){gQDR2018A(bmi = 30, ...)}
expect_error(tQDR2018A(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018A-Male [age < 25]")
expect_error(tQDR2018A(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018A-Male [age >= 85]")
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(tQDR2018A(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018A-Male [bmi < 40/2.1^2]")
expect_error(tQDR2018A(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018A-Male [bmi > 180/1.4^2]")
expect_warning(tQDR2018A(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018A-Male [bmi < 20]")
expect_warning(tQDR2018A(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018A-Male [bmi > 40]")
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(...){gQDR2018A(age = 60, weight = 90, ...)}
expect_error(tQDR2018A(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018A-Male [height < 1.4]")
expect_error(tQDR2018A(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018A-Male [height > 2.1]")
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, ...)}
expect_error(tQDR2018A(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018A-Male [weight < 40]")
expect_error(tQDR2018A(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018A-Male [weight > 180]")
rm(tQDR2018A)

## Townsend ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018A-Male [townsend < -7.028634577]")
expect_error(tQDR2018A(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018A-Male [townsend > 13.3114711]")
rm(tQDR2018A)

## Binary Variables ##
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018A(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018A(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018A-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018A)

### Numerical Values ###
## Age ##
tQDR2018A <- function(x){gQDR2018A(age = x, height = 1.83, weight = 90)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.3, 0.5, 1, 1.7, 2.5, 3.5, 4.4, 5.3, 5.9, 6.1, 6, 5.5)
names(risk_web) <- vec_age

risk_fun <- tQDR2018A(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(x){gQDR2018A(bmi = x, age = 60)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(23.6, 23.6, 17.1, 10.5, 6.2, 3.7, 2.3, 1.6)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018A(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(x){gQDR2018A(height = x, age = 60, weight = 90)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(23.6, 23.6, 17.1, 10.5, 6.2, 3.7, 2.3, 1.6)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018A(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(x){gQDR2018A(weight = x, age = 60, height = 1.83)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(1.4, 1.4, 3.1, 8.7, 18.3, 23.6, 23.6, 23.6)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018A(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

### Categorical Variables ###
## Ethnicity ##
tQDR2018A <- function(x){gQDR2018A(ethnicity = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDR2018A(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018A-Male [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(5.3, 15.1, 17.9, 25.2, 15.6, 8.3, 10.4, 10.3, 7.9)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018A(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

## Smoking ##
tQDR2018A <- function(x){gQDR2018A(smoking = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDR2018A(x = "Maybe"),
             regexp = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018A-Male [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(5.3, 6.2, 7.2, 7.2, 8.2)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018A(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018A-Male [range(smoke)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018A)

### Binary Variables ###
tQDR2018A <- function(...){gQDR2018A(age = 60, height = 1.83, weight = 90, ...)}
expect_equal(tQDR2018A(antipsy = T), 6, tolerance = tol, label = "QDR2018A-Male [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(steroids = T), 6.6, tolerance = tol, label = "QDR2018A-Male [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(cvd = T), 6.4, tolerance = tol, label = "QDR2018A-Male [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(learndiff = T), 5.5, tolerance = tol, label = "QDR2018A-Male [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(schizobipo = T), 6.6, tolerance = tol, label = "QDR2018A-Male [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(statins = T), 6.7, tolerance = tol, label = "QDR2018A-Male [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(hypertension = T), 7.3, tolerance = tol, label = "QDR2018A-Male [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018A(fh_diab = T), 8.8, tolerance = tol, label = "QDR2018A-Male [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018A)

### Tidy Up ###
rm(gQDR2018A)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDR2018A)
