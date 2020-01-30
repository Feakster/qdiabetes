#========================#
#                        #
#### QDR2018C() TESTS ####
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
  hba1c = seq(15, 48 - tiny, length.out = 50),
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2018C <- function(...){round(QDR2018C(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018C <- function(...){rQDR2018C(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5), "double")
expect_length(gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018C(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
                                                  townsend = -7.028634577))

dat_test[["risk_max"]] <- with(dat_test, QDR2018C(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018C-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018C-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018C-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018C-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018C(age = 60, hba1c = 31.5),
             regexp = "gender, age & hba1c must be specified",
             label = "QDR2018C-Female [is.null(gender)]")

## Age ##
expect_error(gQDR2018C(hba1c = 31.5),
             regexp = "gender, age & hba1c must be specified",
             label = "QDR2018C-Female [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(tQDR2018C(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018C-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018C(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018C-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018C(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018C-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018C)

## FPG ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(fpg = 4.5),
             regexp = "unused argument",
             label = "QDR2018C-Female [!is.null(fpg)]")
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018C(),
             regexp = "gender, age & hba1c must be specified",
             label = "QDRA-Female [is.null(hba1c)]")
rm(tQDR2018C)

### Boundaries ###
## Age ##
tQDR2018C <- function(...){gQDR2018C(bmi = 30, hba1c = 31.5, ...)}
expect_error(tQDR2018C(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018C-Female [age < 25]")
expect_error(tQDR2018C(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018C-Female [age >= 85]]")
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(tQDR2018C(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018C-Female [bmi < 40/2.1^2]")
expect_error(tQDR2018C(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018C-Female [bmi > 180/1.4^2]")
expect_warning(tQDR2018C(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018C-Female [bmi < 20]")
expect_warning(tQDR2018C(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018C-Female [bmi > 40]")
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(...){gQDR2018C(age = 60, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018C-Female [height < 1.4]")
expect_error(tQDR2018C(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018C-Female [height > 2.1]")
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, hba1c = 31.5, ...)}
expect_error(tQDR2018C(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018C-Female [weight < 40]")
expect_error(tQDR2018C(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018C-Female [weight > 180]")
rm(tQDR2018C)

## Townsend ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018C-Female [townsend < -7.028634577]")
expect_error(tQDR2018C(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018C-Female [townsend > 13.3114711]")
rm(tQDR2018C)

## Binary Variables ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(gestdiab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(pcos = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018C)

### Numerical Values ###
## Age ##
tQDR2018C <- function(x){gQDR2018C(age = x, height = 1.83, weight = 90, hba1c = 31.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.5)
names(risk_web) <- vec_age

risk_fun <- tQDR2018C(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(x){gQDR2018C(bmi = x, age = 60, hba1c = 31.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(2.3, 2.3, 1.8, 1.3, 0.9, 0.6, 0.4, 0.3)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018C(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(x){gQDR2018C(height = x, age = 60, weight = 90, hba1c = 31.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(2.3, 2.3, 1.8, 1.3, 0.9, 0.6, 0.4, 0.3)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018C(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(x){gQDR2018C(weight = x, age = 60, height = 1.83, hba1c = 31.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.3, 0.3, 0.5, 1.1, 1.9, 2.3, 2.3, 2.3)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018C(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(x){gQDR2018C(hba1c = x, age = 60, height = 1.83, weight = 90)}
vec_hba1c <- seq(15, 45, 5)
risk_web <- c(0, 0, 0.1, 0.5, 1.8, 5.2, 13.6)
names(risk_web) <- vec_hba1c

risk_fun <- tQDR2018C(vec_hba1c)
names(risk_fun) <- vec_hba1c

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(hba1c)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

### Categorical Variables ###
## Ethnicity ##
tQDR2018C <- function(x){gQDR2018C(ethnicity = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDR2018C(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018C-Female [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(0.8, 1.4, 1.7, 2.6, 1.6, 0.9, 0.8, 1.4, 0.9)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018C(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(etnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Smoking ##
tQDR2018C <- function(x){gQDR2018C(smoking = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDR2018C(x = "Maybe"),
             regexpr = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018C-Female [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(0.8, 0.8, 0.9, 0.9, 1.1)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018C(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

### Binary Variables ###
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_equal(tQDR2018C(antipsy = T), 1.1, tolerance = tol, label = "QDR2018C-Female [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(steroids = T), 0.9, tolerance = tol, label = "QDR2018C-Female [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(cvd = T), 0.9, tolerance = tol, label = "QDR2018C-Female [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(gestdiab = T), 2.4, tolerance = tol, label = "QDR2018C-Female [gestdiab = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(learndiff = T), 0.9, tolerance = tol, label = "QDR2018C-Female [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(schizobipo = T), 1.1, tolerance = tol, label = "QDR2018C-Female [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(pcos = T), 1.1, tolerance = tol, label = "QDR2018C-Female [pcos = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(statins = T), 0.9, tolerance = tol, label = "QDR2018C-Female [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(hypertension = T), 1.2, tolerance = tol, label = "QDR2018C-Female [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(fh_diab = T), 1.2, tolerance = tol, label = "QDR2018C-Female [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018C)

### Tidy Up ###
rm(gQDR2018C)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018C <- function(...){rQDR2018C(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5), "double")
expect_length(gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018C(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non",
                                                  townsend = -7.028634577))
dat_test[["risk_max"]] <- with(dat_test, QDR2018C(gender = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2018C-Male [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDR2018C-Male [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDR2018C-Male [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDR2018C-Male [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2018C(age = 60, hba1c = 31.5),
             regexp = "gender, age & hba1c must be specified",
             label = "QDR2018C-Male [is.null(gender)]")

## Age ##
expect_error(gQDR2018C(hba1c = 31.5),
             regexp = "gender, age & hba1c must be specified",
             label = "QDR2018C-Male [is.null(age)]")

## BMI, Height & Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(tQDR2018C(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018C-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDR2018C(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDR2018C-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDR2018C(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDR2018C-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDR2018C)

## FPG ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(fpg = 4.5),
             regexp = "unused argument",
             label = "QDR2018C-Male [!is.null(fpg)]")
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDR2018C(),
             regexp = "gender, age & hba1c must be specified",
             label = "QDR2018C-Male [is.null(hba1c)]")
rm(tQDR2018C)

## Gestational Diabetes & PCOS ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(gestdiab = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018C-Male [gestdiab = T]")
expect_error(tQDR2018C(pcos = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDR2018C-Male [pcos = T]")
rm(tQDR2018C)

### Boundaries ###
## Age ##
tQDR2018C <- function(...){gQDR2018C(bmi = 30, hba1c = 31.5, ...)}
expect_error(tQDR2018C(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018C-Male [age < 25]")
expect_error(tQDR2018C(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2018C-Male [age >= 85]")
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(tQDR2018C(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018C-Male [bmi < 40/2.1^2]")
expect_error(tQDR2018C(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2018C-Male [bmi > 180/1.4^2]")
expect_warning(tQDR2018C(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2018C-Male [bmi < 20]")
expect_warning(tQDR2018C(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2018C-Male [bmi > 40]")
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(...){gQDR2018C(age = 60, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018C-Male [height < 1.4]")
expect_error(tQDR2018C(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDR2018C-Male [height > 2.1]")
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, hba1c = 31.5, ...)}
expect_error(tQDR2018C(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018C-Male [weight < 40]")
expect_error(tQDR2018C(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDR2018C-Male [weight > 180]")
rm(tQDR2018C)

## Townsend ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018C-Male [townsend < -7.028634577]")
expect_error(tQDR2018C(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDR2018C-Male [townsend > 13.3114711]")
rm(tQDR2018C)

## Binary Variables ##
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDR2018C(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDR2018C(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDR2018C-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDR2018C)

### Numerical Values ###
## Age ##
tQDR2018C <- function(x){gQDR2018C(age = x, height = 1.83, weight = 90, hba1c = 31.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.1, 1.3, 1.5, 1.5, 1.5, 1.3, 1.1)
names(risk_web) <- vec_age

risk_fun <- tQDR2018C(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(x){gQDR2018C(bmi = x, age = 60, hba1c = 31.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(5, 5, 3.9, 2.6, 1.7, 1.1, 0.7, 0.5)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDR2018C(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(x){gQDR2018C(height = x, age = 60, weight = 90, hba1c = 31.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(5, 5, 3.9, 2.6, 1.7, 1.1, 0.7, 0.5)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDR2018C(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(x){gQDR2018C(weight = x, age = 60, height = 1.83, hba1c = 31.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.5, 0.5, 0.9, 2.2, 4.1, 5, 5, 5)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDR2018C(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(x){gQDR2018C(hba1c = x, age = 60, height = 1.83, weight = 90)}
vec_hba1c <- seq(15, 45, 5)
risk_web <- c(0, 0.1, 0.3, 1.1, 3, 7.5, 17)
names(risk_web) <- vec_hba1c

risk_fun <- tQDR2018C(vec_hba1c)
names(risk_fun) <- vec_hba1c

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male Risk (hba1c)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

### Categorical Variables ###
## Ethnicity ##
tQDR2018C <- function(x){gQDR2018C(ethnicity = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDR2018C(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDR2018C-Male [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1.5, 2.9, 3.3, 4.3, 3.1, 1.8, 2.1, 2.1, 1.8)
names(risk_web) <- vec_eth

risk_fun <- tQDR2018C(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

## Smoking ##
tQDR2018C <- function(x){gQDR2018C(smoking = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDR2018C(x = "Maybe"),
             regexp = "all\\(smoking %in% .+ is not TRUE",
             label = "QDR2018C-Male [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1.5, 1.6, 1.7, 1.6, 1.8)
names(risk_web) <- vec_smo

risk_fun <- tQDR2018C(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDR2018C-Male [range(smoke)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDR2018C)

### Binary Variables ###
tQDR2018C <- function(...){gQDR2018C(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_equal(tQDR2018C(antipsy = T), 1.8, tolerance = tol, label = "QDR2018C-Male [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(steroids = T), 1.7, tolerance = tol, label = "QDR2018C-Male [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(cvd = T), 1.7, tolerance = tol, label = "QDR2018C-Male [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(learndiff = T), 1.6, tolerance = tol, label = "QDR2018C-Male [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(schizobipo = T), 1.9, tolerance = tol, label = "QDR2018C-Male [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(statins = T), 1.6, tolerance = tol, label = "QDR2018C-Male [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(hypertension = T), 2, tolerance = tol, label = "QDR2018C-Male [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDR2018C(fh_diab = T), 2.3, tolerance = tol, label = "QDR2018C-Male [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDR2018C)

### Tidy Up ###
rm(gQDR2018C)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDR2018C)
