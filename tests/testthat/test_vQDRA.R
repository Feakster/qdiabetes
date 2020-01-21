#=====================#
#                     #
#### vQDRA() TESTS ####
#                     #
#=====================#

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
rQDRA <- function(...){round(vQDRA(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDRA <- function(...){rQDRA(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDRA(age = 60, height = 1.83, weight = 90), "double")
expect_length(gQDRA(age = 60, height = 1.83, weight = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, vQDRA(gender = "Female",
                                               age = age,
                                               bmi = bmi,
                                               townsend = -7.028634577))

dat_test[["risk_max"]] <- with(dat_test, vQDRA(gender = "Female",
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDRB-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDRB-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDRB-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRA(age = 60),
             regexp = "gender & age must be specified",
             label = "QDRA-Female [is.null(gender)]")

## Age ##
expect_error(gQDRA(),
             regexp = "gender & age must be specified",
             label = "QDRA-Female [is.null(age)]")

## BMI, Height & Weight ##
tQDRA <- function(...){gQDRA(age = 60, ...)}
expect_error(tQDRA(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDRA-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDRA(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDRA-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDRA(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDRA-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRA)

## FPG & HbA1c ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(fpg = 4.5),
             regexp = "unused argument",
             label = "QDRA-Female [!is.null(fpg)]")
expect_error(tQDRA(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDRA-Female [!is.nulll(hba1c)]")
rm(tQDRA)

### Boundaries ###
## Age ##
tQDRA <- function(...){gQDRA(bmi = 30, ...)}
expect_error(tQDRA(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDRA-Female [age < 25]")
expect_error(tQDRA(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDRA-Female [age >= 85]]")
rm(tQDRA)

## BMI ##
tQDRA <- function(...){gQDRA(age = 60, ...)}
expect_error(tQDRA(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDRA-Female [bmi < 40/2.1^2]")
expect_error(tQDRA(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDRA-Female [bmi > 180/1.4^2]")
expect_warning(tQDRA(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDRA-Female [bmi < 20]")
expect_warning(tQDRA(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDRA-Female [bmi > 40]")
rm(tQDRA)

## Height ##
tQDRA <- function(...){gQDRA(age = 60, weight = 90, ...)}
expect_error(tQDRA(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDRA-Female [height < 1.4]")
expect_error(tQDRA(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDRA-Female [height > 2.1]")
rm(tQDRA)

## Weight ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, ...)}
expect_error(tQDRA(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDRA-Female [weight < 40]")
expect_error(tQDRA(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDRA-Female [weight > 180]")
rm(tQDRA)

## Townsend ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDRA-Female [townsend < -7.028634577]")
expect_error(tQDRA(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDRA-Female [townsend > 13.3114711]")
rm(tQDRA)

## Binary Variables ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRA(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRA(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRA(gestdiab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDRA(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRA(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRA(pcos = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDRA(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRA(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRA(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRA)

### Numerical Values ###
## Age ##
tQDRA <- function(x){gQDRA(age = x, height = 1.83, weight = 90)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.2, 0.4, 0.7, 1.1, 1.7, 2.3, 3.0, 3.7, 4.2, 4.4, 4.4, 4.1)
names(risk_web) <- vec_age

risk_fun <- tQDRA(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## BMI ##
tQDRA <- function(x){gQDRA(bmi = x, age = 60)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDRA(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Height ##
tQDRA <- function(x){gQDRA(height = x, age = 60, weight = 90)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDRA(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Weight ##
tQDRA <- function(x){gQDRA(weight = x, age = 60, height = 1.83)}
vec_wt <- seq(40, 180, 10)
risk_web <- c(1, 1, 1, 1.2, 2.2, 3.7, 5.7, 8.2, 11, 13.5, 14.2, 14.2, 14.2, 14.2, 14.2)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDRA(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

### Categorical Variables ###
## Ethnicity ##
tQDRA <- function(x){gQDRA(ethnicity = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDRA(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDRA-Female [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(3.7, 10.3, 13.3, 20.2, 10.9, 5.5, 4.8, 8.6, 5.2)
names(risk_web) <- vec_eth

risk_fun <- tQDRA(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(etnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Smoking ##
tQDRA <- function(x){gQDRA(smoking = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDRA(x = "Maybe"),
             regexpr = "all\\(smoking %in% .+ is not TRUE",
             label = "QDRA-Female [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(3.7, 3.9, 4.8, 5.2, 6.2)
names(risk_web) <- vec_smo

risk_fun <- tQDRA(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

### Binary Variables ###
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_equal(tQDRA(antipsy = T), 5.1, tolerance = tol, label = "QDRA-Female [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(steroids = T), 4.8, tolerance = tol, label = "QDRA-Female [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(cvd = T), 4.4, tolerance = tol, label = "QDRA-Female [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(gestdiab = T), 15.7, tolerance = tol, label = "QDRA-Female [gestdiab = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(learndiff = T), 4, tolerance = tol, label = "QDRA-Female [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(schizobipo = T), 4.7, tolerance = tol, label = "QDRA-Female [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(pcos = T), 5.1, tolerance = tol, label = "QDRA-Female [pcos = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(statins = T), 4.9, tolerance = tol, label = "QDRA-Female [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(hypertension = T), 5.6, tolerance = tol, label = "QDRA-Female [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(fh_diab = T), 5.9, tolerance = tol, label = "QDRA-Female [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDRA)

### Tidy Up ###
rm(gQDRA)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDRA <- function(...){rQDRA(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDRA(age = 60, height = 1.83, weight = 90), "double")
expect_length(gQDRA(age = 60, height = 1.83, weight = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, vQDRA(gender = "Male",
                                               age = age,
                                               bmi = bmi,
                                               ethnicity = "WhiteNA",
                                               smoking = "Non",
                                               townsend = -7.028634577))
dat_test[["risk_max"]] <- with(dat_test, vQDRA(gender = "Male",
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

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDRB-Male [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDRB-Male [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB-Male [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDRB-Male [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRA(age = 60),
             regexp = "gender & age must be specified",
             label = "QDRA-Male [is.null(gender)]")

## Age ##
expect_error(gQDRA(),
             regexp = "gender & age must be specified",
             label = "QDRA-Male [is.null(age)]")

## BMI, Height & Weight ##
tQDRA <- function(...){gQDRA(age = 60, ...)}
expect_error(tQDRA(height = 1.83),
             regexp = "either bmi or height & weight must be specified",
             label = "QDRA-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDRA(weight = 90),
             regexp = "either bmi or height & weight must be specified",
             label = "QDRA-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDRA(bmi = 30, height = 1.83, weight = 90),
               regexp = "bmi, height & weight all specified, height & weight ignored",
               label = "QDRA-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRA)

## FPG & HbA1c ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(fpg = 4.5),
             regexp = "unused argument",
             label = "QDRA-Male [!is.null(fpg)]")
expect_error(tQDRA(hba1c = 31.5),
             regexp = "unused argument",
             label = "QDRA-Male [!is.null(hba1c)]")
rm(tQDRA)

## Gestational Diabetes & PCOS ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(gestdiab = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDRA-Male [gestdiab = T]")
expect_error(tQDRA(pcos = T),
             regexp = "'pcos' and 'gestdiab' must be set to FALSE for male 'gender'",
             label = "QDRA-Male [pcos = T]")
rm(tQDRA)

### Boundaries ###
## Age ##
tQDRA <- function(...){gQDRA(bmi = 30, ...)}
expect_error(tQDRA(age = 24),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDRA-Male [age < 25]")
expect_error(tQDRA(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDRA-Male [age >= 85]")
rm(tQDRA)

## BMI ##
tQDRA <- function(...){gQDRA(age = 60, ...)}
expect_error(tQDRA(bmi = (40/2.1^2) - 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDRA-Male [bmi < 40/2.1^2]")
expect_error(tQDRA(bmi = (180/1.4^2) + 1),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDRA-Male [bmi > 180/1.4^2]")
expect_warning(tQDRA(bmi = 19),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDRA-Male [bmi < 20]")
expect_warning(tQDRA(bmi = 41),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDRA-Male [bmi > 40]")
rm(tQDRA)

## Height ##
tQDRA <- function(...){gQDRA(age = 60, weight = 90, ...)}
expect_error(tQDRA(height = 1.3),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDRA-Male [height < 1.4]")
expect_error(tQDRA(height = 2.2),
             regexp = "all\\(height >= 1\\.4 & height <= 2\\.1\\) is not TRUE",
             label = "QDRA-Male [height > 2.1]")
rm(tQDRA)

## Weight ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, ...)}
expect_error(tQDRA(weight = 39),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDRA-Male [weight < 40]")
expect_error(tQDRA(weight = 181),
             regexp = "all\\(weight >= 40 & weight <= 180\\) is not TRUE",
             label = "QDRA-Male [weight > 180]")
rm(tQDRA)

## Townsend ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(townsend = -7.028634578),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDRA-Male [townsend < -7.028634577]")
expect_error(tQDRA(townsend = 13.3114712),
             regexp = "all\\(townsend >= -7\\.\\d+ & townsend <= 13\\.\\d+\\) is not TRUE",
             label = "QDRA-Male [townsend > 13.3114711]")
rm(tQDRA)

## Binary Variables ##
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRA(antipsy = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRA(steroids = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRA(cvd = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRA(learndiff = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRA(schizobipo = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRA(statins = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRA(hypertension = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRA(fh_diab = -1),
             regexp = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
             label = "QDRA-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRA)

### Numerical Values ###
## Age ##
tQDRA <- function(x){gQDRA(age = x, height = 1.83, weight = 90)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.3, 0.5, 1, 1.7, 2.5, 3.5, 4.4, 5.3, 5.9, 6.1, 6, 5.5)
names(risk_web) <- vec_age

risk_fun <- tQDRA(vec_age)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## BMI ##
tQDRA <- function(x){gQDRA(bmi = x, age = 60)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(23.6, 23.6, 17.1, 10.5, 6.2, 3.7, 2.3, 1.6)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- tQDRA(vec_bmi)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Height ##
tQDRA <- function(x){gQDRA(height = x, age = 60, weight = 90)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(23.6, 23.6, 17.1, 10.5, 6.2, 3.7, 2.3, 1.6)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- tQDRA(vec_ht)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Weight ##
tQDRA <- function(x){gQDRA(weight = x, age = 60, height = 1.83)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(1.4, 1.4, 3.1, 8.7, 18.3, 23.6, 23.6, 23.6)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- tQDRA(vec_wt)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

### Categorical Variables ###
## Ethnicity ##
tQDRA <- function(x){gQDRA(ethnicity = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDRA(x = "Blue"),
             regexp = "all\\(ethnicity %in% .+ is not TRUE",
             label = "QDRB-Male [ethnicity == 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(5.3, 15.1, 17.9, 25.2, 15.6, 8.3, 10.4, 10.3, 7.9)
names(risk_web) <- vec_eth

risk_fun <- tQDRA(vec_eth)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

## Smoking ##
tQDRA <- function(x){gQDRA(smoking = x, age = 60, height = 1.83, weight = 90)}
expect_error(tQDRA(x = "Maybe"),
             regexp = "all\\(smoking %in% .+ is not TRUE",
             label = "QDRB-Male [smoking == 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(5.3, 6.2, 7.2, 7.2, 8.2)
names(risk_web) <- vec_smo

risk_fun <- tQDRA(vec_smo)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRA-Male [range(smoke)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRA)

### Binary Variables ###
tQDRA <- function(...){gQDRA(age = 60, height = 1.83, weight = 90, ...)}
expect_equal(tQDRA(antipsy = T), 6, tolerance = tol, label = "QDRA-Male [antipsy = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(steroids = T), 6.6, tolerance = tol, label = "QDRA-Male [steroids = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(cvd = T), 6.4, tolerance = tol, label = "QDRA-Male [cvd = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(learndiff = T), 5.5, tolerance = tol, label = "QDRA-Male [learndiff = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(schizobipo = T), 6.6, tolerance = tol, label = "QDRA-Male [schizobipo = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(statins = T), 6.7, tolerance = tol, label = "QDRA-Male [statins = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(hypertension = T), 7.3, tolerance = tol, label = "QDRA-Male [hypertension = T]", expected.label = "ClinRisk")
expect_equal(tQDRA(fh_diab = T), 8.8, tolerance = tol, label = "QDRA-Male [fh_diab = T]", expected.label = "ClinRisk")
rm(tQDRA)

### Tidy Up ###
rm(gQDRA)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDRA)
