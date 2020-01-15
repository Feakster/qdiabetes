#====================#
#                    #
#### QDRB() TESTS ####
#                    #
#====================#

### Notes ###
# - 0.1 tolerance allowed in risk outputs to account for different rounding standards.
# - Townsend score currently not easily testable.

### Parameters ###
tol <- 0.1 # Tolerance
tiny <- 1e-8 # Small number

### Test Data ###
dat_test <- expand.grid(
  age = seq(25, 85 - tiny, length.out = 10),
  height = seq(1.4, 2.1, length.out = 10),
  weight = seq(40, 180, length.out = 10),
  fpg = seq(2, 7 - tiny, length.out = 10),
  townsend = seq(-7.028634577 + tiny, 13.3114711 - tiny, length.out = 10),
  KEEP.OUT.ATTRS = F
)

dat_test[["bmi"]] <- with(dat_test, weight/height^2)
dat_test <- dat_test[dat_test$bmi >= 20 & dat_test$bmi <= 40, ]

### Redefine Function (Rounding) ###
rQDRB <- function(...){round(QDRB(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDRB <- function(...){rQDRB(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5), "double")
expect_length(gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, mapply(QDRB,
                                                age = age,
                                                height = height,
                                                weight = weight,
                                                fpg = fpg,
                                                townsend = townsend,
                                                MoreArgs = list(
                                                  gender = "Female",
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non")))
dat_test[["risk_max"]] <- with(dat_test, mapply(QDRB,
                                                age = age,
                                                height = height, 
                                                weight = weight,
                                                fpg = fpg,
                                                townsend = townsend,
                                                MoreArgs = list(
                                                  gender = "Female",
                                                  ethnicity = "Bangladeshi",
                                                  smoking = "Heavy",
                                                  steroids = T,
                                                  cvd = T,
                                                  gestdiab = T,
                                                  learndiff = T,
                                                  schizobipo = T,
                                                  pcos = T,
                                                  statins = T,
                                                  hypertension = T,
                                                  fh_diab = T)))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDRB-Female [min(risk) >= 0]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB-Female [min(risk) <= 100]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRB(age = 60, fpg = 4.5), label = "QDRB-Female [is.null(gender)]")
## Age ##
expect_error(gQDRB(fpg = 4.5), label = "QDRB-Female [is.null(age)]")
## BMI, Height & Weight ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.83), label = "QDRB-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDRB(weight = 90), label = "QDRB-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDRB(bmi = 30, height = 1.83, weight = 90), label = "QDRB-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRB)
## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(), label = "QDRB-Female [is.null(fpg)]")
rm(tQDRB)
## HbA1c ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(hba1c = 31.5), label = "QDRA-Female [!is.null(hba1c)]")
rm(tQDRB)

### Boundaries ###
## Age ##
tQDRB <- function(...){gQDRB(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDRB(age = 24), label = "QDRB-Female [age < 25]")
expect_error(tQDRB(age = 85), label = "QDRB-Female [age >= 85]")
rm(tQDRB)

## BMI ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(bmi = (40/2.1^2) - 1), label = "QDRB-Female [bmi < 40/2.1^2]")
expect_error(tQDRB(bmi = (180/1.4^2) + 1), label = "QDRB-Female [bmi > 180/1.4^2]")
expect_warning(tQDRB(bmi = 19), label = "QDRB-Female [bmi < 20]")
expect_warning(tQDRB(bmi = 41), label = "QDRB-Female [bmi > 41]")
rm(tQDRB)

## Height ##
tQDRB <- function(...){gQDRB(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.3), label = "QDRB-Female [height < 1.4]")
expect_error(tQDRB(height = 2.2), label = "QDRB-Female [height > 2.1]")
rm(tQDRB)

## Weight ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDRB(weight = 39), label = "QDRB-Female [weight < 40]")
expect_error(tQDRB(weight = 181), label = "QDRB-Female [weight > 180]")
rm(tQDRB)

## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(fpg = 1.9), label = "QDRB-Female [fpg < 2]")
expect_error(tQDRB(fpg = 7), label = "QDRB-Female [fpg >= 7]")
rm(tQDRB)

## Townsend ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(townsend = -7.028634578), label = "QDRB-Female [townsend < -7.028634577]")
expect_error(tQDRB(townsend = 13.3114712), label = "QDRB-Female [townsend > 13.3114711]")
rm(tQDRB)

## Binary Variables ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(antipsy = -1), label = "QDRB-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRB(steroids = -1), label = "QDRB-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRB(cvd = -1), label = "QDRB-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRB(gestdiab = -1), label = "QDRB-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDRB(learndiff = -1), label = "QDRB-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRB(schizobipo = -1), label = "QDRB-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRB(pcos = -1), label = "QDRB-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDRB(statins = -1), label = "QDRB-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRB(hypertension = -1), label = "QDRB-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRB(fh_diab = -1), label = "QDRB-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRB)

### Numerical Values ###
## Age ##
tQDRB <- function(x){gQDRB(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.8, 0.9, 1, 1.1, 1, 0.9, 0.7)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRB)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## BMI ##
tQDRB <- function(x){gQDRB(bmi = x, age = 60, fpg = 4.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(2.8, 2.8, 2.3, 1.6, 1.1, 0.8, 0.6, 0.4)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- sapply(vec_bmi, tQDRB)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Height ##
tQDRB <- function(x){gQDRB(height = x, age = 60, weight = 90, fpg = 4.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(2.8, 2.8, 2.3, 1.6, 1.1, 0.8, 0.6, 0.4)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- sapply(vec_ht, tQDRB)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Weight ##
tQDRB <- function(x){gQDRB(weight = x, age = 60, height = 1.83, fpg = 4.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.4, 0.4, 0.7, 1.4, 2.4, 2.8, 2.8, 2.8)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, tQDRB)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## FPG ##
tQDRB <- function(x){gQDRB(fpg = x, age = 60, height = 1.83, weight = 90)}
vec_fpg <- 2:6
risk_web <- c(0.5, 0.1, 0.4, 2.5, 13)
names(risk_web) <- vec_fpg

risk_fun <- sapply(vec_fpg, tQDRB)
names(risk_fun) <- vec_fpg

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(fpg)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Categorical Variables ###
## Ethnicity ##
tQDRB <- function(x){gQDRB(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Blue"), label = "QDRB-Female [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1, 2.7, 3.5, 4.5, 2.7, 1.7, 1.7, 2.2, 1.5)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRB)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Smoking ##
tQDRB <- function(x){gQDRB(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Maybe"), label = "QDRB-Female [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1, 1.1, 1.3, 1.4, 1.6)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRB)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Binary Variables ###
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDRB(antipsy = T), 1.4, tolerance = tol, label = "QDRB-Female [antipsy = T]")
expect_equal(tQDRB(steroids = T), 1.5, tolerance = tol, label = "QDRB-Female [steroids = T]")
expect_equal(tQDRB(cvd = T), 1.3, tolerance = tol, label = "QDRB-Female [cvd = T]")
expect_equal(tQDRB(gestdiab = T), 3, tolerance = tol, label = "QDRB-Female [gestdiab = T]")
expect_equal(tQDRB(learndiff = T), 1.3, tolerance = tol, label = "QDRB-Female [learndiff = T]")
expect_equal(tQDRB(schizobipo = T), 1.2, tolerance = tol, label = "QDRB-Female [schizobipo = T]")
expect_equal(tQDRB(pcos = T), 1.5, tolerance = tol, label = "QDRB-Female [pcos = T]")
expect_equal(tQDRB(statins = T), 1.3, tolerance = tol, label = "QDRB-Female [statins = T]")
expect_equal(tQDRB(hypertension = T), 1.4, tolerance = tol, label = "QDRB-Female [hypertension = T]")
expect_equal(tQDRB(fh_diab = T), 1.5, tolerance = tol, label = "QDRB-Female [fh_diab = T]")
rm(tQDRB)

### Tidy Up ###
rm(gQDRB)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDRB <- function(...){rQDRB(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5), "double")
expect_length(gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, mapply(QDRB,
                                                age = age,
                                                height = height,
                                                weight = weight,
                                                fpg = fpg,
                                                townsend = townsend,
                                                MoreArgs = list(
                                                  gender = "Male",
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non")))
dat_test[["risk_max"]] <- with(dat_test, mapply(QDRB,
                                                age = age,
                                                height = height, 
                                                weight = weight,
                                                fpg = fpg,
                                                townsend = townsend,
                                                MoreArgs = list(
                                                  gender = "Male",
                                                  ethnicity = "Bangladeshi",
                                                  smoking = "Heavy",
                                                  steroids = T,
                                                  cvd = T,
                                                  learndiff = T,
                                                  schizobipo = T,
                                                  statins = T,
                                                  hypertension = T,
                                                  fh_diab = T)))

expect_gte(min(dat_test[["risk_min"]]), 0)
expect_lte(max(dat_test[["risk_max"]]), 100)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRB(age = 60, fpg = 4.5), label = "QDRB-Male [is.null(gender)]")
## Age ##
expect_error(gQDRB(fpg = 4.5), label = "QDRB-Male [is.null(age)]")
## BMI, Height & Weight ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.83), label = "QDRB-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDRB(weight = 90), label = "QDRB-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDRB(bmi = 30, height = 1.83, weight = 90), label = "QDRB-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRB)
## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(), label = "QDRB-Male [is.null(fpg)]")
rm(tQDRB)
## HbA1c ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(hba1c = 31.5), label = "QDRB-Male [!is.null(hba1c)]")
rm(tQDRB)

### Boundaries ###
## Age ##
tQDRB <- function(...){gQDRB(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDRB(age = 24), label = "QDRB-Male [age < 25]")
expect_error(tQDRB(age = 85), label = "QDRB-Male [age >= 85]")
rm(tQDRB)

## BMI ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(bmi = (40/2.1^2) - 1), label = "QDRB-Male [bmi < 40/2.1^2]")
expect_error(tQDRB(bmi = (180/1.4^2) + 1), label = "QDRB-Male [bmi > 40/2.1^2]")
expect_warning(tQDRB(bmi = 19), label = "QDRB-Male [bmi < 20]")
expect_warning(tQDRB(bmi = 41), label = "QDRB-Male [bm > 40]")
rm(tQDRB)

## Height ##
tQDRB <- function(...){gQDRB(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.3), label = "QDRB-Male [height < 1.4]")
expect_error(tQDRB(height = 2.2), label = "QDRB-Male [height > 2.1]")
rm(tQDRB)

## Weight ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDRB(weight = 39), label = "QDRB-Male [weight < 40]")
expect_error(tQDRB(weight = 181), label = "QDRB-Male [weight > 180]")
rm(tQDRB)

## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(fpg = 1.9), label = "QDRB-Male [fpg < 2.0]")
expect_error(tQDRB(fpg = 7), label = "QDRB-Male [fpg >= 7]")
rm(tQDRB)

## Townsend ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(townsend = -7.028634578), label = "QDRB-Male [townsend < -7.028634577]")
expect_error(tQDRB(townsend = 13.3114712), label = "QDRB-Male [townsend > 13.3114711]")
rm(tQDRB)

## Binary Variables ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(antipsy = -1), label = "QDRB-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRB(steroids = -1), label = "QDRB-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRB(cvd = -1), label = "QDRB-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRB(learndiff = -1), label = "QDRB-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRB(schizobipo = -1), label = "QDRB-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRB(statins = -1), label = "QDRB-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRB(hypertension = -1), label = "QDRB-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRB(fh_diab = -1), label = "QDRB-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRB)

### Numerical Values ###
## Age ##
tQDRB <- function(x){gQDRB(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.1, 1.3, 1.5, 1.5, 1.5, 1.3, 1.1)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRB)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (age)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## BMI ##
tQDRB <- function(x){gQDRB(bmi = x, age = 60, fpg = 4.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(5.1, 5.1, 4, 2.6, 1.7, 1.1, 0.8, 0.5)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- sapply(vec_bmi, tQDRB)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (bmi)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Height ##
tQDRB <- function(x){gQDRB(height = x, age = 60, weight = 90, fpg = 4.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(5.1, 5.1, 4, 2.6, 1.7, 1.1, 0.8, 0.5)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- sapply(vec_ht, tQDRB)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (height)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Weight ##
tQDRB <- function(x){gQDRB(weight = x, age = 60, height = 1.83, fpg = 4.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.5, 0.5, 0.9, 2.3, 4.2, 5.1, 5.1, 5.1)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, tQDRB)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (weight)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## FPG ##
tQDRB <- function(x){gQDRB(fpg = x, age = 60, height = 1.83, weight = 90)}
vec_fpg <- 2:6
risk_web <- c(0.4, 0.4, 0.7, 3.1, 13.1)
names(risk_web) <- vec_fpg

risk_fun <- sapply(vec_fpg, tQDRB)
names(risk_fun) <- vec_fpg

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (fpg)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Categorical Variables ###
## Ethnicity ##
tQDRB <- function(x){gQDRB(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Blue"), label = "QDRB-Male [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1.5, 4, 5.5, 6.4, 4.1, 2.5, 3.5, 2.8, 2.4)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRB)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (ethnicity)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Smoking ##
tQDRB <- function(x){gQDRB(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Maybe"), label = "QDRB-Male [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1.5, 1.7, 2, 2.1, 2.3)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRB)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (smoke)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Binary Variables ###
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDRB(antipsy = T), 1.8, tolerance = tol, label = "QDRB-Male [antipsy = T]")
expect_equal(tQDRB(steroids = T), 2.1, tolerance = tol, label = "QDRB-Male [steroids = T]")
expect_equal(tQDRB(cvd = T), 1.8, tolerance = tol, label = "QDRB-Male [cvd = T]")
expect_equal(tQDRB(learndiff = T), 1.9, tolerance = tol, label = "QDRB-Male [learndiff = T]")
expect_equal(tQDRB(schizobipo = T), 1.8, tolerance = tol, label = "QDRB-Male [schizobipo = T]")
expect_equal(tQDRB(statins = T), 1.8, tolerance = tol, label = "QDRB-Male [statins = T]")
expect_equal(tQDRB(hypertension = T), 1.9, tolerance = tol, label = "QDRB-Male [hypertension = T]")
expect_equal(tQDRB(fh_diab = T), 2.3, tolerance = tol, label = "QDRB-Male [fh_diab = T]")
rm(tQDRB)

### Tidy Up ###
rm(gQDRB)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDRB)
