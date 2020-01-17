#====================#
#                    #
#### QDRC() TESTS ####
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
  age = seq(25, 85 - tiny, length.out = 20),
  height = seq(1.4, 2.1, length.out = 20),
  weight = seq(40, 180, length.out = 20),
  hba1c = seq(15, 48 - tiny, length.out = 20),
  KEEP.OUT.ATTRS = F
)

dat_test[["bmi"]] <- with(dat_test, weight/height^2)
dat_test <- dat_test[dat_test$bmi >= 20 & dat_test$bmi <= 40, ]

### Redefine Function (Rounding) ###
rQDRC <- function(...){round(QDRC(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDRC <- function(...){rQDRC(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5), "double")
expect_length(gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, mapply(QDRC,
                                                age = age,
                                                height = height,
                                                weight = weight,
                                                hba1c = hba1c,
                                                MoreArgs = list(
                                                  gender = "Female",
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non",
                                                  townsend = -7.028634577)))
dat_test[["risk_max"]] <- with(dat_test, mapply(QDRC,
                                                age = age,
                                                height = height, 
                                                weight = weight,
                                                hba1c = hba1c,
                                                MoreArgs = list(
                                                  gender = "Female",
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
                                                  fh_diab = T)))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDRC-Female [min(risk) >= 0]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRC-Female [min(risk) <= 100]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRC(age = 60, hba1c = 31.5), label = "QDRC-Female [is.null(gender)]")
## Age ##
expect_error(gQDRC(hba1c = 31.5), label = "QDRC-Female [is.null(age)]")
## BMI, Height & Weight ##
tQDRC <- function(...){gQDRC(age = 60, hba1c = 31.5, ...)}
expect_error(tQDRC(height = 1.83), label = "QDRC-Female [is.null(bmi) & is.null(weight)]")
expect_error(tQDRC(weight = 90), label = "QDRC-Female [is.null(bmi) & is.null(height)]")
expect_warning(tQDRC(bmi = 30, height = 1.83, weight = 90), label = "QDRC-Female [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRC)
## FPG ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(fpg = 4.5), label = "QDRC-Female [!is.null(fpg)]")
rm(tQDRC)
## HbA1c ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRC(), label = "QDRA-Female [is.null(hba1c)]")
rm(tQDRC)

### Boundaries ###
## Age ##
tQDRC <- function(...){gQDRC(bmi = 30, hba1c = 31.5, ...)}
expect_error(tQDRC(age = 24), label = "QDRC-Female [age < 25]")
expect_error(tQDRC(age = 85), label = "QDRC-Female [age >= 85]")
rm(tQDRC)

## BMI ##
tQDRC <- function(...){gQDRC(age = 60, hba1c = 31.5, ...)}
expect_error(tQDRC(bmi = (40/2.1^2) - 1), label = "QDRC-Female [bmi < 40/2.1^2]")
expect_error(tQDRC(bmi = (180/1.4^2) + 1), label = "QDRC-Female [bmi > 180/1.4^2]")
expect_warning(tQDRC(bmi = 19), label = "QDRC-Female [bmi < 20]")
expect_warning(tQDRC(bmi = 41), label = "QDRC-Female [bmi > 41]")
rm(tQDRC)

## Height ##
tQDRC <- function(...){gQDRC(age = 60, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(height = 1.3), label = "QDRC-Female [height < 1.4]")
expect_error(tQDRC(height = 2.2), label = "QDRC-Female [height > 2.1]")
rm(tQDRC)

## Weight ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, hba1c = 31.5, ...)}
expect_error(tQDRC(weight = 39), label = "QDRC-Female [weight < 40]")
expect_error(tQDRC(weight = 181), label = "QDRC-Female [weight > 180]")
rm(tQDRC)

## FPG ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRC(fpg = 1.9), label = "QDRC-Female [fpg < 2]")
expect_error(tQDRC(fpg = 7), label = "QDRC-Female [fpg >= 7]")
rm(tQDRC)

## Townsend ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(townsend = -7.028634578), label = "QDRC-Female [townsend < -7.028634577]")
expect_error(tQDRC(townsend = 13.3114712), label = "QDRC-Female [townsend > 13.3114711]")
rm(tQDRC)

## Binary Variables ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(antipsy = -1), label = "QDRC-Female [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRC(steroids = -1), label = "QDRC-Female [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRC(cvd = -1), label = "QDRC-Female [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRC(gestdiab = -1), label = "QDRC-Female [!{gestdiab %in% c(0, 1, F, T)}]")
expect_error(tQDRC(learndiff = -1), label = "QDRC-Female [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRC(schizobipo = -1), label = "QDRC-Female [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRC(pcos = -1), label = "QDRC-Female [!{pcos %in% c(0, 1, F, T)}]")
expect_error(tQDRC(statins = -1), label = "QDRC-Female [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRC(hypertension = -1), label = "QDRC-Female [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRC(fh_diab = -1), label = "QDRC-Female [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRC)

### Numerical Values ###
## Age ##
tQDRC <- function(x){gQDRC(age = x, height = 1.83, weight = 90, hba1c = 31.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.5)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRC)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(age)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## BMI ##
tQDRC <- function(x){gQDRC(bmi = x, age = 60, hba1c = 31.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(2.3, 2.3, 1.8, 1.3, 0.9, 0.6, 0.4, 0.3)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- sapply(vec_bmi, tQDRC)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(bmi)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Height ##
tQDRC <- function(x){gQDRC(height = x, age = 60, weight = 90, hba1c = 31.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(2.3, 2.3, 1.8, 1.3, 0.9, 0.6, 0.4, 0.3)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- sapply(vec_ht, tQDRC)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(height)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Weight ##
tQDRC <- function(x){gQDRC(weight = x, age = 60, height = 1.83, hba1c = 31.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.3, 0.3, 0.5, 1.1, 1.9, 2.3, 2.3, 2.3)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, tQDRC)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(weight)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## HbA1c ##
tQDRC <- function(x){gQDRC(hba1c = x, age = 60, height = 1.83, weight = 90)}
vec_hba1c <- seq(15, 45, 5)
risk_web <- c(0, 0, 0.1, 0.5, 1.8, 5.2, 13.6)
names(risk_web) <- vec_hba1c

risk_fun <- sapply(vec_hba1c, tQDRC)
names(risk_fun) <- vec_hba1c

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(hba1c)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

### Categorical Variables ###
## Ethnicity ##
tQDRC <- function(x){gQDRC(ethnicity = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDRC(x = "Blue"), label = "QDRC-Female [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(0.8, 1.4, 1.7, 2.6, 1.6, 0.9, 0.8, 1.4, 0.9)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRC)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(ethnicity)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Smoking ##
tQDRC <- function(x){gQDRC(smoking = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDRC(x = "Maybe"), label = "QDRC-Female [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(0.8, 0.8, 0.9, 0.9, 1.1)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRC)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Female [range(smoking)]", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

### Binary Variables ###
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_equal(tQDRC(antipsy = T), 1.1, tolerance = tol, label = "QDRC-Female [antipsy = T]")
expect_equal(tQDRC(steroids = T), 0.9, tolerance = tol, label = "QDRC-Female [steroids = T]")
expect_equal(tQDRC(cvd = T), 0.9, tolerance = tol, label = "QDRC-Female [cvd = T]")
expect_equal(tQDRC(gestdiab = T), 2.4, tolerance = tol, label = "QDRC-Female [gestdiab = T]")
expect_equal(tQDRC(learndiff = T), 0.9, tolerance = tol, label = "QDRC-Female [learndiff = T]")
expect_equal(tQDRC(schizobipo = T), 1.1, tolerance = tol, label = "QDRC-Female [schizobipo = T]")
expect_equal(tQDRC(pcos = T), 1.1, tolerance = tol, label = "QDRC-Female [pcos = T]")
expect_equal(tQDRC(statins = T), 0.9, tolerance = tol, label = "QDRC-Female [statins = T]")
expect_equal(tQDRC(hypertension = T), 1.2, tolerance = tol, label = "QDRC-Female [hypertension = T]")
expect_equal(tQDRC(fh_diab = T), 1.2, tolerance = tol, label = "QDRC-Female [fh_diab = T]")
rm(tQDRC)

### Tidy Up ###
rm(gQDRC)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDRC <- function(...){rQDRC(gender = "Male", ...)}

### Correct Output Format ###
expect_type(gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5), "double")
expect_length(gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, mapply(QDRC,
                                                age = age,
                                                height = height,
                                                weight = weight,
                                                hba1c = hba1c,
                                                MoreArgs = list(
                                                  gender = "Male",
                                                  ethnicity = "WhiteNA",
                                                  smoking = "Non",
                                                  townsend = -7.028634577)))
dat_test[["risk_max"]] <- with(dat_test, mapply(QDRC,
                                                age = age,
                                                height = height, 
                                                weight = weight,
                                                hba1c = hba1c,
                                                MoreArgs = list(
                                                  gender = "Male",
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
                                                  fh_diab = T)))

expect_gte(min(dat_test[["risk_min"]]), 0)
expect_lte(max(dat_test[["risk_max"]]), 100)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRC(age = 60, hba1c = 31.5), label = "QDRC-Male [is.null(gender)]")
## Age ##
expect_error(gQDRC(hba1c = 31.5), label = "QDRC-Male [is.null(age)]")
## BMI, Height & Weight ##
tQDRC <- function(...){gQDRC(age = 60, hba1c = 31.5, ...)}
expect_error(tQDRC(height = 1.83), label = "QDRC-Male [is.null(bmi) & is.null(weight)]")
expect_error(tQDRC(weight = 90), label = "QDRC-Male [is.null(bmi) & is.null(height)]")
expect_warning(tQDRC(bmi = 30, height = 1.83, weight = 90), label = "QDRC-Male [!is.null(bmi) & !is.null(height) & !is.null(weight)]")
rm(tQDRC)
## FPG ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(fpg = 4.5), label = "QDRC-Male [!is.null(fpg)]")
rm(tQDRC)
## HbA1c ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRC(), label = "QDRC-Male [is.null(hba1c)]")
rm(tQDRC)

### Boundaries ###
## Age ##
tQDRC <- function(...){gQDRC(bmi = 30, hba1c = 31.5, ...)}
expect_error(tQDRC(age = 24), label = "QDRC-Male [age < 25]")
expect_error(tQDRC(age = 85), label = "QDRC-Male [age >= 85]")
rm(tQDRC)

## BMI ##
tQDRC <- function(...){gQDRC(age = 60, hba1c = 31.5, ...)}
expect_error(tQDRC(bmi = (40/2.1^2) - 1), label = "QDRC-Male [bmi < 40/2.1^2]")
expect_error(tQDRC(bmi = (180/1.4^2) + 1), label = "QDRC-Male [bmi > 40/2.1^2]")
expect_warning(tQDRC(bmi = 19), label = "QDRC-Male [bmi < 20]")
expect_warning(tQDRC(bmi = 41), label = "QDRC-Male [bm > 40]")
rm(tQDRC)

## Height ##
tQDRC <- function(...){gQDRC(age = 60, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(height = 1.3), label = "QDRC-Male [height < 1.4]")
expect_error(tQDRC(height = 2.2), label = "QDRC-Male [height > 2.1]")
rm(tQDRC)

## Weight ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, hba1c = 31.5, ...)}
expect_error(tQDRC(weight = 39), label = "QDRC-Male [weight < 40]")
expect_error(tQDRC(weight = 181), label = "QDRC-Male [weight > 180]")
rm(tQDRC)

## FPG ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRC(fpg = 1.9), label = "QDRC-Male [fpg < 2.0]")
expect_error(tQDRC(fpg = 7), label = "QDRC-Male [fpg >= 7]")
rm(tQDRC)

## Townsend ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(townsend = -7.028634578), label = "QDRC-Male [townsend < -7.028634577]")
expect_error(tQDRC(townsend = 13.3114712), label = "QDRC-Male [townsend > 13.3114711]")
rm(tQDRC)

## Binary Variables ##
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_error(tQDRC(antipsy = -1), label = "QDRC-Male [!{antipsy %in% c(0, 1, F, T)}]")
expect_error(tQDRC(steroids = -1), label = "QDRC-Male [!{steroids %in% c(0, 1, F, T)}]")
expect_error(tQDRC(cvd = -1), label = "QDRC-Male [!{cvd %in% c(0, 1, F, T)}]")
expect_error(tQDRC(learndiff = -1), label = "QDRC-Male [!{learndiff %in% c(0, 1, F, T)}]")
expect_error(tQDRC(schizobipo = -1), label = "QDRC-Male [!{schizobipo %in% c(0, 1, F, T)}]")
expect_error(tQDRC(statins = -1), label = "QDRC-Male [!{statins %in% c(0, 1, F, T)}]")
expect_error(tQDRC(hypertension = -1), label = "QDRC-Male [!{hypertension %in% c(0, 1, F, T)}]")
expect_error(tQDRC(fh_diab = -1), label = "QDRC-Male [!{fh_diab %in% c(0, 1, F, T)}]")
rm(tQDRC)

## Numerical Values ###
## Age ##
tQDRC <- function(x){gQDRC(age = x, height = 1.83, weight = 90, hba1c = 31.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.1, 1.3, 1.5, 1.5, 1.5, 1.3, 1.1)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRC)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (age)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## BMI ##
tQDRC <- function(x){gQDRC(bmi = x, age = 60, hba1c = 31.5)}
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(5, 5, 3.9, 2.6, 1.7, 1.1, 0.7, 0.5)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- sapply(vec_bmi, tQDRC)
})
names(risk_fun) <- round(vec_bmi, 1)

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (bmi)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Height ##
tQDRC <- function(x){gQDRC(height = x, age = 60, weight = 90, hba1c = 31.5)}
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(5, 5, 3.9, 2.6, 1.7, 1.1, 0.7, 0.5)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- sapply(vec_ht, tQDRC)
})
names(risk_fun) <- vec_ht

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (height)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Weight ##
tQDRC <- function(x){gQDRC(weight = x, age = 60, height = 1.83, hba1c = 31.5)}
vec_wt <- seq(40, 180, 20)
risk_web <- c(0.5, 0.5, 0.9, 2.2, 4.1, 5, 5, 5)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, tQDRC)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (weight)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## HbA1c ##
tQDRC <- function(x){gQDRC(hba1c = x, age = 60, height = 1.83, weight = 90)}
vec_hba1c <- seq(15, 45, 5)
risk_web <- c(0, 0.1, 0.3, 1.1, 3, 7.5, 17)
names(risk_web) <- vec_hba1c

risk_fun <- sapply(vec_hba1c, tQDRC)
names(risk_fun) <- vec_hba1c

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (hba1c)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

### Categorical Variables ###
## Ethnicity ##
tQDRC <- function(x){gQDRC(ethnicity = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDRC(x = "Blue"), label = "QDRC-Male [ethnicity = 'Blue']")
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1.5, 2.9, 3.3, 4.3, 3.1, 1.8, 2.1, 2.1, 1.8)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRC)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (ethnicity)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

## Smoking ##
tQDRC <- function(x){gQDRC(smoking = x, age = 60, height = 1.83, weight = 90, hba1c = 31.5)}
expect_error(tQDRC(x = "Maybe"), label = "QDRC-Male [smoking = 'Maybe']")
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1.5, 1.6, 1.7, 1.6, 1.8)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRC)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRC-Male Risk (smoke)", expected.label = "ClinRisk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRC)

### Binary Variables ###
tQDRC <- function(...){gQDRC(age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}
expect_equal(tQDRC(antipsy = T), 1.8, tolerance = tol, label = "QDRC-Male [antipsy = T]")
expect_equal(tQDRC(steroids = T), 1.7, tolerance = tol, label = "QDRC-Male [steroids = T]")
expect_equal(tQDRC(cvd = T), 1.7, tolerance = tol, label = "QDRC-Male [cvd = T]")
expect_equal(tQDRC(learndiff = T), 1.6, tolerance = tol, label = "QDRC-Male [learndiff = T]")
expect_equal(tQDRC(schizobipo = T), 1.9, tolerance = tol, label = "QDRC-Male [schizobipo = T]")
expect_equal(tQDRC(statins = T), 1.6, tolerance = tol, label = "QDRC-Male [statins = T]")
expect_equal(tQDRC(hypertension = T), 2, tolerance = tol, label = "QDRC-Male [hypertension = T]")
expect_equal(tQDRC(fh_diab = T), 2.3, tolerance = tol, label = "QDRC-Male [fh_diab = T]")
rm(tQDRC)

### Tidy Up ###
rm(gQDRC)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDRC)
