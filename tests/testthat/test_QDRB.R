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
  age = c(seq(25, 84.5, 0.5), 85L - tiny),
  height = seq(1.4, 2.1, 0.01),
  weight = c(40, 180, 0.5),
  fpg = c(seq(2, 6.5, 0.5), 7 - tiny),
  townsend = c(-7.028634577 + tiny, -7:13, 13.3114711 - tiny),
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

expect_gte(min(dat_test[["risk_min"]]), 0)
expect_lte(max(dat_test[["risk_max"]]), 100)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDRB(age = 60, fpg = 4.5))
## Age ##
expect_error(gQDRB(fpg = 4.5))
## BMI, Height & Weight ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.83))
expect_error(tQDRB(weight = 90))
expect_warning(tQDRB(bmi = 30, height = 1.83, weight = 90))
rm(tQDRB)
## HbA1c ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(hba1c = 31.5))
rm(tQDRB)

### Boundaries ###
## Age ##
tQDRB <- function(...){gQDRB(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDRB(age = 24))
expect_error(tQDRB(age = 85))
rm(tQDRB)

## BMI ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(bmi = (40/2.10^2) - 1))
expect_error(tQDRB(bmi = (180/1.4^2) + 1))
expect_warning(tQDRB(bmi = 19))
expect_warning(tQDRB(bmi = 41))
rm(tQDRB)

## Height ##
tQDRB <- function(...){gQDRB(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.3))
expect_error(tQDRB(height = 2.2))
rm(tQDRB)

## Weight ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDRB(weight = 39))
expect_error(tQDRB(weight = 181))
rm(tQDRB)

## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(fpg = 1.9))
expect_error(tQDRB(fpg = 7))
rm(tQDRB)

## Townsend ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(townsend = -7.028634578))
expect_error(tQDRB(townsend = 13.3114712))
rm(tQDRB)

### Numerical Values ###
## Age ##
tQDRB <- function(x){gQDRB(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.8, 0.9, 1, 1.1, 1, 0.9, 0.7)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRB)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (age)", expected.label = "Web Risk")
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

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (bmi)", expected.label = "Web Risk")
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

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (height)", expected.label = "Web Risk")
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

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (weight)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## FPG ##
tQDRB <- function(x){gQDRB(fpg = x, age = 60, height = 1.83, weight = 90)}
vec_wt <- 2:6
risk_web <- c(0.5, 0.1, 0.4, 2.5, 13)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, tQDRB)
})
names(risk_fun) <- vec_wt

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (weight)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Categorical Variables ###
## Ethnicity ##
tQDRB <- function(x){gQDRB(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Blue"))
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1, 2.7, 3.5, 4.5, 2.7, 1.7, 1.7, 2.2, 1.5)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRB)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (ethnicity)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Smoking ##
tQDRB <- function(x){gQDRB(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Maybe"))
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1, 1.1, 1.3, 1.4, 1.6)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRB)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Female Risk (smoke)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Binary Variables ###
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDRB(antipsy = T), 1.4, tolerance = tol)
expect_equal(tQDRB(steroids = T), 1.5, tolerance = tol)
expect_equal(tQDRB(cvd = T), 1.3, tolerance = tol)
expect_equal(tQDRB(gestdiab = T), 3, tolerance = tol)
expect_equal(tQDRB(learndiff = T), 1.3, tolerance = tol)
expect_equal(tQDRB(schizobipo = T), 1.2, tolerance = tol)
expect_equal(tQDRB(pcos = T), 1.5, tolerance = tol)
expect_equal(tQDRB(statins = T), 1.3, tolerance = tol)
expect_equal(tQDRB(hypertension = T), 1.4, tolerance = tol)
expect_equal(tQDRB(fh_diab = T), 1.5, tolerance = tol)
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
expect_error(rQDRB(age = 60, fpg = 4.5))
## Age ##
expect_error(gQDRB(fpg = 4.5))
## BMI, Height & Weight ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.83))
expect_error(tQDRB(weight = 90))
expect_warning(tQDRB(bmi = 30, height = 1.83, weight = 90))
rm(tQDRB)
## HbA1c ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(hba1c = 31.5))
rm(tQDRB)

### Boundaries ###
## Age ##
tQDRB <- function(...){gQDRB(bmi = 30, fpg = 4.5, ...)}
expect_error(tQDRB(age = 24))
expect_error(tQDRB(age = 85))
rm(tQDRB)

## BMI ##
tQDRB <- function(...){gQDRB(age = 60, fpg = 4.5, ...)}
expect_error(tQDRB(bmi = (40/2.10^2) - 1))
expect_error(tQDRB(bmi = (180/1.4^2) + 1))
expect_warning(tQDRB(bmi = 19))
expect_warning(tQDRB(bmi = 41))
rm(tQDRB)

## Height ##
tQDRB <- function(...){gQDRB(age = 60, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(height = 1.3))
expect_error(tQDRB(height = 2.2))
rm(tQDRB)

## Weight ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, fpg = 4.5, ...)}
expect_error(tQDRB(weight = 39))
expect_error(tQDRB(weight = 181))
rm(tQDRB)

## FPG ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, ...)}
expect_error(tQDRB(fpg = 1.9))
expect_error(tQDRB(fpg = 7))
rm(tQDRB)

## Townsend ##
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_error(tQDRB(townsend = -7.028634578))
expect_error(tQDRB(townsend = 13.3114712))
rm(tQDRB)

### Numerical Values ###
## Age ##
tQDRB <- function(x){gQDRB(age = x, height = 1.83, weight = 90, fpg = 4.5)}
vec_age <- seq(25, 80, 5)
risk_web <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.1, 1.3, 1.5, 1.5, 1.5, 1.3, 1.1)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, tQDRB)
names(risk_fun) <- vec_age

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (age)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

# ## BMI ##
# tQDRB <- function(x){gQDRB(bmi = x, age = 60, fpg = 4.5)}
# vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
# risk_web <- c()
# names(risk_web) <- round(vec_bmi, 1)
# 
# suppressWarnings({
#   risk_fun <- sapply(vec_bmi, tQDRB)
# })
# names(risk_fun) <- round(vec_bmi, 1)
# 
# expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (bmi)", expected.label = "Web Risk")
# rm(list = ls(pattern = "^(risk|vec)_"))
# rm(tQDRB)

# ## Height ##
# tQDRB <- function(x){gQDRB(height = x, age = 60, weight = 90, fpg = 4.5)}
# vec_ht <- seq(1.4, 2.1, 0.1)
# risk_web <- c()
# names(risk_web) <- vec_ht
# 
# suppressWarnings({
#   risk_fun <- sapply(vec_ht, tQDRB)
# })
# names(risk_fun) <- vec_ht
# 
# expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (height)", expected.label = "Web Risk")
# rm(list = ls(pattern = "^(risk|vec)_"))
# rm(tQDRB)

# ## Weight ##
# tQDRB <- function(x){gQDRB(weight = x, age = 60, height = 1.83, fpg = 4.5)}
# vec_wt <- seq(40, 180, 20)
# risk_web <- c()
# names(risk_web) <- vec_wt
# 
# suppressWarnings({
#   risk_fun <- sapply(vec_wt, tQDRB)
# })
# names(risk_fun) <- vec_wt
# 
# expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (weight)", expected.label = "Web Risk")
# rm(list = ls(pattern = "^(risk|vec)_"))
# rm(tQDRB)

# ## FPG ##
# tQDRB <- function(x){gQDRB(fpg = x, age = 60, height = 1.83, weight = 90)}
# vec_wt <- 2:6
# risk_web <- c()
# names(risk_web) <- vec_wt
# 
# suppressWarnings({
#   risk_fun <- sapply(vec_wt, tQDRB)
# })
# names(risk_fun) <- vec_wt
# 
# expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (weight)", expected.label = "Web Risk")
# rm(list = ls(pattern = "^(risk|vec)_"))
# rm(tQDRB)

### Categorical Variables ###
## Ethnicity ##
tQDRB <- function(x){gQDRB(ethnicity = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Blue"))
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(1.5, 4, 5.5, 6.4, 4.1, 2.5, 3.5, 2.8, 2.4)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, tQDRB)
names(risk_fun) <- vec_eth

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (ethnicity)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

## Smoking ##
tQDRB <- function(x){gQDRB(smoking = x, age = 60, height = 1.83, weight = 90, fpg = 4.5)}
expect_error(tQDRB(x = "Maybe"))
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(1.5, 1.7, 2, 2.1, 2.3)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, tQDRB)
names(risk_fun) <- vec_smo

expect_equal(risk_fun, risk_web, tolerance = tol, label = "QDRB-Male Risk (smoke)", expected.label = "Web Risk")
rm(list = ls(pattern = "^(risk|vec)_"))
rm(tQDRB)

### Binary Variables ###
tQDRB <- function(...){gQDRB(age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
expect_equal(tQDRB(antipsy = T), 1.8, tolerance = tol)
expect_equal(tQDRB(steroids = T), 2.1, tolerance = tol)
expect_equal(tQDRB(cvd = T), 1.8, tolerance = tol)
expect_equal(tQDRB(learndiff = T), 1.9, tolerance = tol)
expect_equal(tQDRB(schizobipo = T), 1.8, tolerance = tol)
expect_equal(tQDRB(statins = T), 1.8, tolerance = tol)
expect_equal(tQDRB(hypertension = T), 1.9, tolerance = tol)
expect_equal(tQDRB(fh_diab = T), 2.3, tolerance = tol)
rm(tQDRB)

### Tidy Up ###
rm(gQDRB)

###############
### Tidy Up ###
###############

rm(tol, tiny, dat_test, rQDRB)
