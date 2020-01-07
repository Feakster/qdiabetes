#====================#
#                    #
#### QDRA() TESTS ####
#                    #
#====================#

### Notes ###
# - 0.1 tolerance allowed in risk outputs to account for different rounding standards.
# - Townsend score currently not easily testable.

### Libraries ###
library(QDiabetes)

### Tolerance ###
tol <- 0.1

### Functions ###
aeq <- function(...){isTRUE(all.equal(...))}
rQDRA <- function(...){round(QDRA(...), 1)}

##############
### Female ###
##############

### Boundaries ###
## Age ##
class(try(QDRA(gender = "Female", age = 24, bmi = 30), silent = T)) == "try-error"
class(try(QDRA(gender = "Female", age = 25, bmi = 30), silent = T)) == "numeric"
class(try(QDRA(gender = "Female", age = 85, bmi = 30), silent = T)) == "try-error"
class(try(QDRA(gender = "Female", age = 84, bmi = 30), silent = T)) == "numeric"

### Numerical Values ###
## Age ##
vec_age <- seq(25, 80, 5)
risk_web <- c(0.2, 0.4, 0.7, 1.1, 1.7, 2.3, 3.0, 3.7, 4.2, 4.4, 4.4, 4.1)
names(risk_web) <- vec_age

risk_fun <- sapply(vec_age, function(x){rQDRA(age = x, gender = "Female", height = 1.83, weight = 90)})
names(risk_fun) <- vec_age

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

## Weight ##
vec_wt <- seq(40, 180, 10)
risk_web <- c(1, 1, 1, 1.2, 2.2, 3.7, 5.7, 8.2, 11, 13.5, 14.2, 14.2, 14.2, 14.2, 14.2)
names(risk_web) <- vec_wt

suppressWarnings({
  risk_fun <- sapply(vec_wt, function(x){rQDRA(weight = x, gender = "Female", age = 60, height = 1.83)})
})
names(risk_fun) <- vec_wt

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

## Height ##
vec_ht <- seq(1.4, 2.1, 0.1)
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- vec_ht

suppressWarnings({
  risk_fun <- sapply(vec_ht, function(x){rQDRA(height = x, gender = "Female", age = 60, weight = 90)})
})
names(risk_fun) <- vec_ht

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

## BMI ##
vec_bmi <- 90/seq(1.4, 2.1, 0.1)^2
risk_web <- c(14.2, 14.2, 10.4, 6.7, 4.2, 2.6, 1.7, 1.1)
names(risk_web) <- round(vec_bmi, 1)

suppressWarnings({
  risk_fun <- sapply(vec_bmi, function(x){rQDRA(bmi = x, gender = "Female", age = 60)})
})
names(risk_fun) <- round(vec_bmi, 1)

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

### Categorical Variables ###
## Ethnicity ##
vec_eth <- c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")
risk_web <- c(3.7, 10.3, 13.3, 20.2, 10.9, 5.5, 4.8, 8.6, 5.2)
names(risk_web) <- vec_eth

risk_fun <- sapply(vec_eth, function(x){rQDRA(eth = x, gender = "Female", age = 60, height = 1.83, weight = 90)})
names(risk_fun) <- vec_eth

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

## Smoking ##
vec_smo <- c("Non", "Ex", "Light", "Moderate", "Heavy")
risk_web <- c(3.7, 3.9, 4.8, 5.2, 6.2)
names(risk_web) <- vec_smo

risk_fun <- sapply(vec_smo, function(x){rQDRA(smoking = x, gender = "Female", age = 60, height = 1.83, weight = 90)})
names(risk_fun) <- vec_smo

rbind(risk_web, risk_fun)
mapply(aeq, risk_fun, risk_web, tolerance = tol)
rm(list = ls(pattern = "^(risk|vec)_"))

### Binary Variables ###
# risk_web <- c(5.1, 4.8, 4.4, 15.7, 4, 4.7, 5.1, 4.9, 5.6, 5.9)
# vec_rf <- c("antipsy", "steroids", "cvd", "gestdiab", "learndiff", "schizobipo", "pcos", "statins", "hypertension", "fh_diab")
# names(risk_web) <- vec_rf

# risk_web <- list(
#   antipsy = 5.1,
#   steroids = 4.8,
#   cvd = 4.4,
#   gestdiab = 15.7,
#   learndiff = 4,
#   schizobipo = 4.7,
#   pcos = 5.1,
#   statins = 4.9,
#   hypertension = 5.6,
#   fh_diab = 5.9
# )
# 
# risk_web <- list(
#   antipsy = T,
#   steroids = T,
#   cvd = T,
#   gestdiab = T,
#   learndiff = T,
#   schizobipo = T,
#   pcos = T,
#   statins = T,
#   hypertension = T,
#   fh_diab = T
# )
# 
# mapply(rQDRA, gender = "Female", age = 60, height = 1.83, weight = 90, get(risk_web))

aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, antipsy = T), 5.1, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, steroids = T), 4.8, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, cvd = T), 4.4, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, gestdiab = T), 15.7, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, learndiff = T), 4, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, schizobipo = T), 4.7, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, pcos = T), 5.1, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, statins = T), 4.9, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, hypertension = T), 5.6, tolerance = tol)
aeq(rQDRA(gender = "Female", age = 60, height = 1.83, weight = 90, fh_diab = T), 5.9, tolerance = tol)

############
### Male ###
############
