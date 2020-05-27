#=======================#
#                       #
#### QDR2013() TESTS ####
#                       #
#=======================#

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
rQDR2013 <- function(...){round(QDR2013(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2013 <- function(...){rQDR2013(sex = "Female", ...)}

### Correct Output Format ###
expect_type(gQDR2013(age = 60, ht = 1.83, wt = 90), "double")
expect_length(gQDR2013(age = 60, ht = 1.83, wt = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2013(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  tds = -7))

dat_test[["risk_max"]] <- with(dat_test, QDR2013(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethn = "Bangladeshi",
                                                  smoke = "Heavy",
                                                  tds = 11,
                                                  ster = T,
                                                  cvd = T,
                                                  htn = T,
                                                  fhdm = T))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2013-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDRB2013-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB2013-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDRB2013-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2013(age = 60),
             regexp = "sex & age must be specified",
             label = "QDR2013-Female [is.null(sex)]")

## Age ##
expect_error(gQDR2013(),
             regexp = "sex & age must be specified",
             label = "QDR2013-Female [is.null(age)]")

## BMI, Height & Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(tQDR2013(ht = 1.83),
             regexp = "either bmi or ht & wt must be specified",
             label = "QDR2013-Female [is.null(bmi) & is.null(wt)]")
expect_error(tQDR2013(wt = 90),
             regexp = "either bmi or ht & wt must be specified",
             label = "QDR2013-Female [is.null(bmi) & is.null(ht)]")
expect_warning(tQDR2013(bmi = 30, ht = 1.83, wt = 90),
               regexp = "bmi, ht & wt all specified, ht & wt ignored",
               label = "QDR2013-Female [!is.null(bmi) & !is.null(ht) & !is.null(wt)]")
rm(tQDR2013)

### Boundaries ###
## Age ##
tQDR2013 <- function(...){gQDR2013(bmi = 30, ...)}
expect_error(tQDR2013(age = 25 - tiny),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2013-Female [age < 25]")
expect_error(tQDR2013(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2013-Female [age >= 85]]")
rm(tQDR2013)

## BMI ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(tQDR2013(bmi = (40/2.1^2) - tiny),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2013-Female [bmi < 40/2.1^2]")
expect_error(tQDR2013(bmi = (180/1.4^2) + tiny),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2013-Female [bmi > 180/1.4^2]")
expect_warning(tQDR2013(bmi = 20 - tiny),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2013-Female [bmi < 20]")
expect_warning(tQDR2013(bmi = 40 + tiny),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2013-Female [bmi > 40]")
rm(tQDR2013)

## Height ##
tQDR2013 <- function(...){gQDR2013(age = 60, wt = 90, ...)}
expect_error(tQDR2013(ht = 1.4 - tiny),
             regexp = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
             label = "QDR2013-Female [ht < 1.4]")
expect_error(tQDR2013(ht = 2.1 + tiny),
             regexp = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
             label = "QDR2013-Female [ht > 2.1]")
rm(tQDR2013)

## Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, ...)}
expect_error(tQDR2013(wt = 40 - tiny),
             regexp = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
             label = "QDR2013-Female [wt < 40]")
expect_error(tQDR2013(wt = 180 + tiny),
             regexp = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
             label = "QDR2013-Female [wt > 180]")
rm(tQDR2013)

## Townsend ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(tQDR2013(tds = -7 - tiny),
             regexp = "all\\(tds >= -7 & tds <= 11) is not TRUE",
             label = "QDR2013-Female [tds < -7]")
expect_error(tQDR2013(tds = 11 + tiny),
             regexp = "all\\(tds >= -7 & tds <= 11) is not TRUE",
             label = "QDR2013-Female [tds > 11]")
rm(tQDR2013)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2013 <- function(...){rQDR2013(sex = "Male", ...)}

### Correct Output Format ###
expect_type(gQDR2013(age = 60, ht = 1.83, wt = 90), "double")
expect_length(gQDR2013(age = 60, ht = 1.83, wt = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2013(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  tds = -7))

dat_test[["risk_max"]] <- with(dat_test, QDR2013(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethn = "Bangladeshi",
                                                  smoke = "Heavy",
                                                  tds = 11,
                                                  ster = T,
                                                  cvd = T,
                                                  htn = T,
                                                  fhdm = T))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2013-Male [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDRB2013-Male [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB2013-Male [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDRB2013-Male [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(rQDR2013(age = 60),
             regexp = "sex & age must be specified",
             label = "QDR2013-Male [is.null(sex)]")

## Age ##
expect_error(gQDR2013(),
             regexp = "sex & age must be specified",
             label = "QDR2013-Male [is.null(age)]")

## BMI, Height & Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(tQDR2013(ht = 1.83),
             regexp = "either bmi or ht & wt must be specified",
             label = "QDR2013-Male [is.null(bmi) & is.null(wt)]")
expect_error(tQDR2013(wt = 90),
             regexp = "either bmi or ht & wt must be specified",
             label = "QDR2013-Male [is.null(bmi) & is.null(ht)]")
expect_warning(tQDR2013(bmi = 30, ht = 1.83, wt = 90),
               regexp = "bmi, ht & wt all specified, ht & wt ignored",
               label = "QDR2013-Male [!is.null(bmi) & !is.null(ht) & !is.null(wt)]")
rm(tQDR2013)

### Boundaries ###
## Age ##
tQDR2013 <- function(...){gQDR2013(bmi = 30, ...)}
expect_error(tQDR2013(age = 25 - tiny),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2013-Male [age < 25]")
expect_error(tQDR2013(age = 85),
             regexp = "all\\(age >= 25 & age < 85\\) is not TRUE",
             label = "QDR2013-Male [age >= 85]]")
rm(tQDR2013)

## BMI ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(tQDR2013(bmi = (40/2.1^2) - tiny),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2013-Male [bmi < 40/2.1^2]")
expect_error(tQDR2013(bmi = (180/1.4^2) + tiny),
             regexp = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
             label = "QDR2013-Male [bmi > 180/1.4^2]")
expect_warning(tQDR2013(bmi = 20 - tiny),
               regexp = "bmi < 20\\. Setting bmi == 20",
               label = "QDR2013-Male [bmi < 20]")
expect_warning(tQDR2013(bmi = 40 + tiny),
               regexp = "bmi > 40\\. Setting bmi == 40",
               label = "QDR2013-Male [bmi > 40]")
rm(tQDR2013)

## Height ##
tQDR2013 <- function(...){gQDR2013(age = 60, wt = 90, ...)}
expect_error(tQDR2013(ht = 1.4 - tiny),
             regexp = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
             label = "QDR2013-Male [ht < 1.4]")
expect_error(tQDR2013(ht = 2.1 + tiny),
             regexp = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
             label = "QDR2013-Male [ht > 2.1]")
rm(tQDR2013)

## Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, ...)}
expect_error(tQDR2013(wt = 40 - tiny),
             regexp = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
             label = "QDR2013-Male [wt < 40]")
expect_error(tQDR2013(wt = 180 + tiny),
             regexp = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
             label = "QDR2013-Male [wt > 180]")
rm(tQDR2013)

## Townsend ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(tQDR2013(tds = -7 - tiny),
             regexp = "all\\(tds >= -7 & tds <= 11) is not TRUE",
             label = "QDR2013-Male [tds < -7]")
expect_error(tQDR2013(tds = 11 + tiny),
             regexp = "all\\(tds >= -7 & tds <= 11) is not TRUE",
             label = "QDR2013-Male [tds > 11]")
rm(tQDR2013)
