#=======================#
#                       #
#### QDR2013() TESTS ####
#                       #
#=======================#

### Notes ###
# - Townsend score currently not easily testable.

### Parameters ###
tiny <- 1e-8 # Small number

### Test Data ###
dat_test <- expand.grid(
  age = seq(25, 85 - tiny, length.out = 50),
  bmi = seq(20, 40, length.out = 50),
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2013 <- function(...){round(QDR2013(...), 1)}

### Input Lengths ###
## 0-Length ##
t_err <- "all\\(inputs_length\\s+%in%\\s+c\\(1, n\\)\\)\\s+is\\s+not\\s+TRUE"
expect_error(
  current = QDR2013(sex = character(0)),
  pattern = t_err,
  info = "QDR2013 [length(sex) == 0]"
)
expect_error(
  current = QDR2013(age = integer(0)),
  pattern = t_err,
  info = "QDR2013 [length(age) == 0]"
)
expect_error(
  current = QDR2013(bmi = double(0)),
  pattern = t_err,
  info = "QDR2013 [length(bmi) == 0]"
)
expect_error(
  current = QDR2013(ht = character(0)),
  pattern = t_err,
  info = "QDR2013 [length(ht) == 0]"
)
expect_error(
  current = QDR2013(wt = character(0)),
  pattern = t_err,
  info = "QDR2013 [length(wt) == 0]"
)
expect_error(
  current = QDR2013(smoke = character(0)),
  pattern = t_err,
  info = "QDR2013 [length(smoke) == 0]"
)
expect_error(
  current = QDR2013(tds = double(0)),
  pattern = t_err,
  info = "QDR2013 [length(tds) == 0]"
)
expect_error(
  current = QDR2013(fhdm = logical(0)),
  pattern = t_err,
  info = "QDR2013 [length(fhdm) == 0]"
)
expect_error(
  current = QDR2013(htn = logical(0)),
  pattern = t_err,
  info = "QDR2013 [length(htn) == 0]"
)
expect_error(
  current = QDR2013(cvd = logical(0)),
  pattern = t_err,
  info = "QDR2013 [length(cvd) == 0]"
)
expect_error(
  current = QDR2013(ster = logical(0)),
  pattern = t_err,
  info = "QDR2013 [length(ster) == 0]"
)
expect_error(
  current = QDR2013(surv = integer(0)),
  pattern = t_err,
  info = "QDR2013 [length(surv) == 0]"
)

## Discordant Lengths ##
expect_error(
  current = QDR2013(sex = character(2), age = double(3)),
  pattern = t_err,
  info = "QDR2013 [discordant input lengths]"
)
rm(t_err)

### Missing Values ###
expect_error(
  current = QDR2013(sex = NA_character_, age = 60, bmi = 30),
  pattern = "all\\(sex\\s+.+is not TRUE",
  info = "QDR2013 [is.na(sex)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = NA_real_, bmi = 30),
  pattern = "all\\(age\\s+.+is not TRUE",
  info = "QDR2013 [is.na(age)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = NA_real_),
  pattern = "all\\(bmi\\s+.+is not TRUE",
  info = "QDR2013 [is.na(bmi)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, ht = NA_real_, wt = 90),
  pattern = "all\\(ht\\s+.+is not TRUE",
  info = "QDR2013 [is.na(ht)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, ht = 1.83, wt = NA_real_),
  pattern = "all\\(wt\\s+.+is not TRUE",
  info = "QDR2013 [is.na(wt)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, ethn = NA_character_),
  pattern = "all\\(ethn\\s+.+is not TRUE",
  info = "QDR2013 [is.na(ethn)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, smoke = NA_character_),
  pattern = "all\\(smoke\\s+.+is not TRUE",
  info = "QDR2013 [is.na(smoke)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, tds = NA_real_),
  pattern = "all\\(tds\\s+.+is not TRUE",
  info = "QDR2013 [is.na(tds)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, fhdm = NA),
  pattern = "all\\(fhdm\\s+.+is not TRUE",
  info = "QDR2013 [is.na(fhdm)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, htn = NA),
  pattern = "all\\(htn\\s+.+is not TRUE",
  info = "QDR2013 [is.na(htn)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, cvd = NA),
  pattern = "all\\(cvd\\s+.+is not TRUE",
  info = "QDR2013 [is.na(cvd)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, ster = NA),
  pattern = "all\\(ster\\s+.+is not TRUE",
  info = "QDR2013 [is.na(ster)]"
)
expect_error(
  current = QDR2013(sex = "Female", age = 60, bmi = 30, surv = NA_integer_),
  pattern = "all\\(surv\\s+.+is not TRUE",
  info = "QDR2013 [is.na(surv)]"
)

### Variable Combinations ###
## Gender ##
expect_error(
  current = QDR2013(age = double(1)),
  pattern = "sex & age must be specified",
  info = "QDR2013 [missing(sex)]"
)

## Age ##
expect_error(
  current = QDR2013(sex = character(1)),
  pattern = "sex & age must be specified",
  info = "QDR2013 [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2013 <- function(...){QDR2013(sex = character(1), age = double(1), ...)}
expect_error(
  current = tQDR2013(ht = double(1)),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2013 [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2013(wt = double(1)),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2013 [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2013(bmi = double(1), ht = double(1), wt = double(1)),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2013 [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2013)

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2013 <- function(...){rQDR2013(sex = "Female", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2013(age = 60, ht = 1.83, wt = 90)),
  info = "QDR2013-Female output class"
)
expect_identical(
  current = length(gQDR2013(age = 60, ht = 1.83, wt = 90)),
  target = 1L,
  info = "QDR2013-Female output length"
)

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

expect_true(
  current = min(dat_test[["risk_min"]]) >= 0,
  info = "QDR2013-Female [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDRB2013-Female [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDRB2013-Female [max(risk) <= 100]"
)
expect_true(
  max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDRB2013-Female [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Boundaries ###
## Age ##
tQDR2013 <- function(...){gQDR2013(bmi = 30, ...)}
expect_error(
  current = tQDR2013(age = 25 - tiny),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2013 [age < 25]"
)
expect_error(
  current = tQDR2013(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2013 [age >= 85]]"
)
rm(tQDR2013)

## BMI ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(
  current = tQDR2013(bmi = (40/2.1^2) - tiny),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2013 [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2013(bmi = (180/1.4^2) + tiny),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2013 [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2013(bmi = 20 - tiny),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2013 [bmi < 20]"
)
expect_warning(
  current = tQDR2013(bmi = 40 + tiny),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2013 [bmi > 40]"
)
rm(tQDR2013)

## Height ##
tQDR2013 <- function(...){gQDR2013(age = 60, wt = 90, ...)}
expect_error(
  current = tQDR2013(ht = 1.4 - tiny),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2013 [ht < 1.4]"
)
expect_error(
  current = tQDR2013(ht = 2.1 + tiny),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2013 [ht > 2.1]"
)
rm(tQDR2013)

## Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, ...)}
expect_error(
  current = tQDR2013(wt = 40 - tiny),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2013 [wt < 40]"
)
expect_error(
  current = tQDR2013(wt = 180 + tiny),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2013 [wt > 180]"
)
rm(tQDR2013)

## Townsend ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2013(tds = -7 - tiny),
  pattern = "all\\(tds >= -7 & tds <= 11) is not TRUE",
  info = "QDR2013 [tds < -7]"
)
expect_error(
  current = tQDR2013(tds = 11 + tiny),
  pattern = "all\\(tds >= -7 & tds <= 11) is not TRUE",
  info = "QDR2013 [tds > 11]"
)
rm(tQDR2013)

### Tidy Up ###
rm(gQDR2013)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2013 <- function(...){rQDR2013(sex = "Male", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2013(age = 60, ht = 1.83, wt = 90)),
  info = "QDR2013-Male output class"
)
expect_identical(
  current = length(gQDR2013(age = 60, ht = 1.83, wt = 90)),
  target = 1L,
  info = "QDR2013-Male output length"
)

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

expect_true(
  current = min(dat_test[["risk_min"]]) >= 0,
  info = "QDR2013-Male [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDRB2013-Male [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDRB2013-Male [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDRB2013-Male [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Boundaries ###
## Age ##
tQDR2013 <- function(...){gQDR2013(bmi = 30, ...)}
expect_error(
  current = tQDR2013(age = 25 - tiny),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2013-Male [age < 25]"
)
expect_error(
  current = tQDR2013(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2013-Male [age >= 85]]"
)
rm(tQDR2013)

## BMI ##
tQDR2013 <- function(...){gQDR2013(age = 60, ...)}
expect_error(
  current = tQDR2013(bmi = (40/2.1^2) - tiny),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2013-Male [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2013(bmi = (180/1.4^2) + tiny),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2013-Male [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2013(bmi = 20 - tiny),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2013-Male [bmi < 20]"
)
expect_warning(
  current = tQDR2013(bmi = 40 + tiny),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2013-Male [bmi > 40]"
)
rm(tQDR2013)

## Height ##
tQDR2013 <- function(...){gQDR2013(age = 60, wt = 90, ...)}
expect_error(
  current = tQDR2013(ht = 1.4 - tiny),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2013-Male [ht < 1.4]"
)
expect_error(
  current = tQDR2013(ht = 2.1 + tiny),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2013-Male [ht > 2.1]"
)
rm(tQDR2013)

## Weight ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, ...)}
expect_error(
  current = tQDR2013(wt = 40 - tiny),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2013-Male [wt < 40]"
)
expect_error(
  current = tQDR2013(wt = 180 + tiny),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2013-Male [wt > 180]"
)
rm(tQDR2013)

## Townsend ##
tQDR2013 <- function(...){gQDR2013(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2013(tds = -7 - tiny),
  pattern = "all\\(tds >= -7 & tds <= 11) is not TRUE",
  info = "QDR2013-Male [tds < -7]"
)
expect_error(
  current = tQDR2013(tds = 11 + tiny),
  pattern = "all\\(tds >= -7 & tds <= 11) is not TRUE",
  info = "QDR2013-Male [tds > 11]"
)
rm(tQDR2013)

### Tidy Up ###
rm(gQDR2013)

###############
### Tidy Up ###
###############

rm(tiny, dat_test, rQDR2013)
