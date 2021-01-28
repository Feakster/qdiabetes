#========================#
#                        #
#### QDR2018A() TESTS ####
#                        #
#========================#

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
rQDR2018A <- function(...){round(QDR2018A(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018A <- function(...){rQDR2018A(sex = "Female", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018A(age = 60, ht = 1.83, wt = 90)),
  info = "QDR2018A-Female output class"
)
expect_identical(
  current = length(gQDR2018A(age = 60, ht = 1.83, wt = 90)),
  target = 1L,
  info = "QDR2018A-Female output length"
)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018A(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  tds = -8))

dat_test[["risk_max"]] <- with(dat_test, QDR2018A(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethn = "Bangladeshi",
                                                  smoke = "Heavy",
                                                  tds = 14,
                                                  apsy = T,
                                                  ster = T,
                                                  cvd = T,
                                                  gdm = T,
                                                  learn = T,
                                                  psy = T,
                                                  pcos = T,
                                                  stat = T,
                                                  htn = T,
                                                  fhdm = T))

expect_true(
  current = min(dat_test[["risk_min"]]) >= 0,
  info = "QDR2018A-Female [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018A-Female [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018A-Female [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018A-Female [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018A(age = 60),
  pattern = "sex & age must be specified",
  info = "QDR2018A-Female [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018A(),
  pattern = "sex & age must be specified",
  info = "QDR2018A-Female [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(
  current = tQDR2018A(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Female [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018A(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Female [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018A(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Female [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018A)

## FPG & HbA1c ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(fpg = 4.5),
  pattern = "unused argument",
  info = "QDR2018A-Female [!missing(fpg)]"
)
expect_error(
  current = tQDR2018A(hba1c = 31.5),
  pattern = "unused argument",
  info = "QDR2018A-Female [!missingl(hba1c)]"
)
rm(tQDR2018A)

### Boundaries ###
## Age ##
tQDR2018A <- function(...){gQDR2018A(bmi = 30, ...)}
expect_error(
  current = tQDR2018A(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018A-Female [age < 25]"
)
expect_error(
  current = tQDR2018A(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018A-Female [age >= 85]]"
)
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(
  current = tQDR2018A(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018A-Female [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018A(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018A-Female [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018A(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018A-Female [bmi < 20]"
)
expect_warning(
  current = tQDR2018A(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018A-Female [bmi > 40]"
)
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(...){gQDR2018A(age = 60, wt = 90, ...)}
expect_error(
  current = tQDR2018A(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018A-Female [ht < 1.4]"
)
expect_error(
  current = tQDR2018A(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018A-Female [ht > 2.1]"
)
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, ...)}
expect_error(
  current = tQDR2018A(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018A-Female [wt < 40]"
)
expect_error(
  current = tQDR2018A(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018A-Female [wt > 180]"
)
rm(tQDR2018A)

## Townsend ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018A-Female [tds < -8]"
)
expect_error(
  current = tQDR2018A(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018A-Female [tds > 14]"
)
rm(tQDR2018A)

## Binary Variables ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{apsy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(gdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{gdm %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(pcos = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{pcos %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Female [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018A)

### Tidy Up ###
rm(gQDR2018A)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018A <- function(...){rQDR2018A(sex = "Male", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018A(age = 60, ht = 1.83, wt = 90)),
  info = "QDR2018A-Female output class"
)
expect_identical(
  current = length(gQDR2018A(age = 60, ht = 1.83, wt = 90)),
  target = 1L,
  info = "QDR2018A-Female output length"
)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018A(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethn = "WhiteNA",
                                                  smoke = "Non",
                                                  tds = -8))
dat_test[["risk_max"]] <- with(dat_test, QDR2018A(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethn = "Bangladeshi",
                                                  smoke = "Heavy",
                                                  tds = 14,
                                                  apsy = T,
                                                  ster = T,
                                                  cvd = T,
                                                  learn = T,
                                                  psy = T,
                                                  stat = T,
                                                  htn = T,
                                                  fhdm = T))

expect_true(
  current = min(dat_test[["risk_min"]]) >= 0,
  info = "QDR2018A-Male [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018A-Male [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018A-Male [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018A-Male [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018A(age = 60),
  pattern = "sex & age must be specified",
  info = "QDR2018A-Male [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018A(),
  pattern = "sex & age must be specified",
  info = "QDR2018A-Male [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(
  current = tQDR2018A(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Male [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018A(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Male [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018A(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018A-Male [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018A)

## FPG & HbA1c ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(fpg = 4.5),
  pattern = "unused argument",
  info = "QDR2018A-Male [!missing(fpg)]"
)
expect_error(
  current = tQDR2018A(hba1c = 31.5),
  pattern = "unused argument",
  info = "QDR2018A-Male [!missing(hba1c)]"
)
rm(tQDR2018A)

## Gestational Diabetes & PCOS ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(gdm = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018A-Male [gdm = T]"
)
expect_error(
  current = tQDR2018A(pcos = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018A-Male [pcos = T]"
)
rm(tQDR2018A)

### Boundaries ###
## Age ##
tQDR2018A <- function(...){gQDR2018A(bmi = 30, ...)}
expect_error(
  current = tQDR2018A(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018A-Male [age < 25]"
)
expect_error(
  current = tQDR2018A(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018A-Male [age >= 85]"
)
rm(tQDR2018A)

## BMI ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ...)}
expect_error(
  current = tQDR2018A(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018A-Male [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018A(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018A-Male [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018A(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018A-Male [bmi < 20]"
)
expect_warning(
  current = tQDR2018A(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018A-Male [bmi > 40]"
)
rm(tQDR2018A)

## Height ##
tQDR2018A <- function(...){gQDR2018A(age = 60, wt = 90, ...)}
expect_error(
  current = tQDR2018A(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018A-Male [ht < 1.4]"
)
expect_error(
  current = tQDR2018A(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018A-Male [ht > 2.1]"
)
rm(tQDR2018A)

## Weight ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, ...)}
expect_error(
  current = tQDR2018A(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018A-Male [wt < 40]"
)
expect_error(
  current = tQDR2018A(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018A-Male [wt > 180]"
)
rm(tQDR2018A)

## Townsend ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018A-Male [tds < -8]"
)
expect_error(
  current = tQDR2018A(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018A-Male [tds > 14]"
)
rm(tQDR2018A)

## Binary Variables ##
tQDR2018A <- function(...){gQDR2018A(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018A(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{apsy %in% c(0, 1, F, T)}]")
expect_error(
  current = tQDR2018A(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018A(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018A-Male [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018A)

### Tidy Up ###
rm(gQDR2018A)

###############
### Tidy Up ###
###############

rm(tiny, dat_test, rQDR2018A)
