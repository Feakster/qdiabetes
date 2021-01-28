#========================#
#                        #
#### QDR2018C() TESTS ####
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
  hba1c = seq(15, 48 - tiny, length.out = 50),
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2018C <- function(...){round(QDR2018C(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018C <- function(...){rQDR2018C(sex = "Female", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5)),
  info = "QDR2018C-Female output class"
)
expect_identical(
  current = length(gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5)),
  target = 1L,
  info = "QDR2018C-Female output length")

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018C(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
                                                  tds = -8))

dat_test[["risk_max"]] <- with(dat_test, QDR2018C(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
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
  info = "QDR2018C-Female [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018C-Female [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018C-Female [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018C-Female [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018C(age = 60, hba1c = 31.5),
  pattern = "sex, age & hba1c must be specified",
  info = "QDR2018C-Female [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018C(hba1c = 31.5),
  pattern = "sex, age & hba1c must be specified",
  info = "QDR2018C-Female [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Female [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018C(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Female [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018C(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Female [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018C)

## FPG ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(fpg = 4.5),
  pattern = "unused argument",
  info = "QDR2018C-Female [!missing(fpg)]"
)
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018C(),
  pattern = "sex, age & hba1c must be specified",
  info = "QDRA-Female [missing(hba1c)]"
)
rm(tQDR2018C)

### Boundaries ###
## Age ##
tQDR2018C <- function(...){gQDR2018C(bmi = 30, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018C-Female [age < 25]"
)
expect_error(
  current = tQDR2018C(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018C-Female [age >= 85]]"
)
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018C-Female [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018C(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018C-Female [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018C(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018C-Female [bmi < 20]"
)
expect_warning(
  current = tQDR2018C(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018C-Female [bmi > 40]"
)
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(...){gQDR2018C(age = 60, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018C-Female [ht < 1.4]"
)
expect_error(
  current = tQDR2018C(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018C-Female [ht > 2.1]"
)
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018C-Female [wt < 40]"
)
expect_error(
  current = tQDR2018C(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018C-Female [wt > 180]"
)
rm(tQDR2018C)

## Townsend ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018C-Female [tds < -8]"
)
expect_error(
  current = tQDR2018C(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018C-Female [tds > 14]"
)
rm(tQDR2018C)

## Binary Variables ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{apsy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(gdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{gdm %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(pcos = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{pcos %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Female [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018C)

### Tidy Up ###
rm(gQDR2018C)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018C <- function(...){rQDR2018C(sex = "Male", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5)),
  info = "QDR2018C-Male output class"
)
expect_identical(
  current = length(gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5)),
  target = 1L,
  info = "QDR2018C-Male output length"
)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018C(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
                                                  ethn = "WhiteNA",
                                                  smoke = "Non",
                                                  tds = -8))
dat_test[["risk_max"]] <- with(dat_test, QDR2018C(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  hba1c = hba1c,
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
  info = "QDR2018C-Male [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018C-Male [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018C-Male [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018C-Male [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018C(age = 60, hba1c = 31.5),
  pattern = "sex, age & hba1c must be specified",
  info = "QDR2018C-Male [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018C(hba1c = 31.5),
  pattern = "sex, age & hba1c must be specified",
  info = "QDR2018C-Male [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Male [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018C(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Male [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018C(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018C-Male [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018C)

## FPG ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(fpg = 4.5),
  pattern = "unused argument",
  info = "QDR2018C-Male [!missing(fpg)]"
)
rm(tQDR2018C)

## HbA1c ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018C(),
  pattern = "sex, age & hba1c must be specified",
  info = "QDR2018C-Male [missing(hba1c)]"
)
rm(tQDR2018C)

## Gestational Diabetes & PCOS ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(gdm = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018C-Male [gdm = T]"
)
expect_error(
  current = tQDR2018C(pcos = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018C-Male [pcos = T]"
)
rm(tQDR2018C)

### Boundaries ###
## Age ##
tQDR2018C <- function(...){gQDR2018C(bmi = 30, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018C-Male [age < 25]"
)
expect_error(
  current = tQDR2018C(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018C-Male [age >= 85]"
)
rm(tQDR2018C)

## BMI ##
tQDR2018C <- function(...){gQDR2018C(age = 60, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018C-Male [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018C(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018C-Male [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018C(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018C-Male [bmi < 20]"
)
expect_warning(
  current = tQDR2018C(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018C-Male [bmi > 40]"
)
rm(tQDR2018C)

## Height ##
tQDR2018C <- function(...){gQDR2018C(age = 60, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018C-Male [ht < 1.4]"
)
expect_error(
  current = tQDR2018C(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018C-Male [ht > 2.1]"
)
rm(tQDR2018C)

## Weight ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018C-Male [wt < 40]"
)
expect_error(
  current = tQDR2018C(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018C-Male [wt > 180]"
)
rm(tQDR2018C)

## Townsend ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018C-Male [tds < -8]"
)
expect_error(
  current = tQDR2018C(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018C-Male [tds > 14]"
)
rm(tQDR2018C)

## Binary Variables ##
tQDR2018C <- function(...){gQDR2018C(age = 60, ht = 1.83, wt = 90, hba1c = 31.5, ...)}
expect_error(
  current = tQDR2018C(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{apsy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018C(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018C-Male [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018C)

### Tidy Up ###
rm(gQDR2018C)

###############
### Tidy Up ###
###############

rm(tiny, dat_test, rQDR2018C)
