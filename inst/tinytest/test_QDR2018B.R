#========================#
#                        #
#### QDR2018B() TESTS ####
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
  fpg = seq(2, 7 - tiny, length.out = 50),
  KEEP.OUT.ATTRS = F
)

### Redefine Function (Rounding) ###
rQDR2018B <- function(...){round(QDR2018B(...), 1)}

##############
### Female ###
##############

### Redefine Function (Gender) ###
gQDR2018B <- function(...){rQDR2018B(sex = "Female", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5)),
  info = "QDR2018B-Female output class"
)
expect_identical(
  current = length(gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5)),
  target = 1L,
  info = "QDR2018B-Female output length"
)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018B(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  tds = -8))

dat_test[["risk_max"]] <- with(dat_test, QDR2018B(sex = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
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
  info = "QDR2018B-Female [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018B-Female [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018B-Female [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018B-Female [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018B(age = 60, fpg = 4.5),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Female [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018B(fpg = 4.5),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Female [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Female [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018B(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Female [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018B(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Female [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018B(),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Female [missing(fpg)]"
)
rm(tQDR2018B)

## HbA1c ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(hba1c = 31.5),
  pattern = "unused argument",
  info = "QDRA-Female [!missing(hba1c)]"
)
rm(tQDR2018B)

### Boundaries ###
## Age ##
tQDR2018B <- function(...){gQDR2018B(bmi = 30, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018B-Female [age < 25]"
)
expect_error(
  current = tQDR2018B(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018B-Female [age >= 85]]"
)
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018B-Female [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018B(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018B-Female [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018B(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018B-Female [bmi < 20]"
)
expect_warning(
  current = tQDR2018B(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018B-Female [bmi > 40]"
)
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(...){gQDR2018B(age = 60, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018B-Female [ht < 1.4]"
)
expect_error(
  current = tQDR2018B(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018B-Female [ht > 2.1]"
)
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018B-Female [wt < 40]"
)
expect_error(
  current = tQDR2018B(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018B-Female [wt > 180]"
)
rm(tQDR2018B)

## Townsend ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018B-Female [tds < -8]"
)
expect_error(
  current = tQDR2018B(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018B-Female [tds > 14]"
)
rm(tQDR2018B)

## Binary Variables ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{apsy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(gdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{gdm %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(pcos = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{pcos %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Female [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018B)

### Tidy Up ###
rm(gQDR2018B)

############
### Male ###
############

### Redefine Function (Gender) ###
gQDR2018B <- function(...){rQDR2018B(sex = "Male", ...)}

### Correct Output Format ###
expect_true(
  current = is.double(gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5)),
  info = "QDR208B-Male output class"
)
expect_identical(
  current = length(gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5)),
  target = 1L,
  info = "QDR2018B-Male output length"
)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018B(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
                                                  ethn = "WhiteNA",
                                                  smoke = "Non",
                                                  tds = -8))
dat_test[["risk_max"]] <- with(dat_test, QDR2018B(sex = "Male",
                                                  age = age,
                                                  bmi = bmi,
                                                  fpg = fpg,
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
  info = "QDR2018B-Male [min(risk) >= 0]"
)
expect_true(
  current = min(dat_test[["risk_min"]]) <= 0.1,
  info = "QDR2018B-Male [min(risk) <= 0.1]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) <= 100,
  info = "QDR2018B-Male [max(risk) <= 100]"
)
expect_true(
  current = max(dat_test[["risk_max"]]) >= 99.9,
  info = "QDR2018B-Male [max(risk) >= 99.9]"
)
dat_test[, c("risk_min", "risk_max")] <- NULL

### Variable Combinations ###
## Gender ##
expect_error(
  current = rQDR2018B(age = 60, fpg = 4.5),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Male [missing(sex)]"
)

## Age ##
expect_error(
  current = gQDR2018B(fpg = 4.5),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Male [missing(age)]"
)

## BMI, Height & Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(ht = 1.83),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Male [missing(bmi) & missing(wt)]"
)
expect_error(
  current = tQDR2018B(wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Male [missing(bmi) & missing(ht)]"
)
expect_error(
  current = tQDR2018B(bmi = 30, ht = 1.83, wt = 90),
  pattern = "Either bmi or ht & wt must be specified",
  info = "QDR2018B-Male [!missing(bmi) & !missing(ht) & !missing(wt)]"
)
rm(tQDR2018B)

## FPG ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, ...)}
expect_error(
  current = tQDR2018B(),
  pattern = "sex, age & fpg must be specified",
  info = "QDR2018B-Male [missing(fpg)]"
)
rm(tQDR2018B)

## HbA1c ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(hba1c = 31.5),
  pattern = "unused argument",
  info = "QDR2018B-Male [!missing(hba1c)]"
)
rm(tQDR2018B)

## Gestational Diabetes & PCOS ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(gdm = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018B-Male [gdm = T]"
)
expect_error(
  current = tQDR2018B(pcos = T),
  pattern = "pcos and gdm must be set to FALSE for male sex",
  info = "QDR2018B-Male [pcos = T]"
)
rm(tQDR2018B)

### Boundaries ###
## Age ##
tQDR2018B <- function(...){gQDR2018B(bmi = 30, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(age = 24),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018B-Male [age < 25]"
)
expect_error(
  current = tQDR2018B(age = 85),
  pattern = "all\\(age >= 25 & age < 85\\) is not TRUE",
  info = "QDR2018B-Male [age >= 85]"
)
rm(tQDR2018B)

## BMI ##
tQDR2018B <- function(...){gQDR2018B(age = 60, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(bmi = (40/2.1^2) - 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018B-Male [bmi < 40/2.1^2]"
)
expect_error(
  current = tQDR2018B(bmi = (180/1.4^2) + 1),
  pattern = "all\\(bmi >= 40/2\\.1\\^2 & bmi <= 180/1\\.4\\^2\\) is not TRUE",
  info = "QDR2018B-Male [bmi > 180/1.4^2]"
)
expect_warning(
  current = tQDR2018B(bmi = 19),
  pattern = "bmi < 20\\. Setting bmi == 20",
  info = "QDR2018B-Male [bmi < 20]"
)
expect_warning(
  current = tQDR2018B(bmi = 41),
  pattern = "bmi > 40\\. Setting bmi == 40",
  info = "QDR2018B-Male [bmi > 40]"
)
rm(tQDR2018B)

## Height ##
tQDR2018B <- function(...){gQDR2018B(age = 60, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(ht = 1.3),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018B-Male [ht < 1.4]"
)
expect_error(
  current = tQDR2018B(ht = 2.2),
  pattern = "all\\(ht >= 1\\.4 & ht <= 2\\.1\\) is not TRUE",
  info = "QDR2018B-Male [ht > 2.1]"
)
rm(tQDR2018B)

## Weight ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(wt = 39),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018B-Male [wt < 40]"
)
expect_error(
  current = tQDR2018B(wt = 181),
  pattern = "all\\(wt >= 40 & wt <= 180\\) is not TRUE",
  info = "QDR2018B-Male [wt > 180]"
)
rm(tQDR2018B)

## Townsend ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(tds = -8.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018B-Male [tds < -8]"
)
expect_error(
  current = tQDR2018B(tds = 14.1),
  pattern = "all\\(tds >= -8 & tds <= 14\\) is not TRUE",
  info = "QDR2018B-Male [tds > 14]"
)
rm(tQDR2018B)

## Binary Variables ##
tQDR2018B <- function(...){gQDR2018B(age = 60, ht = 1.83, wt = 90, fpg = 4.5, ...)}
expect_error(
  current = tQDR2018B(apsy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{apsy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(ster = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{ster %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(cvd = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{cvd %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(learn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{learn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(psy = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{psy %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(stat = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{stat %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(htn = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{htn %in% c(0, 1, F, T)}]"
)
expect_error(
  current = tQDR2018B(fhdm = -1),
  pattern = "all\\(\\w+ %in% c\\(FALSE, TRUE\\)\\) is not TRUE",
  info = "QDR2018B-Male [!{fhdm %in% c(0, 1, F, T)}]"
)
rm(tQDR2018B)

### Tidy Up ###
rm(gQDR2018B)

###############
### Tidy Up ###
###############

rm(tiny, dat_test, rQDR2018B)
