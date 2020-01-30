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
gQDR2013 <- function(...){rQDR2013(gender = "Female", ...)}

### Correct Output Format ###
expect_type(gQDR2013(age = 60, height = 1.83, weight = 90), "double")
expect_length(gQDR2013(age = 60, height = 1.83, weight = 90), 1)

### Correct Range ###
dat_test[["risk_min"]] <- with(dat_test, QDR2018A(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  townsend = -7))

dat_test[["risk_max"]] <- with(dat_test, QDR2018A(gender = "Female",
                                                  age = age,
                                                  bmi = bmi,
                                                  ethnicity = "Bangladeshi",
                                                  smoking = "Heavy",
                                                  townsend = 12,
                                                  steroids = T,
                                                  cvd = T,
                                                  hypertension = T,
                                                  fh_diab = T))

expect_gte(min(dat_test[["risk_min"]]), 0, label = "QDR2013-Female [min(risk) >= 0]")
expect_lte(min(dat_test[["risk_min"]]), 0.1, label = "QDRB2013-Female [min(risk) <= 0.1]")
expect_lte(max(dat_test[["risk_max"]]), 100, label = "QDRB2013-Female [max(risk) <= 100]")
expect_gte(max(dat_test[["risk_max"]]), 99.9, label = "QDRB2013-Female [max(risk) >= 99.9]")
dat_test[, c("risk_min", "risk_max")] <- NULL

##############
### Male ###
##############

### Redefine Function (Gender) ###
gQDR2013 <- function(...){rQDR2013(gender = "Female", ...)}