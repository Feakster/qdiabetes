#===================#
#                   #
#### SAMPLE DATA ####
#                   #
#===================#

n <- 100L

set.seed(10)

dat_qdr <- data.frame(
  gender = sample(gl(2, 1, 2, c("Male", "Female")), n, replace = T),
  age = sample(25:84, n, replace = T),
  height = sample(seq(1.40, 2.10, 0.01), n, replace = T),
  weight = sample(seq(40, 80, 0.5), n, replace = T),
  ethnicity = sample(gl(9, 1, 9, c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")), n, replace = T),
  smoking = sample(gl(3, 1, 3, c("Non", "Ex", "Light", "Moderate", "Heavy")), n, replace = T),
  townsend = round(runif(n, -7.028634577, 13.3114711), 2),
  fpg = round(runif(n, 2, 6.99), 2),
  hba1c = round(runif(n, 15, 47.99), 2),
  antipsy = sample(c(F, T), n, replace = T),
  statins = sample(c(F, T), n, replace = T),
  steroids = sample(c(F, T), n, replace = T),
  cvd = sample(c(F, T), n, replace = T),
  gestdiab = sample(c(F, T), n, replace = T),
  learndiff = sample(c(F, T), n, replace = T),
  schizobipo = sample(c(F, T), n, replace = T),
  pcos = sample(c(F, T), n, replace = T),
  hypertension = sample(c(F, T), n, replace = T),
  fh_diab = sample(c(F, T), n, replace = T)
); rm(n)

dat_qdr[["bmi"]] <- with(dat_qdr, round(weight/height^2, 2))
dat_qdr <- dat_qdr[dat_qdr$bmi >= 20 & dat_qdr$bmi <= 40, ]

dat_qdr[dat_qdr$gender == "Male", c("gestdiab", "pcos")] <- FALSE

rownames(dat_qdr) <- 1:nrow(dat_qdr)

dat_qdr <- dat_qdr[, c("gender", "age", "height", "weight", "bmi", "ethnicity", "smoking", "townsend",
                       "fpg", "hba1c",
                       "antipsy", "statins", "steroids",
                       "cvd", "gestdiab", "learndiff", "schizobipo", "pcos", "hypertension", "fh_diab")]

save(dat_qdr, file = "data/dat_qdr.rda", version = 2, compress = "gzip"); rm(dat_qdr)
