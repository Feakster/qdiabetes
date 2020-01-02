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
  height = sample((140:210)/100, n, replace = T),
  weight = sample(40:180, n, replace = T),
  fpg = round(runif(n, 2, 6.99), 2),
  hba1c = round(runif(n, 15, 47.99), 2),
  ethnicity = sample(gl(9, 1, 9, c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")), n, replace = T),
  smoking = sample(gl(3, 1, 3, c("Never", "Ex", "Current")), n, replace = T),
  townsend = sample(-6:11, n, replace = T),
  antipsy = sample(c(F, T), n, replace = T),
  steroids = sample(c(F, T), n, replace = T),
  cvd = sample(c(F, T), n, replace = T),
  gestdiab = sample(c(F, T), n, replace = T),
  learndiff = sample(c(F, T), n, replace = T),
  schizobipo = sample(c(F, T), n, replace = T),
  pcos = sample(c(F, T), n, replace = T),
  statins = sample(c(F, T), n, replace = T),
  hypertension = sample(c(F, T), n, replace = T),
  fh_diab = sample(c(F, T), n, replace = T)
); rm(n)
dat_qdr[["bmi"]] <- with(dat_qdr, round(weight/height^2, 2))
dat_qdr <- dat_qdr[dat_qdr$bmi >= 20 & dat_qdr$bmi <= 40, ]

save(dat_qdr, file = "data/dat_qdr.RData")
