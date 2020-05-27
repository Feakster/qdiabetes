#===================#
#                   #
#### SAMPLE DATA ####
#                   #
#===================#

### License Information ###
# - Contains OS data (C) Crown copyright and database right 2019
# - Contains Royal Mail data (C) Royal Mail copyright and database right 2019
# - Source: Office for National Statistics licensed under the Open Government Licence v.3.0

### Set Observations ###
n <- 50L

### Set Seed ###
set.seed(10)

### Generate Random Data ###
dat_qdr <- data.frame(
  sex = sample(gl(2, 1, 2, c("Male", "Female")), n, replace = T),
  age = sample(25:84, n, replace = T),
  ht = sample(seq(1.40, 2.10, 0.01), n, replace = T),
  wt = sample(seq(40, 80, 0.5), n, replace = T),
  ethn = sample(gl(9, 1, 9, c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")), n, replace = T),
  smoke = sample(gl(5, 1, 5, c("Non", "Ex", "Light", "Moderate", "Heavy")), n, replace = T),
  fpg = round(runif(n, 2, 6.99), 2),
  hba1c = round(runif(n, 15, 47.99), 2),
  fhdm = sample(c(F, T), n, replace = T),
  htn = sample(c(F, T), n, replace = T),
  cvd = sample(c(F, T), n, replace = T),
  gdm = sample(c(F, T), n, replace = T),
  pcos = sample(c(F, T), n, replace = T),
  learn = sample(c(F, T), n, replace = T),
  psy = sample(c(F, T), n, replace = T),
  ster = sample(c(F, T), n, replace = T),
  stat = sample(c(F, T), n, replace = T),
  apsy = sample(c(F, T), n, replace = T)
)

dat_qdr[["bmi"]] <- with(dat_qdr, wt/ht^2)

### Rejection Sampling ###
oob <- nrow(dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, ])
while(oob != 0){
  dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, "ht"] <- sample(seq(1.40, 2.10, 0.01), oob, replace = T)
  dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, "wt"] <- sample(seq(40, 80, 0.5), oob, replace = T)
  dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, "bmi"] <- with(dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, ], wt/ht^2)
  oob <- nrow(dat_qdr[dat_qdr$bmi < 20 | dat_qdr$bmi > 40, ])
}; rm(oob)

dat_qdr[["bmi"]] <- round(dat_qdr[["bmi"]], 2)

### Import and Sample Postcode + Deprivation Data ###
load("R/sysdata.rda")
.dat_oa <- .dat_oa[.dat_oa$tds >= -7 & .dat_oa$tds <= 11, ] # Restrict range for QDR2013()
ind <- sample(1:nrow(.dat_oa), n); rm(n)
dat_tmp <- .dat_oa[ind, ]; rm(ind, .dat_oa)
dat_tmp[["postcode"]] <- with(dat_tmp, gsub("(\\w{3}$)", " \\1", postcode))

dat_qdr <- cbind(dat_qdr, dat_tmp); rm(dat_tmp)

### Remove Biological Implausibilities ###
dat_qdr[dat_qdr$sex == "Male", c("gdm", "pcos")] <- FALSE

rownames(dat_qdr) <- 1:nrow(dat_qdr)

### Reorder Variables ###
dat_qdr <- dat_qdr[, c("sex", "age",
                       "bmi", "ht", "wt", "fpg", "hba1c",
                       "ethn", "smoke", "postcode", "tds",
                       "fhdm", "htn", "cvd", "gdm", "pcos", "learn", "psy",
                       "ster", "stat", "apsy")]

### Export Data ###
save(dat_qdr, file = "data/dat_qdr.rda", version = 2, compress = "gzip"); rm(dat_qdr)
