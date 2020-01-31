#======================#
#                      #
#### QDIABETES-2013 ####
#                      #
#======================#

### License Information ###
# The QDR2013 function is part of the QDiabetes package, and is for
# calculating the 10-year risk of developing type-2 diabetes.
# Copyright (C) 2020  University of Oxford

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# The additional terms stated in the license of the source material
# mandate that the following disclaimer be included alongside the license
# notice (above):

# The initial version of this file, to be found at
# http://svn.clinrisk.co.uk/opensource/qdiabetes, faithfully implements
# QDiabetes-2013. We have released this code under the GNU Lesser General
# Public License to enable others to implement the algorithm faithfully.
# However, the nature of the GNU Lesser General Public License is such that
# we cannot prevent, for example, someone altering the coefficients.
# We stress, therefore, that it is the responsibility of the end user to
# check that the source that they receive produces the same results as the
# original code posted at http://svn.clinrisk.co.uk/opensource/qdiabetes.
# Inaccurate implementations of risk scores can lead to wrong patients
# being given the wrong treatment.

### Notes ###
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - townsend >= -7 & townsend <= 11
# - surv >= 1 & surv <= 10

### Function ###
QDR2013 <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, steroids = FALSE, cvd = FALSE, hypertension = FALSE, fh_diab = FALSE, surv = 10L){
  ## Stop Conditions ##
  if(any(is.null(gender), is.null(age))) stop("gender & age must be specified")
  if(is.null(bmi) & any(is.null(height), is.null(weight))) stop("either bmi or height & weight must be specified")
  inputs <- list(gender, age, bmi, height, weight, ethnicity, smoking, townsend, steroids, cvd, hypertension, fh_diab, surv)
  inputs_length <- sapply(inputs, length)
  n <- max(inputs_length)
  stopifnot(all(inputs_length %in% c(0:1, n)))
  stopifnot(all(gender %in% c("Female", "Male")))
  stopifnot(all(ethnicity %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")))
  stopifnot(all(smoking %in% c("Non", "Ex", "Light", "Moderate", "Heavy")))
  stopifnot(all(age >= 25 & age < 85))
  stopifnot(all(height >= 1.4 & height <= 2.1))
  stopifnot(all(weight >= 40 & weight <= 180))
  stopifnot(all(townsend >= -7 & townsend <= 11))
  stopifnot(all(steroids %in% c(FALSE, TRUE)))
  stopifnot(all(cvd %in% c(FALSE, TRUE)))
  stopifnot(all(hypertension %in% c(FALSE, TRUE)))
  stopifnot(all(fh_diab %in% c(FALSE, TRUE)))
  stopifnot(all(!is.logical(surv)))
  stopifnot(all(surv %in% 1:10))
  
  ## BMI Pre-Procession ##
  if(all(!is.null(bmi), !is.null(height), !is.null(weight))){
    warning("bmi, height & weight all specified, height & weight ignored", call. = FALSE)
    bmi[is.na(bmi)] <- weight/height^2
  } else if(is.null(bmi)) bmi <- weight/height^2
  stopifnot(all(bmi >= 40/2.1^2 & bmi <= 180/1.4^2))
  if(any(bmi < 20)){
    warning("bmi < 20. Setting bmi == 20", call. = FALSE)
    bmi[bmi < 20] <- 20
  }
  if(any(bmi > 40)){
    warning("bmi > 40. Setting bmi == 40", call. = FALSE)
    bmi[bmi > 40] <- 40
  }
  
  ## Harmonize Input Lengths ##
  if(length(gender) == 1) gender <- rep(gender, n)
  if(length(age) == 1) age <- rep(age, n)
  if(length(bmi) == 1) bmi <- rep(bmi, n)
  if(length(ethnicity) == 1) ethnicity <- rep(ethnicity, n)
  if(length(smoking) == 1) smoking <- rep(smoking, n)
  if(length(townsend) == 1) townsend <- rep(townsend, n)
  if(length(steroids) == 1) steroids <- rep(steroids, n)
  if(length(cvd) == 1) cvd <- rep(cvd, n)
  if(length(hypertension) == 1) hypertension <- rep(hypertension, n)
  if(length(fh_diab) == 1) fh_diab <- rep(fh_diab, n)
  if(length(surv) == 1) surv <- rep(surv, n)
  
  ## Intermediate Vectors ##
  vec_surv <- rep(NA_real_, n)
  vec_eth <- rep(0, n)
  vec_smok <- rep(0, n)
  dage <- age/10
  age_1 <- rep(NA_real_, n)
  age_2 <- dage^3
  dbmi <- bmi/10
  bmi_1 <- rep(NA_real_, n)
  bmi_2 <- dbmi^3
  bin <- rep(NA_real_, n)
  int <- rep(NA_real_, n)
  risk <- rep(NA_real_, n)
  
  ## Gender Indices ##
  ind_f <- which(gender == "Female")
  ind_m <- which(gender == "Male")
  
  ## Female ##
  # Survivor Function #
  vec_surv[gender == "Female" & surv == 1] <- 0.998714804649353
  vec_surv[gender == "Female" & surv == 2] <- 0.997435748577118
  vec_surv[gender == "Female" & surv == 3] <- 0.996052920818329
  vec_surv[gender == "Female" & surv == 4] <- 0.99456250667572
  vec_surv[gender == "Female" & surv == 5] <- 0.992949724197388
  vec_surv[gender == "Female" & surv == 6] <- 0.991141080856323
  vec_surv[gender == "Female" & surv == 7] <- 0.989293158054352
  vec_surv[gender == "Female" & surv == 8] <- 0.987293541431427
  vec_surv[gender == "Female" & surv == 9] <- 0.98513388633728
  vec_surv[gender == "Female" & surv == 10] <- 0.982810735702515
  # vec_surv[gender == "Female" & surv == 11] <- 0.980465650558472
  # vec_surv[gender == "Female" & surv == 12] <- 0.978020071983337
  # vec_surv[gender == "Female" & surv == 13] <- 0.97549307346344
  # vec_surv[gender == "Female" & surv == 14] <- 0.972945988178253
  # vec_surv[gender == "Female" & surv == 15] <- 0.97035014629364
  
  # Ethnicity #
  vec_eth[gender == "Female" & ethnicity == "Indian"] <- 1.2672136244963337
  vec_eth[gender == "Female" & ethnicity == "Pakistani"] <- 1.4277605208830098
  vec_eth[gender == "Female" & ethnicity == "Bangladeshi"] <- 1.8624060798103199
  vec_eth[gender == "Female" & ethnicity == "OtherAsian"] <- 1.2379988338989651
  vec_eth[gender == "Female" & ethnicity == "BlackCaribbean"] <- 0.47090341729076779
  vec_eth[gender == "Female" & ethnicity == "BlackAfrican"] <- 0.34764009017031605
  vec_eth[gender == "Female" & ethnicity == "Chinese"] <- 1.1587283467731935
  vec_eth[gender == "Female" & ethnicity == "Other"] <- 0.73354993250103151
  
  # Smoking #
  vec_smok[gender == "Female" & smoking == "Ex"] <- 0.10125370249475051
  vec_smok[gender == "Female" & smoking == "Light"] <- 0.19155205643806134
  vec_smok[gender == "Female" & smoking == "Moderate"] <- 0.30918941361433339
  vec_smok[gender == "Female" & smoking == "Heavy"] <- 0.46467303926938208
  
  # Age #
  age_1[ind_f] <- sqrt(dage[ind_f])
  age_1[ind_f] <- age_1[ind_f] - 2.135220289230347
  age_2[ind_f] <- age_2[ind_f] - 94.766799926757813
  age[ind_f] <- 4.3848331212989669*age_1[ind_f] - 0.0049763964406541149*age_2[ind_f]
  
  # BMI #
  bmi_1[ind_f] <- dbmi[ind_f]
  bmi_1[ind_f] <- bmi_1[ind_f] - 2.549620866775513
  bmi_2[ind_f] <- bmi_2[ind_f] - 16.573980331420898
  bmi[ind_f] <- 3.3753336326064329*bmi_1[ind_f] - 0.063162848866731833*bmi_2[ind_f]
  
  # Townsend #
  townsend[ind_m] <- townsend[ind_m] + 0.224075347185135
  townsend[ind_m] <- 0.043272699299863597*townsend[ind_m]
  
  # Binary Variables #
  bin[ind_f] <- Reduce("+", list(0.2681990966241487*steroids[ind_f],
                                 0.35961768309842529*cvd[ind_f],
                                 0.53145984369747257*hypertension[ind_f],
                                 0.73153588458376406*fh_diab[ind_f]))
  
  # Interaction Terms #
  int[ind_f] <- Reduce("+", list(1.303783287399799*age_1[ind_f]*bmi_1[ind_f],
                                 -0.070829371776904612*age_1[ind_f]*bmi_2[ind_f],
                                 -0.79682668158342518*age_1[ind_f]*fh_diab[ind_f],
                                 -0.0067725323761278549*age_2[ind_f]*bmi_1[ind_f],
                                 0.00023749807286661167*age_2[ind_f]*bmi_2[ind_f],
                                 0.0017048228889394394*age_2[ind_f]*fh_diab[ind_f]))
  
  ## Male ##
  # Survivor Function #
  vec_surv[gender == "Male" & surv == 1] <- 0.998213708400726
  vec_surv[gender == "Male" & surv == 2] <- 0.996353209018707
  vec_surv[gender == "Male" & surv == 3] <- 0.994382798671722
  vec_surv[gender == "Male" & surv == 4] <- 0.992213606834412
  vec_surv[gender == "Male" & surv == 5] <- 0.989733397960663
  vec_surv[gender == "Male" & surv == 6] <- 0.9870645403862
  vec_surv[gender == "Male" & surv == 7] <- 0.984254062175751
  vec_surv[gender == "Male" & surv == 8] <- 0.981255292892456
  vec_surv[gender == "Male" & surv == 9] <- 0.977990627288818
  vec_surv[gender == "Male" & surv == 10] <- 0.974455237388611
  # vec_surv[gender == "Male" & surv == 11] <- 0.970843732357025
  # vec_surv[gender == "Male" & surv == 12] <- 0.967315018177032
  # vec_surv[gender == "Male" & surv == 13] <- 0.963437378406525
  # vec_surv[gender == "Male" & surv == 14] <- 0.959633111953735
  # vec_surv[gender == "Male" & surv == 15] <- 0.955690681934357
  
  # Ethnicity #
  vec_eth[gender == "Male" & ethnicity == "Indian"] <- 1.2366090720913343
  vec_eth[gender == "Male" & ethnicity == "Pakistani"] <- 1.4716746107789032
  vec_eth[gender == "Male" & ethnicity == "Bangladeshi"] <- 1.8073235649498174
  vec_eth[gender == "Male" & ethnicity == "OtherAsian"] <- 1.2056055595936399
  vec_eth[gender == "Male" & ethnicity == "BlackCaribbean"] <- 0.60323699759387661
  vec_eth[gender == "Male" & ethnicity == "BlackAfrican"] <- 0.90954362074527373
  vec_eth[gender == "Male" & ethnicity == "Chinese"] <- 0.91376046329275129
  vec_eth[gender == "Male" & ethnicity == "Other"] <- 0.71237190459907795
  
  # Smoking #
  vec_smok[gender == "Male" & smoking == "Ex"] <- 0.16182385823959777
  vec_smok[gender == "Male" & smoking == "Light"] <- 0.1902020385619117
  vec_smok[gender == "Male" & smoking == "Moderate"] <- 0.32106361793124671
  vec_smok[gender == "Male" & smoking == "Heavy"] <- 0.41400013017974946
  
  # Age #
  age_1[ind_m] <- log(dage[ind_m])
  age_1[ind_m] <- age_1[ind_m] - 1.496771812438965
  age_2[ind_m] <- age_2[ind_m] - 89.149559020996094
  age[ind_m] <- 4.420559832337168*age_1[ind_m] - 0.0041132238299394193*age_2[ind_m]
  
  # BMI #
  bmi_1[ind_m] <- dbmi[ind_m]^2
  bmi_1[ind_m] <- bmi_1[ind_m] - 6.832604885101318
  bmi_2[ind_m] <- bmi_2[ind_m] - 17.859918594360352
  bmi[ind_m] <- 1.1169895991721528*bmi_1[ind_m] - 0.17935295302512691*bmi_2[ind_m]
  
  # Townsend #
  townsend[ind_m] <- townsend[ind_m] + 0.132148191332817
  townsend[ind_m] <- 0.029153081590382265*townsend[ind_m]
  
  # Binary Variables #
  bin[ind_m] <- Reduce("+", list(0.20598119799056924*steroids[ind_m],
                                 0.39147284549905031*cvd[ind_m],
                                 0.50107879798490351*hypertension[ind_m],
                                 0.83858004034289935*fh_diab[ind_m]))
  
  # Interaction Terms #
  int[ind_m] <- Reduce("+", list(0.50510312537680635*age_1[ind_m]*bmi_1[ind_m],
                                 -0.1375233635462656*age_1[ind_m]*bmi_2[ind_m],
                                 -1.1463560542602569*age_1[ind_m]*fh_diab[ind_m],
                                 -0.00158006864527727*age_2[ind_m]*bmi_1[ind_m],
                                 0.00033940900578240623*age_2[ind_m]*bmi_2[ind_m],
                                 0.001852416035398126*age_2[ind_m]*fh_diab[ind_m]))
  
  ## Risk Score ##
  surv <- vec_surv
  ethnicity <- vec_eth
  smoking <- vec_smok
  score <- ethnicity + smoking + bmi + age + townsend + bin + int
  risk[ind_f] <- 100*(1 - surv[ind_f]^exp(score[ind_f]))
  risk[ind_m] <- 100*(1 - surv[ind_m]^exp(score[ind_m]))
  
  ## Named Output ##
  if(length(inputs_length[inputs_length == n]) == 1){
    names_out <- inputs[inputs_length == n][[1]]
    names(risk) <- names_out
  }
  
  ## Output ##
  return(risk)
}
