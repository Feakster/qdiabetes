#=============================================#
#                                             #
#### QDIABETES-2018 (MODEL C) [VECTORISED] ####
#                                             #
#=============================================#

### License Information ###
# The vQDRC function is part of the QDiabetes package, and is for
# calculating the 10-year risk of developing type-2 diabetes.
# Copyright (C) 2020  University of Oxford

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# The additional terms stated in the license of the source material
# mandate that the following disclaimer be included alongside the license
# notice (above):

# The initial version of this file, to be found at http://qdiabetes.org,
# faithfully implements QDiabetes-2018. ClinRisk Ltd. have released this
# code under the GNU Affero General Public License to enable others to
# implement the algorithm faithfully. However, the nature of the GNU Affero
# General Public License is such that we cannot prevent, for example, someone
# accidentally altering the coefficients, getting the inputs wrong, or just
# poor programming. ClinRisk Ltd. stress, therefore, that it is the
# responsibility of the end user to check that the source that they receive
# produces the same  results as the original code found at
# http://qdiabetes.org. Inaccurate implementations of risk scores can lead to
# wrong patients being given the wrong treatment.

### Notes ###
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - hba1c >= 15 & hba1c < 48
# - townsend >= -7.028634577 & townsend <= 13.3114711

vQDRC <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, hba1c = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE, gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE, pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE){
  ## Stop Conditions ##
  if(any(c(is.null(gender), is.null(age), is.null(hba1c)))) stop("gender, age & hba1c must be specified")
  if(is.null(bmi) & any(c(is.null(height), is.null(weight)))) stop("either bmi or height & weight must be specified")
  inputs <- list(gender, age, bmi, height, weight, hba1c, ethnicity, smoking, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab)
  inputs_length <- sapply(inputs, length)
  n <- max(inputs_length)
  stopifnot(all(inputs_length %in% c(0:1, n)))
  stopifnot(all(gender %in% c("Female", "Male")))
  stopifnot(all(ethnicity %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")))
  stopifnot(all(smoking %in% c("Non", "Ex", "Light", "Moderate", "Heavy")))
  stopifnot(all(age >= 25 & age < 85))
  stopifnot(all(height >= 1.4 & height <= 2.1))
  stopifnot(all(weight >= 40 & weight <= 180))
  stopifnot(all(hba1c >= 15 & hba1c < 48))
  stopifnot(all(townsend >= -7.028634577 & townsend <= 13.3114711))
  if(any(pcos[gender == "Male"], gestdiab[gender == "Male"])) stop("'pcos' and 'gestdiab' must be set to FALSE for male 'gender'")
  stopifnot(all(antipsy %in% c(FALSE, TRUE)))
  stopifnot(all(steroids %in% c(FALSE, TRUE)))
  stopifnot(all(cvd %in% c(FALSE, TRUE)))
  stopifnot(all(gestdiab %in% c(FALSE, TRUE)))
  stopifnot(all(learndiff %in% c(FALSE, TRUE)))
  stopifnot(all(schizobipo %in% c(FALSE, TRUE)))
  stopifnot(all(pcos %in% c(FALSE, TRUE)))
  stopifnot(all(statins %in% c(FALSE, TRUE)))
  stopifnot(all(hypertension %in% c(FALSE, TRUE)))
  stopifnot(all(fh_diab %in% c(FALSE, TRUE)))
  
  ## BMI Pre-Procession ##
  if(!is.null(bmi) & !is.null(height) & !is.null(weight)){
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
  if(length(hba1c) == 1) hba1c <- rep(hba1c, n)
  if(length(ethnicity) == 1) ethnicity <- rep(ethnicity, n)
  if(length(smoking) == 1) smoking <- rep(smoking, n)
  if(length(townsend) == 1) townsend <- rep(townsend, n)
  if(length(antipsy) == 1) antipsy <- rep(antipsy, n)
  if(length(steroids) == 1) steroids <- rep(steroids, n)
  if(length(cvd) == 1) cvd <- rep(cvd, n)
  if(length(gestdiab) == 1) gestdiab <- rep(gestdiab, n)
  if(length(learndiff) == 1) learndiff <- rep(learndiff, n)
  if(length(schizobipo) == 1) schizobipo <- rep(schizobipo, n)
  if(length(pcos) == 1) pcos <- rep(pcos, n)
  if(length(statins) == 1) statins <- rep(statins, n)
  if(length(hypertension) == 1) hypertension <- rep(hypertension, n)
  if(length(fh_diab) == 1) fh_diab <- rep(fh_diab, n)
  
  ## Intermediate Vectors ##
  vec_eth <- rep(0, n)
  vec_smok <- rep(0, n)
  dage <- age/10
  age_1 <- rep(NA_real_, n)
  age_2 <- rep(NA_real_, n)
  age_2 <- dage^3
  dbmi <- bmi/10
  bmi_1 <- rep(NA_real_, n)
  bmi_2 <- rep(NA_real_, n)
  bmi_2 <- dbmi^3
  dhba1c <- hba1c/10
  hba1c_1 <- dhba1c^0.5
  hba1c_2 <- dhba1c
  risk <- rep(NA_real_, n)
  int <- rep(NA_real_, n)
  
  ## Gender Indices ##
  ind_f <- which(gender == "Female")
  ind_m <- which(gender == "Male")
  
  ## Female ##
  # Ethnicity #
  vec_eth[gender == "Female" & ethnicity == "Indian"] <- 0.599095159929154
  vec_eth[gender == "Female" & ethnicity == "Pakistani"] <- 0.78320309656353893
  vec_eth[gender == "Female" & ethnicity == "Bangladeshi"] <- 1.1947351247960103
  vec_eth[gender == "Female" & ethnicity == "OtherAsian"] <- 0.71417446991681433
  vec_eth[gender == "Female" & ethnicity == "BlackCaribbean"] <- 0.11953284683887688
  vec_eth[gender == "Female" & ethnicity == "BlackAfrican"] <- 0.013668872878490427
  vec_eth[gender == "Female" & ethnicity == "Chinese"] <- 0.57092265376939455
  vec_eth[gender == "Female" & ethnicity == "Other"] <- 0.17091076281069292
  
  # Smoking #
  vec_smok[gender == "Female" & smoking == "Ex"] <- 0.065848258510000673
  vec_smok[gender == "Female" & smoking == "Light"] <- 0.1458413689734224
  vec_smok[gender == "Female" & smoking == "Moderate"] <- 0.15258642474801187
  vec_smok[gender == "Female" & smoking == "Heavy"] <- 0.30787416796613976
  
  # Age #
  age_1[ind_f] <- dage[ind_f]^0.5
  age_1[ind_f] <- age_1[ind_f] - 2.123332023620606
  age_2[ind_f] <- age_2[ind_f] - 91.644744873046875
  age[ind_f] <- 3.5655214891947722*age_1[ind_f] - 0.0056158243572733135*age_2[ind_f]
  
  # BMI #
  bmi_1[ind_f] <- dbmi[ind_f]
  bmi_1[ind_f] <- bmi_1[ind_f] - 2.571253299713135
  bmi_2[ind_f] <- bmi_2[ind_f] - 16.999439239501953
  bmi[ind_f] <- 2.5043028874544841*bmi_1[ind_f] - 0.042875801892690461*bmi_2[ind_f]
  
  # Townsend #
  townsend[ind_f] <- townsend[ind_f] - 0.391116052865982
  townsend[ind_f] <- 0.035866822056348294*townsend[ind_f]
  
  # HbA1c #
  hba1c_1[ind_f] <- hba1c_1[ind_f] - 1.886751174926758
  hba1c_2[ind_f] <- hba1c_2[ind_f] - 3.559829950332642
  hba1c[ind_f] <- 8.7368031307362184*hba1c_1[ind_f] - 0.07823138666994997*hba1c_2[ind_f]
  
  # Binary Variables #
  antipsy[ind_f] <- 0.54976333110422004*antipsy[ind_f]
  steroids[ind_f] <- 0.16872205506389704*steroids[ind_f]
  cvd[ind_f] <- 0.16443300362739344*cvd[ind_f]
  gestdiab[ind_f] <-1.125009810517114*gestdiab[ind_f]
  learndiff[ind_f] <- 0.28912058310739658*learndiff[ind_f]
  schizobipo[ind_f] <- 0.31825122490684077*schizobipo[ind_f]
  pcos[ind_f] <- 0.33806444140981745*pcos[ind_f]
  statins[ind_f] <- 0.45593968473811164*statins[ind_f]
  hypertension[ind_f] <- 0.4040022295023758*hypertension[ind_f]
  fh_diab[ind_f] <- 0.44280154048260317*fh_diab[ind_f]
  
  # Interaction Terms #
  int[ind_f] <- Reduce("+", list(-0.81254341971621313*age_1[ind_f]*antipsy[ind_f],
                                 0.0004665611306005428*age_2[ind_f]*antipsy[ind_f],
                                 -0.90846657652698082*age_1[ind_f]*learndiff[ind_f],
                                 0.00085189801399280065*age_2[ind_f]*learndiff[ind_f],
                                 -1.8557960585560658*age_1[ind_f]*statins[ind_f],
                                 0.0022627250963352537*age_2[ind_f]*statins[ind_f],
                                 0.6023218765235252*age_1[ind_f]*bmi_1[ind_f],
                                 -0.0043386645663133425*age_2[ind_f]*bmi_1[ind_f],
                                 -0.03449503839680447*age_1[ind_f]*bmi_2[ind_f],
                                 0.00011627785616712089*age_2[ind_f]*bmi_2[ind_f],
                                 25.441203322736715*age_1[ind_f]*hba1c_1[ind_f],
                                 -0.052254135588592522*age_2[ind_f]*hba1c_1[ind_f],
                                 -6.8076080421556107*age_1[ind_f]*hba1c_2[ind_f],
                                 0.014054825906114453*age_2[ind_f]*hba1c_2[ind_f],
                                 -0.27275713515061872*age_1[ind_f]*fh_diab[ind_f],
                                 0.00043545197952207749*age_2[ind_f]*fh_diab[ind_f]))
  
  ## Male ##
  # Ethnicity #
  vec_eth[gender == "Male" & ethnicity == "Indian"] <- 0.67571207054987803
  vec_eth[gender == "Male" & ethnicity == "Pakistani"] <- 0.83147325049663456
  vec_eth[gender == "Male" & ethnicity == "Bangladeshi"] <- 1.0969133802228563
  vec_eth[gender == "Male" & ethnicity == "OtherAsian"] <- 0.76822446364560482
  vec_eth[gender == "Male" & ethnicity == "BlackCaribbean"] <- 0.20897529259108502
  vec_eth[gender == "Male" & ethnicity == "BlackAfrican"] <- 0.38091593781970579
  vec_eth[gender == "Male" & ethnicity == "Chinese"] <- 0.34235836796612695
  vec_eth[gender == "Male" & ethnicity == "Other"] <- 0.22046477853433083
  
  # Smoking #
  vec_smok[gender == "Male" & smoking == "Ex"] <- 0.11592891206878651
  vec_smok[gender == "Male" & smoking == "Light"] <- 0.14624182637633271
  vec_smok[gender == "Male" & smoking == "Moderate"] <- 0.10781424112493142
  vec_smok[gender == "Male" & smoking == "Heavy"] <- 0.19848629163668474
  
  # Age #
  age_1[ind_m] <- log(dage[ind_m])
  age_1[ind_m] <- age_1[ind_m] - 1.496392488479614
  age_2[ind_m] <- age_2[ind_m] - 89.048171997070313
  age[ind_m] <- 4.0193435623978031*age_1[ind_m] - 0.0048396442306278238*age_2[ind_m]
  
  # BMI #
  bmi_1[ind_m] <- dbmi[ind_m]^2
  bmi_1[ind_m] <- bmi_1[ind_m] - 6.817805767059326
  bmi_2[ind_m] <- bmi_2[ind_m] - 17.801923751831055
  bmi[ind_m] <- 0.81829168905349325*bmi_1[ind_m] - 0.12558808701359642*bmi_2[ind_m]
  
  # Townsend #
  townsend[ind_m] <- townsend[ind_m] - 0.515986680984497
  townsend[ind_m] <- 0.025229965184900727*townsend[ind_m]
  
  # HbA1c #
  hba1c_1[ind_m] <- hba1c_1[ind_m] - 1.900265336036682
  hba1c_2[ind_m] <- hba1c_2[ind_m] - 3.611008167266846
  hba1c[ind_m] <- 8.0511642238857934*hba1c_1[ind_m] - 0.14652346893914495*hba1c_2[ind_m]
  
  # Binary Variables #
  antipsy[ind_m] <- 0.45541525220173301*antipsy[ind_m]
  steroids[ind_m] <- 0.13816187686823922*steroids[ind_m]
  cvd[ind_m] <- 0.14546988896239518*cvd[ind_m]
  learndiff[ind_m] <- 0.2596046658040857*learndiff[ind_m]
  schizobipo[ind_m] <- 0.28523788490585894*schizobipo[ind_m]
  statins[ind_m] <- 0.42551951901185525*statins[ind_m]
  hypertension[ind_m] <- 0.33169430006459311*hypertension[ind_m]
  fh_diab[ind_m] <- 0.56612325943680619*fh_diab[ind_m]
  
  # Interaction Terms #
  int[ind_m] <- Reduce("+", list(-1.0013331909079835*age_1[ind_m]*antipsy[ind_m],
                                 0.00022455973985742407*age_2[ind_m]*antipsy[ind_m],
                                 -0.89164657372215927*age_1[ind_m]*learndiff[ind_m],
                                 0.00066044360765696482*age_2[ind_m]*learndiff[ind_m],
                                 -1.7074561167819817*age_1[ind_m]*statins[ind_m],
                                 0.0013873509357389619*age_2[ind_m]*statins[ind_m],
                                 0.45074527472672443*age_1[ind_m]*bmi_1[ind_m],
                                 -0.0012224736160287865*age_2[ind_m]*bmi_1[ind_m],
                                 -0.10851859809165601*age_1[ind_m]*bmi_2[ind_m],
                                 0.0002266731010346126*age_2[ind_m]*bmi_2[ind_m],
                                 27.670593827146565*age_1[ind_m]*hba1c_1[ind_m],
                                 -0.05920145812475433*age_2[ind_m]*hba1c_1[ind_m],
                                 -7.4006134846785434*age_1[ind_m]*hba1c_2[ind_m],
                                 0.015592089485149988*age_2[ind_m]*hba1c_2[ind_m],
                                 -0.61410093887097161*age_1[ind_m]*fh_diab[ind_m],
                                 0.00050602582894772091*age_2[ind_m]*fh_diab[ind_m]))
  
  ## Risk Score ##
  ethnicity <- vec_eth; rm(vec_eth)
  smoking <- vec_smok; rm(vec_smok)
  score <- ethnicity + smoking + bmi + age + townsend + antipsy + steroids + cvd + gestdiab + learndiff + schizobipo + pcos + statins + hypertension + fh_diab + hba1c + int
  risk[ind_f] <- 100*(1 - 0.988788545131683^exp(score[ind_f]))
  risk[ind_m] <- 100*(1 - 0.981181740760803^exp(score[ind_m]))
  
  ## Output ##
  return(risk)
}
