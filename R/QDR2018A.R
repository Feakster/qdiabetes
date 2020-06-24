#================================#
#                                #
#### QDIABETES-2018 (MODEL A) ####
#                                #
#================================#

### License Information ###
# The QDR2018A function is part of the QDiabetes package, and is for
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
# - ht >= 1.40 & ht <= 2.10
# - wt >= 40 & wt <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - tds >= -8 & tds <= 14

### Function ###
QDR2018A <- function(sex, age,
                     bmi, ht, wt,
                     ethn = "WhiteNA", smoke = "Non", tds = 0,
                     fhdm = FALSE,
                     htn = FALSE, cvd = FALSE, gdm = FALSE, pcos = FALSE,
                     learn = FALSE, psy = FALSE,
                     ster = FALSE, stat = FALSE, apsy = FALSE){
  ## Stop Conditions ##
  inputs <- as.list(sys.frame(sys.nframe()))
  inputs_length <- lengths(inputs)
  n <- max(inputs_length)
  stopifnot(all(inputs_length %in% c(1, n)))
  if(any(missing(sex), missing(age))) stop("sex & age must be specified")
  if(missing(bmi) & any(missing(ht), missing(wt))) stop("Either bmi or ht & wt must be specified")
  if(!missing(bmi) & any(!missing(ht), !missing(wt))) stop("Either bmi or ht & wt must be specified")
  stopifnot(all(sex %in% c("Female", "Male")))
  stopifnot(all(ethn %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")))
  stopifnot(all(smoke %in% c("Non", "Ex", "Light", "Moderate", "Heavy")))
  stopifnot(all(age >= 25 & age < 85))
  stopifnot(all(tds >= -8 & tds <= 14))
  if(any(sex == "Male" & (pcos | gdm))) stop("pcos and gdm must be set to FALSE for male sex")
  stopifnot(all(fhdm %in% c(FALSE, TRUE)))
  stopifnot(all(htn %in% c(FALSE, TRUE)))
  stopifnot(all(cvd %in% c(FALSE, TRUE)))
  stopifnot(all(gdm %in% c(FALSE, TRUE)))
  stopifnot(all(pcos %in% c(FALSE, TRUE)))
  stopifnot(all(learn %in% c(FALSE, TRUE)))
  stopifnot(all(psy %in% c(FALSE, TRUE)))
  stopifnot(all(ster %in% c(FALSE, TRUE)))
  stopifnot(all(stat %in% c(FALSE, TRUE)))
  stopifnot(all(apsy %in% c(FALSE, TRUE)))
  
  ## BMI Pre-Processing ##
  if(!missing(ht) & !missing(wt)){
    stopifnot(all(ht >= 1.4 & ht <= 2.1))
    stopifnot(all(wt >= 40 & wt <= 180))
    bmi <- wt/ht^2
  }
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
  if(length(sex) == 1) sex <- rep(sex, n)
  if(length(age) == 1) age <- rep(age, n)
  if(length(bmi) == 1) bmi <- rep(bmi, n)
  if(length(ethn) == 1) ethn <- rep(ethn, n)
  if(length(smoke) == 1) smoke <- rep(smoke, n)
  if(length(tds) == 1) tds <- rep(tds, n)
  if(length(fhdm) == 1) fhdm <- rep(fhdm, n)
  if(length(htn) == 1) htn <- rep(htn, n)
  if(length(cvd) == 1) cvd <- rep(cvd, n)
  if(length(gdm) == 1) gdm <- rep(gdm, n)
  if(length(pcos) == 1) pcos <- rep(pcos, n)
  if(length(learn) == 1) learn <- rep(learn, n)
  if(length(psy) == 1) psy <- rep(psy, n)
  if(length(ster) == 1) ster <- rep(ster, n)
  if(length(stat) == 1) stat <- rep(stat, n)
  if(length(apsy) == 1) apsy <- rep(apsy, n)
  
  ## Intermediate Vectors ##
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
  ind_f <- which(sex == "Female")
  ind_m <- which(sex == "Male")
  
  ## Female ##
  # Ethnicity #
  vec_eth[sex == "Female" & ethn == "Indian"] <- 1.0695857881565456
  vec_eth[sex == "Female" & ethn == "Pakistani"] <- 1.3430172097414006
  vec_eth[sex == "Female" & ethn == "Bangladeshi"] <- 1.8029022579794518
  vec_eth[sex == "Female" & ethn == "OtherAsian"] <- 1.127465451770802
  vec_eth[sex == "Female" & ethn == "BlackCaribbean"] <- 0.42146314902399101
  vec_eth[sex == "Female" & ethn == "BlackAfrican"] <- 0.2850919645908353
  vec_eth[sex == "Female" & ethn == "Chinese"] <- 0.88151087975891995
  vec_eth[sex == "Female" & ethn == "Other"] <- 0.36605733431684873
  
  # Smoking #
  vec_smok[sex == "Female" & smoke == "Ex"] <- 0.065601690175059055
  vec_smok[sex == "Female" & smoke == "Light"] <- 0.28450988673698374
  vec_smok[sex == "Female" & smoke == "Moderate"] <- 0.3567664381700702
  vec_smok[sex == "Female" & smoke == "Heavy"] <- 0.53595171106787753
  
  # Age #
  age_1[ind_f] <- dage[ind_f]^0.5
  age_1[ind_f] <- age_1[ind_f] - 2.123332023620606
  age_2[ind_f] <- age_2[ind_f] - 91.644744873046875
  age[ind_f] <- 4.3400852699139278*age_1[ind_f] - 0.0048771702696158879*age_2[ind_f]
  
  # BMI #
  bmi_1[ind_f] <- dbmi[ind_f]
  bmi_1[ind_f] <- bmi_1[ind_f] - 2.571253299713135
  bmi_2[ind_f] <- bmi_2[ind_f] - 16.999439239501953
  bmi[ind_f] <- 2.9320361259524925*bmi_1[ind_f] - 0.04740020587484349*bmi_2[ind_f]
  
  # Townsend #
  tds[ind_f] <- tds[ind_f] - 0.391116052865982
  tds[ind_f] <- 0.037340569618049151*tds[ind_f]
  
  # Binary Variables #
  bin[ind_f] <- Reduce("+", list(0.55267646110984381*apsy[ind_f],
                                 0.26792233680674599*ster[ind_f],
                                 0.17797229054586691*cvd[ind_f],
                                 1.5248871531467574*gdm[ind_f],
                                 0.27835143587172717*learn[ind_f],
                                 0.26180852109179059*psy[ind_f],
                                 0.34061739882066661*pcos[ind_f],
                                 0.65907287732808217*stat[ind_f],
                                 0.43947582858137119*htn[ind_f],
                                 0.53133594565587339*fhdm[ind_f]))
  
  # Interaction Terms #
  int[ind_f] <- Reduce("+", list(-0.80315183983163951*age_1[ind_f]*apsy[ind_f],
                                 0.00046840411810210498*age_2[ind_f]*apsy[ind_f],
                                 -0.86415960028820571*age_1[ind_f]*learn[ind_f],
                                 0.00067249688089533602*age_2[ind_f]*learn[ind_f],
                                 -1.9757776696583935*age_1[ind_f]*stat[ind_f],
                                 0.0023750534194347966*age_2[ind_f]*stat[ind_f],
                                 0.65531387575629452*age_1[ind_f]*bmi_1[ind_f],
                                 -0.0044719662445263054*age_2[ind_f]*bmi_1[ind_f],
                                 -0.036209657201630177*age_1[ind_f]*bmi_2[ind_f],
                                 0.0001185479967753342*age_2[ind_f]*bmi_2[ind_f],
                                 -0.26411714505588962*age_1[ind_f]*fhdm[ind_f],
                                 0.00041610258289047683*age_2[ind_f]*fhdm[ind_f]))
  
  ## Male ##
  # Ethnicity #
  vec_eth[sex == "Male" & ethn == "Indian"] <- 1.1000230829124793
  vec_eth[sex == "Male" & ethn == "Pakistani"] <- 1.290384012614721
  vec_eth[sex == "Male" & ethn == "Bangladeshi"] <- 1.6740908848727458
  vec_eth[sex == "Male" & ethn == "OtherAsian"] <- 1.1400446789147816
  vec_eth[sex == "Male" & ethn == "BlackCaribbean"] <- 0.46824681690655806
  vec_eth[sex == "Male" & ethn == "BlackAfrican"] <- 0.69905649963015448
  vec_eth[sex == "Male" & ethn == "Chinese"] <- 0.68943657127111568
  vec_eth[sex == "Male" & ethn == "Other"] <- 0.41722228467738209
  
  # Smoking #
  vec_smok[sex == "Male" & smoke == "Ex"] <- 0.16387409105485573
  vec_smok[sex == "Male" & smoke == "Light"] <- 0.31851449113958979
  vec_smok[sex == "Male" & smoke == "Moderate"] <- 0.32207266567783432
  vec_smok[sex == "Male" & smoke == "Heavy"] <- 0.45052437163409531
  
  # Age #
  age_1[ind_m] <- log(dage[ind_m])
  age_1[ind_m] <- age_1[ind_m] - 1.496392488479614
  age_2[ind_m] <- age_2[ind_m] - 89.048171997070313
  age[ind_m] <- 4.4642324388691348*age_1[ind_m] - 0.0040750108019255568*age_2[ind_m]
  
  # BMI #
  bmi_1[ind_m] <- dbmi[ind_m]^2
  bmi_1[ind_m] <- bmi_1[ind_m] - 6.817805767059326
  bmi_2[ind_m] <- bmi_2[ind_m] - 17.801923751831055
  bmi[ind_m] <- 0.95129027867120675*bmi_1[ind_m] - 0.14352488277885475*bmi_2[ind_m]
  
  # Townsend #
  tds[ind_m] <- tds[ind_m] - 0.515986680984497
  tds[ind_m] <- 0.025918182067678725*tds[ind_m]
  
  # Binary Variables #
  bin[ind_m] <- Reduce("+", list(0.42101092346005436*apsy[ind_m],
                                 0.22183580932925384*ster[ind_m],
                                 0.20269605756290021*cvd[ind_m],
                                 0.23315321407986961*learn[ind_m],
                                 0.22770449520517727*psy[ind_m],
                                 0.58490075431141342*stat[ind_m],
                                 0.33379392183501078*htn[ind_m],
                                 0.64799284899369536*fhdm[ind_m]))
  
  # Interaction Terms #
  int[ind_m] <- Reduce("+", list(-0.94637722268534152*age_1[ind_m]*apsy[ind_m],
                                 -0.0000519927442172335*age_2[ind_m]*apsy[ind_m],
                                 -0.93842375526499833*age_1[ind_m]*learn[ind_m],
                                 0.00071026438559688141*age_2[ind_m]*learn[ind_m],
                                 -1.7479070653003299*age_1[ind_m]*stat[ind_m],
                                 0.0013508364599531669*age_2[ind_m]*stat[ind_m],
                                 0.45147599241879766*age_1[ind_m]*bmi_1[ind_m],
                                 -0.0011797722394560309*age_2[ind_m]*bmi_1[ind_m],
                                 -0.10795481262776381*age_1[ind_m]*bmi_2[ind_m],
                                 0.00021471509139319291*age_2[ind_m]*bmi_2[ind_m],
                                 -0.60118530429301198*age_1[ind_m]*fhdm[ind_m],
                                 0.00049141855940878034*age_2[ind_m]*fhdm[ind_m]))
  
  ## Risk Score ##
  ethn <- vec_eth
  smoke <- vec_smok
  score <- ethn + smoke + bmi + age + tds + bin + int
  risk[ind_f] <- 100*(1 - 0.986227273941040^exp(score[ind_f]))
  risk[ind_m] <- 100*(1 - 0.978732228279114^exp(score[ind_m]))
  
  ## Named Output ##
  if(length(inputs_length[inputs_length == n]) == 1){
    names_out <- inputs[inputs_length == n][[1]]
    if(is.null(names(names_out))){
      names(risk) <- names_out
    } else {
      names(risk) <- names(names_out)
    }
  }
  
  ## Output ##
  return(risk)
}
