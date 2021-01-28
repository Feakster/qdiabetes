#================================#
#                                #
#### QDIABETES-2018 (MODEL B) ####
#                                #
#================================#

### License Information ###
# The QDR2018B function is part of the QDiabetes package, and is for
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
# - fpg >= 2 & fpg < 7
# - tds >= -8 & tds <= 14

### Function ###
QDR2018B <- function(sex, age,
                     bmi, ht, wt, fpg,
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
  if (any(missing(sex), missing(age), missing(fpg))) stop("sex, age & fpg must be specified")
  if (missing(bmi) & any(missing(ht), missing(wt))) stop("Either bmi or ht & wt must be specified")
  if (!missing(bmi) & any(!missing(ht), !missing(wt))) stop("Either bmi or ht & wt must be specified")
  stopifnot(all(sex %in% c("Female", "Male")))
  stopifnot(all(ethn %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")))
  stopifnot(all(smoke %in% c("Non", "Ex", "Light", "Moderate", "Heavy")))
  stopifnot(all(age >= 25 & age < 85))
  stopifnot(all(fpg >= 2 & fpg < 7))
  stopifnot(all(tds >= -8 & tds <= 14))
  if (any(sex == "Male" & (pcos | gdm))) stop("pcos and gdm must be set to FALSE for male sex")
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
  if (!missing(ht) & !missing(wt)) {
    stopifnot(all(ht >= 1.4 & ht <= 2.1))
    stopifnot(all(wt >= 40 & wt <= 180))
    bmi <- wt/ht^2
  }
  stopifnot(all(bmi >= 40/2.1^2 & bmi <= 180/1.4^2))
  if (any(bmi < 20)) {
    warning("bmi < 20. Setting bmi == 20", call. = FALSE)
    bmi[bmi < 20] <- 20
  }
  if (any(bmi > 40)) {
    warning("bmi > 40. Setting bmi == 40", call. = FALSE)
    bmi[bmi > 40] <- 40
  }
  
  ## Harmonize Input Lengths ##
  if (n != 1L) {
    sex <- rep_len(sex, n)
    age <- rep_len(age, n)
    bmi <- rep_len(bmi, n)
    fpg <- rep_len(fpg, n)
    ethn <- rep_len(ethn, n)
    smoke <- rep_len(smoke, n)
    tds <- rep_len(tds, n)
    fhdm <- rep_len(fhdm, n)
    htn <- rep_len(htn, n)
    cvd <- rep_len(cvd, n)
    gdm <- rep_len(gdm, n)
    pcos <- rep_len(pcos, n)
    learn <- rep_len(learn, n)
    psy <- rep_len(psy, n)
    ster <- rep_len(ster, n)
    stat <- rep_len(stat, n)
    apsy <- rep_len(apsy, n)
  }
  
  ## Intermediate Vectors ##
  vec_eth <- rep(0, n)
  vec_smok <- rep(0, n)
  dage <- age/10
  age_1 <- rep(NA_real_, n)
  age_2 <- dage^3
  dbmi <- bmi/10
  bmi_1 <- rep(NA_real_, n)
  bmi_2 <- dbmi^3
  fpg_1 <- rep(NA_real_, n)
  fpg_2 <- rep(NA_real_, n)
  bin <- rep(NA_real_, n)
  int <- rep(NA_real_, n)
  risk <- rep(NA_real_, n)
  
  ## Gender Indices ##
  ind_f <- which(sex == "Female")
  ind_m <- which(sex == "Male")
  
  ## Female ##
  # Ethnicity #
  vec_eth[sex == "Female" & ethn == "Indian"] <- 0.9898906127239111
  vec_eth[sex == "Female" & ethn == "Pakistani"] <- 1.2511504196326508
  vec_eth[sex == "Female" & ethn == "Bangladeshi"] <- 1.493475756819612
  vec_eth[sex == "Female" & ethn == "OtherAsian"] <- 0.96738874345659664
  vec_eth[sex == "Female" & ethn == "BlackCaribbean"] <- 0.48446445195931781
  vec_eth[sex == "Female" & ethn == "BlackAfrican"] <- 0.47842149553601027
  vec_eth[sex == "Female" & ethn == "Chinese"] <- 0.75209462708055774
  vec_eth[sex == "Female" & ethn == "Other"] <- 0.40508807415414244
  
  # Smoking #
  vec_smok[sex == "Female" & smoke == "Ex"] <- 0.037415630723696323
  vec_smok[sex == "Female" & smoke == "Light"] <- 0.22529736725144828
  vec_smok[sex == "Female" & smoke == "Moderate"] <- 0.30997364280236628
  vec_smok[sex == "Female" & smoke == "Heavy"] <- 0.43619421394964175
  
  # Age #
  age_1[ind_f] <- dage[ind_f]^0.5
  age_1[ind_f] <- age_1[ind_f] - 2.123332023620606
  age_2[ind_f] <- age_2[ind_f] - 91.644744873046875
  age[ind_f] <- 3.765012950751728*age_1[ind_f] - 0.0056043343436614941*age_2[ind_f]
  
  # BMI #
  bmi_1[ind_f] <- dbmi[ind_f]
  bmi_1[ind_f] <- bmi_1[ind_f] - 2.571253299713135
  bmi_2[ind_f] <- bmi_2[ind_f] - 16.999439239501953
  bmi[ind_f] <- 2.4410935031672469*bmi_1[ind_f] - 0.042152633479909642*bmi_2[ind_f]
  
  # Townsend #
  tds[ind_f] <- tds[ind_f] - 0.391116052865982
  tds[ind_f] <- 0.03580462976631265*tds[ind_f]
  
  # FPG #
  fpg_1[ind_f] <- fpg[ind_f]^-1
  fpg_2[ind_f] <- log(fpg[ind_f])*fpg[ind_f]^-1
  fpg_1[ind_f] <- fpg_1[ind_f] - 0.208309367299080
  fpg_2[ind_f] <- fpg_2[ind_f] - 0.326781362295151
  fpg[ind_f] <- -2.1887891946337308*fpg_1[ind_f] - 69.960841982866029*fpg_2[ind_f]
  
  # Binary Variables #
  bin[ind_f] <- Reduce("+", list(0.47483785502538534*apsy[ind_f],
                                 0.37679334437547285*ster[ind_f],
                                 0.19672615680665251*cvd[ind_f],
                                 1.0689325033692647*gdm[ind_f],
                                 0.45422934089510347*learn[ind_f],
                                 0.16161718890842605*psy[ind_f],
                                 0.35653657895767171*pcos[ind_f],
                                 0.58092873827186675*stat[ind_f],
                                 0.28366320201229073*htn[ind_f],
                                 0.45221497662061116*fhdm[ind_f]))
  
  # Interaction Terms #
  int[ind_f] <- Reduce("+", list(-0.76835916427865225*age_1[ind_f]*apsy[ind_f],
                                 0.00051944556244134762*age_2[ind_f]*apsy[ind_f],
                                 -0.79831281242975882*age_1[ind_f]*learn[ind_f],
                                 0.00030283275671618906*age_2[ind_f]*learn[ind_f],
                                 -1.9033508839833257*age_1[ind_f]*stat[ind_f],
                                 0.0024397111406018711*age_2[ind_f]*stat[ind_f],
                                 0.48447476024049152*age_1[ind_f]*bmi_1[ind_f],
                                 -0.0041572976682154057*age_2[ind_f]*bmi_1[ind_f],
                                 -0.031939988307181345*age_1[ind_f]*bmi_2[ind_f],
                                 0.00011268821942042522*age_2[ind_f]*bmi_2[ind_f],
                                 2.244290304740435*age_1[ind_f]*fpg_1[ind_f],
                                 0.019934530853431255*age_2[ind_f]*fpg_1[ind_f],
                                 13.006838869978303*age_1[ind_f]*fpg_2[ind_f],
                                 -0.071667718752930668*age_2[ind_f]*fpg_2[ind_f],
                                 -0.30406273740345013*age_1[ind_f]*fhdm[ind_f],
                                 0.00045236396712023254*age_2[ind_f]*fhdm[ind_f]))
  
  ## Male ##
  # Ethnicity #
  vec_eth[sex == "Male" & ethn == "Indian"] <- 1.0081475800686235
  vec_eth[sex == "Male" & ethn == "Pakistani"] <- 1.3359138425778705
  vec_eth[sex == "Male" & ethn == "Bangladeshi"] <- 1.4815419524892652
  vec_eth[sex == "Male" & ethn == "OtherAsian"] <- 1.0384996851820663
  vec_eth[sex == "Male" & ethn == "BlackCaribbean"] <- 0.52023480708875247
  vec_eth[sex == "Male" & ethn == "BlackAfrican"] <- 0.85796734182585588
  vec_eth[sex == "Male" & ethn == "Chinese"] <- 0.64131089607656155
  vec_eth[sex == "Male" & ethn == "Other"] <- 0.48383402208215048
  
  # Smoking #
  vec_smok[sex == "Male" & smoke == "Ex"] <- 0.11194757923641625
  vec_smok[sex == "Male" & smoke == "Light"] <- 0.31101320954122047
  vec_smok[sex == "Male" & smoke == "Moderate"] <- 0.33288984693260421
  vec_smok[sex == "Male" & smoke == "Heavy"] <- 0.42570690269419931
  
  # Age #
  age_1[ind_m] <- log(dage[ind_m])
  age_1[ind_m] <- age_1[ind_m] - 1.496392488479614
  age_2[ind_m] <- age_2[ind_m] - 89.048171997070313
  age[ind_m] <- 4.1149143302364717*age_1[ind_m] - 0.0047593576668505362*age_2[ind_m]
  
  # BMI #
  bmi_1[ind_m] <- dbmi[ind_m]^2
  bmi_1[ind_m] <- bmi_1[ind_m] - 6.817805767059326
  bmi_2[ind_m] <- bmi_2[ind_m] - 17.801923751831055
  bmi[ind_m] <- 0.81693615876442971*bmi_1[ind_m] - 0.12502377403433362*bmi_2[ind_m]
  
  # Townsend #
  tds[ind_m] <- tds[ind_m] - 0.515986680984497
  tds[ind_m] <- 0.025374175519894356*tds[ind_m]
  
  # FPG #
  fpg_1[ind_m] <- fpg[ind_m]^-0.5
  fpg_2[ind_m] <- log(fpg[ind_m])*fpg[ind_m]^-0.5
  fpg_1[ind_m] <- fpg_1[ind_m] - 0.448028832674026
  fpg_2[ind_m] <- fpg_2[ind_m] - 0.719442605972290
  fpg[ind_m] <- -54.841788128097107*fpg_1[ind_m] - 53.11207849848136*fpg_2[ind_m]
  
  # Binary Variables #
  bin[ind_m] <- Reduce("+", list(0.44179340888895774*apsy[ind_m],
                                 0.34135473483394541*ster[ind_m],
                                 0.21589774543727566*cvd[ind_m],
                                 0.40128850275853001*learn[ind_m],
                                 0.21817693913997793*psy[ind_m],
                                 0.51476576001117347*stat[ind_m],
                                 0.24672092874070373*htn[ind_m],
                                 0.57494373339875127*fhdm[ind_m]))
  
  # Interaction Terms #
  int[ind_m] <- Reduce("+", list(-0.95022243138231266*age_1[ind_m]*apsy[ind_m],
                                 0.00014729720771628743*age_2[ind_m]*apsy[ind_m],
                                 -0.83583701630900453*age_1[ind_m]*learn[ind_m],
                                 0.00060129192649664091*age_2[ind_m]*learn[ind_m],
                                 -1.814178691926946*age_1[ind_m]*stat[ind_m],
                                 0.0016393484911405418*age_2[ind_m]*stat[ind_m],
                                 0.37484820920783846*age_1[ind_m]*bmi_1[ind_m],
                                 -0.0010774782221531713*age_2[ind_m]*bmi_1[ind_m],
                                 -0.090983657956248742*age_1[ind_m]*bmi_2[ind_m],
                                 0.00019110487304583101*age_2[ind_m]*bmi_2[ind_m],
                                 21.011730121764334*age_1[ind_m]*fpg_1[ind_m],
                                 -0.039004607922383527*age_2[ind_m]*fpg_1[ind_m],
                                 23.824460044746974*age_1[ind_m]*fpg_2[ind_m],
                                 -0.041127719805895947*age_2[ind_m]*fpg_2[ind_m],
                                 -0.67806477052916658*age_1[ind_m]*fhdm[ind_m],
                                 0.00062575882488594993*age_2[ind_m]*fhdm[ind_m]))
  
  ## Risk Score ##
  ethn <- vec_eth
  smoke <- vec_smok
  score <- ethn + smoke + bmi + age + tds + fpg + bin + int
  risk[ind_f] <- 100*(1 - 0.990905702114105^exp(score[ind_f]))
  risk[ind_m] <- 100*(1 - 0.985019445419312^exp(score[ind_m]))
  
  ## Named Output ##
  if (length(inputs_length[inputs_length == n]) == 1) {
    names_out <- inputs[inputs_length == n][[1]]
    if (is.null(names(names_out))) {
      names(risk) <- names_out
    } else {
      names(risk) <- names(names_out)
    }
  }
  
  ## Output ##
  return(risk)
}
