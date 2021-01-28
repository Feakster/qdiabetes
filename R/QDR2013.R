#======================#
#                      #
#### QDIABETES-2013 ####
#                      #
#======================#

### License Information ###
# The QDR2013 function is part of the QDiabetes package, and is for
# calculating the risk of developing type-2 diabetes.
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
# - ht >= 1.40 & ht <= 2.10
# - wt >= 40 & wt <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - tds >= -7 & tds <= 11
# - surv >= 1 & surv <= 10

### Function ###
QDR2013 <- function(sex, age,
                    bmi, ht, wt,
                    ethn = "WhiteNA", smoke = "Non", tds = 0,
                    fhdm = FALSE,
                    htn = FALSE, cvd = FALSE,
                    ster = FALSE,
                    surv = 10L){
  ## Stop Conditions ##
  inputs <- as.list(sys.frame(sys.nframe()))
  inputs_length <- lengths(inputs)
  n <- max(inputs_length)
  stopifnot(all(inputs_length %in% c(1, n)))
  if (any(missing(sex), missing(age))) stop("sex & age must be specified")
  if (missing(bmi) & any(missing(ht), missing(wt))) stop("Either bmi or ht & wt must be specified")
  if (!missing(bmi) & any(!missing(ht), !missing(wt))) stop("Either bmi or ht & wt must be specified")
  stopifnot(all(sex %in% c("Female", "Male")))
  stopifnot(all(ethn %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other")))
  stopifnot(all(smoke %in% c("Non", "Ex", "Light", "Moderate", "Heavy")))
  stopifnot(all(age >= 25 & age < 85))
  stopifnot(all(tds >= -7 & tds <= 11))
  stopifnot(all(fhdm %in% c(FALSE, TRUE)))
  stopifnot(all(htn %in% c(FALSE, TRUE)))
  stopifnot(all(cvd %in% c(FALSE, TRUE)))
  stopifnot(all(ster %in% c(FALSE, TRUE)))
  stopifnot(all(!is.logical(surv)))
  stopifnot(all(surv %in% 1:10))
  
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
    ethn <- rep_len(ethn, n)
    smoke <- rep_len(smoke, n)
    tds <- rep_len(tds, n)
    fhdm <- rep_len(fhdm, n)
    htn <- rep_len(htn, n)
    cvd <- rep_len(cvd, n)
    ster <- rep_len(ster, n)
    surv <- rep_len(surv, n)
  }
  
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
  ind_f <- which(sex == "Female")
  ind_m <- which(sex == "Male")
  
  ## Female ##
  # Survivor Function #
  vec_surv[sex == "Female" & surv == 1] <- 0.998714804649353
  vec_surv[sex == "Female" & surv == 2] <- 0.997435748577118
  vec_surv[sex == "Female" & surv == 3] <- 0.996052920818329
  vec_surv[sex == "Female" & surv == 4] <- 0.99456250667572
  vec_surv[sex == "Female" & surv == 5] <- 0.992949724197388
  vec_surv[sex == "Female" & surv == 6] <- 0.991141080856323
  vec_surv[sex == "Female" & surv == 7] <- 0.989293158054352
  vec_surv[sex == "Female" & surv == 8] <- 0.987293541431427
  vec_surv[sex == "Female" & surv == 9] <- 0.98513388633728
  vec_surv[sex == "Female" & surv == 10] <- 0.982810735702515
  # vec_surv[sex == "Female" & surv == 11] <- 0.980465650558472
  # vec_surv[sex == "Female" & surv == 12] <- 0.978020071983337
  # vec_surv[sex == "Female" & surv == 13] <- 0.97549307346344
  # vec_surv[sex == "Female" & surv == 14] <- 0.972945988178253
  # vec_surv[sex == "Female" & surv == 15] <- 0.97035014629364
  
  # Ethnicity #
  vec_eth[sex == "Female" & ethn == "Indian"] <- 1.2672136244963337
  vec_eth[sex == "Female" & ethn == "Pakistani"] <- 1.4277605208830098
  vec_eth[sex == "Female" & ethn == "Bangladeshi"] <- 1.8624060798103199
  vec_eth[sex == "Female" & ethn == "OtherAsian"] <- 1.2379988338989651
  vec_eth[sex == "Female" & ethn == "BlackCaribbean"] <- 0.47090341729076779
  vec_eth[sex == "Female" & ethn == "BlackAfrican"] <- 0.34764009017031605
  vec_eth[sex == "Female" & ethn == "Chinese"] <- 1.1587283467731935
  vec_eth[sex == "Female" & ethn == "Other"] <- 0.73354993250103151
  
  # Smoking #
  vec_smok[sex == "Female" & smoke == "Ex"] <- 0.10125370249475051
  vec_smok[sex == "Female" & smoke == "Light"] <- 0.19155205643806134
  vec_smok[sex == "Female" & smoke == "Moderate"] <- 0.30918941361433339
  vec_smok[sex == "Female" & smoke == "Heavy"] <- 0.46467303926938208
  
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
  tds[ind_f] <- tds[ind_f] + 0.224075347185135
  tds[ind_f] <- 0.043272699299863597*tds[ind_f]
  
  # Binary Variables #
  bin[ind_f] <- Reduce("+", list(0.2681990966241487*ster[ind_f],
                                 0.35961768309842529*cvd[ind_f],
                                 0.53145984369747257*htn[ind_f],
                                 0.73153588458376406*fhdm[ind_f]))
  
  # Interaction Terms #
  int[ind_f] <- Reduce("+", list(1.303783287399799*age_1[ind_f]*bmi_1[ind_f],
                                 -0.070829371776904612*age_1[ind_f]*bmi_2[ind_f],
                                 -0.79682668158342518*age_1[ind_f]*fhdm[ind_f],
                                 -0.0067725323761278549*age_2[ind_f]*bmi_1[ind_f],
                                 0.00023749807286661167*age_2[ind_f]*bmi_2[ind_f],
                                 0.0017048228889394394*age_2[ind_f]*fhdm[ind_f]))
  
  ## Male ##
  # Survivor Function #
  vec_surv[sex == "Male" & surv == 1] <- 0.998213708400726
  vec_surv[sex == "Male" & surv == 2] <- 0.996353209018707
  vec_surv[sex == "Male" & surv == 3] <- 0.994382798671722
  vec_surv[sex == "Male" & surv == 4] <- 0.992213606834412
  vec_surv[sex == "Male" & surv == 5] <- 0.989733397960663
  vec_surv[sex == "Male" & surv == 6] <- 0.9870645403862
  vec_surv[sex == "Male" & surv == 7] <- 0.984254062175751
  vec_surv[sex == "Male" & surv == 8] <- 0.981255292892456
  vec_surv[sex == "Male" & surv == 9] <- 0.977990627288818
  vec_surv[sex == "Male" & surv == 10] <- 0.974455237388611
  # vec_surv[sex == "Male" & surv == 11] <- 0.970843732357025
  # vec_surv[sex == "Male" & surv == 12] <- 0.967315018177032
  # vec_surv[sex == "Male" & surv == 13] <- 0.963437378406525
  # vec_surv[sex == "Male" & surv == 14] <- 0.959633111953735
  # vec_surv[sex == "Male" & surv == 15] <- 0.955690681934357
  
  # Ethnicity #
  vec_eth[sex == "Male" & ethn == "Indian"] <- 1.2366090720913343
  vec_eth[sex == "Male" & ethn == "Pakistani"] <- 1.4716746107789032
  vec_eth[sex == "Male" & ethn == "Bangladeshi"] <- 1.8073235649498174
  vec_eth[sex == "Male" & ethn == "OtherAsian"] <- 1.2056055595936399
  vec_eth[sex == "Male" & ethn == "BlackCaribbean"] <- 0.60323699759387661
  vec_eth[sex == "Male" & ethn == "BlackAfrican"] <- 0.90954362074527373
  vec_eth[sex == "Male" & ethn == "Chinese"] <- 0.91376046329275129
  vec_eth[sex == "Male" & ethn == "Other"] <- 0.71237190459907795
  
  # Smoking #
  vec_smok[sex == "Male" & smoke == "Ex"] <- 0.16182385823959777
  vec_smok[sex == "Male" & smoke == "Light"] <- 0.1902020385619117
  vec_smok[sex == "Male" & smoke == "Moderate"] <- 0.32106361793124671
  vec_smok[sex == "Male" & smoke == "Heavy"] <- 0.41400013017974946
  
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
  tds[ind_m] <- tds[ind_m] + 0.132148191332817
  tds[ind_m] <- 0.029153081590382265*tds[ind_m]
  
  # Binary Variables #
  bin[ind_m] <- Reduce("+", list(0.20598119799056924*ster[ind_m],
                                 0.39147284549905031*cvd[ind_m],
                                 0.50107879798490351*htn[ind_m],
                                 0.83858004034289935*fhdm[ind_m]))
  
  # Interaction Terms #
  int[ind_m] <- Reduce("+", list(0.50510312537680635*age_1[ind_m]*bmi_1[ind_m],
                                 -0.1375233635462656*age_1[ind_m]*bmi_2[ind_m],
                                 -1.1463560542602569*age_1[ind_m]*fhdm[ind_m],
                                 -0.00158006864527727*age_2[ind_m]*bmi_1[ind_m],
                                 0.00033940900578240623*age_2[ind_m]*bmi_2[ind_m],
                                 0.001852416035398126*age_2[ind_m]*fhdm[ind_m]))
  
  ## Risk Score ##
  surv <- vec_surv
  ethn <- vec_eth
  smoke <- vec_smok
  score <- ethn + smoke + bmi + age + tds + bin + int
  risk[ind_f] <- 100*(1 - surv[ind_f]^exp(score[ind_f]))
  risk[ind_m] <- 100*(1 - surv[ind_m]^exp(score[ind_m]))
  
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
