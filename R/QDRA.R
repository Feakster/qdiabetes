#================================#
#                                #
#### QDIABETES-2018 (MODEL A) ####
#                                #
#================================#

### Notes ###
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - townsend >= -7.028634577 & townsend <= 13.3114711

QDRA <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE, gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE, pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE){
  ## Stop Conditions ##
  if(any(c(is.null(gender), is.null(age)))) stop("gender & age must be specified")
  if(is.null(bmi) & any(c(is.null(height), is.null(weight)))) stop("either bmi or height & weight must be specified")
  stopifnot(gender %in% c("Male", "Female"))
  stopifnot(ethnicity %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other"))
  stopifnot(smoking %in% c("Non", "Ex", "Light", "Moderate", "Heavy"))
  stopifnot(age >= 25 & age < 85)
  stopifnot(height >= 1.40 & height <= 2.10)
  stopifnot(weight >= 40 & weight <= 180)
  stopifnot(townsend >= -7.028634577 & townsend <= 13.3114711)

  ## BMI Pre-Processing ##
  if(all(c(!is.null(bmi), !is.null(height), !is.null(weight)))) warning("bmi, height & weight all specified, height & weight ignored", call. = FALSE)
  if(is.null(bmi)) bmi <- weight/height^2
  stopifnot(bmi >= 40/2.10^2 & bmi <= 180/1.4^2)
  if(bmi < 20){
    warning("bmi < 20. Setting bmi == 20", call. = FALSE)
    bmi <- 20
  }
  if(bmi > 40){
    warning("bmi > 40. Setting bmi == 40", call. = FALSE)
    bmi <- 40
  }

  ## Female ##
  if(gender == "Female"){
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 1.0695857881565456000000000,
      Pakistani = 1.3430172097414006000000000,
      Bangladeshi = 1.8029022579794518000000000,
      OtherAsian = 1.1274654517708020000000000,
      BlackCaribbean = 0.4214631490239910100000000,
      BlackAfrican = 0.2850919645908353000000000,
      Chinese = 0.8815108797589199500000000,
      Other = 0.3660573343168487300000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.0656016901750590550000000,
      Light = 0.2845098867369837400000000,
      Moderate = 0.3567664381700702000000000,
      Heavy = 0.5359517110678775300000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- dage^0.5
    age_2 <- dage^3
    age_1 <- age_1 - 2.123332023620606
    age_2 <- age_2 - 91.644744873046875
    age <- 4.3400852699139278000000000*age_1 - 0.0048771702696158879000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 2.571253299713135
    bmi_2 <- bmi_2 - 16.999439239501953
    bmi <- 2.9320361259524925000000000*bmi_1 - 0.0474002058748434900000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.391116052865982
    townsend <- 0.0373405696180491510000000*townsend

    # Binary Variables #
    antipsy <- 0.5526764611098438100000000*antipsy
    steroids <- 0.2679223368067459900000000*steroids
    cvd <- 0.1779722905458669100000000*cvd
    gestdiab <- 1.5248871531467574000000000*gestdiab
    learndiff <- 0.2783514358717271700000000*learndiff
    schizobipo <- 0.2618085210917905900000000*schizobipo
    pcos <- 0.3406173988206666100000000*pcos
    statins <- 0.6590728773280821700000000*statins
    hypertension <- 0.4394758285813711900000000*hypertension
    fh_diab <- 0.5313359456558733900000000*fh_diab

    # Interaction Terms #
    int <- sum(-0.8031518398316395100000000*age_1*antipsy,
               -0.8641596002882057100000000*age_1*learndiff,
               -1.9757776696583935000000000*age_1*statins,
               0.6553138757562945200000000*age_1*bmi_1,
               -0.0362096572016301770000000*age_1*bmi_2,
               -0.2641171450558896200000000*age_1*fh_diab,
               0.0004684041181021049800000*age_2*antipsy,
               0.0006724968808953360200000*age_2*learndiff,
               0.0023750534194347966000000*age_2*statins,
               -0.0044719662445263054000000*age_2*bmi_1,
               0.0001185479967753342000000*age_2*bmi_2,
               0.0004161025828904768300000*age_2*fh_diab)

    # Risk Score #
    score <- sum(ethnicity, smoking, bmi, age, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab, int)
    risk <- 100*(1 - 0.986227273941040^exp(score))
  }

  ## Male ##
  if(gender == "Male"){
    if(any(pcos, gestdiab)) stop("'pcos' and 'gestdiab' must be set to FALSE for male 'gender'")
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 1.1000230829124793000000000,
      Pakistani = 1.2903840126147210000000000,
      Bangladeshi = 1.6740908848727458000000000,
      OtherAsian = 1.1400446789147816000000000,
      BlackCaribbean = 0.4682468169065580600000000,
      BlackAfrican = 0.6990564996301544800000000,
      Chinese = 0.6894365712711156800000000,
      Other = 0.4172222846773820900000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.1638740910548557300000000,
      Light = 0.3185144911395897900000000,
      Moderate = 0.3220726656778343200000000,
      Heavy = 0.4505243716340953100000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- log(dage)
    age_2 <- dage^3
    age_1 <- age_1 - 1.496392488479614
    age_2 <- age_2 - 89.048171997070313
    age <- 4.4642324388691348000000000*age_1 - 0.0040750108019255568000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi^2
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 6.817805767059326
    bmi_2 <- bmi_2 - 17.801923751831055
    bmi <- 0.9512902786712067500000000*bmi_1 - 0.1435248827788547500000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.515986680984497
    townsend <- 0.0259181820676787250000000*townsend

    # Binary Variables #
    antipsy <- 0.4210109234600543600000000*antipsy # HERE!
    steroids <- 0.2218358093292538400000000*steroids
    cvd <- 0.2026960575629002100000000*cvd
    learndiff <- 0.2331532140798696100000000*learndiff
    schizobipo <- 0.2277044952051772700000000*schizobipo
    statins <- 0.5849007543114134200000000*statins
    hypertension <- 0.3337939218350107800000000*hypertension
    fh_diab <- 0.6479928489936953600000000*fh_diab

    # Interaction Terms #
    int <- sum(-0.9463772226853415200000000*age_1*antipsy,
               -0.9384237552649983300000000*age_1*learndiff,
               -1.7479070653003299000000000*age_1*statins,
               0.4514759924187976600000000*age_1*bmi_1,
               -0.1079548126277638100000000*age_1*bmi_2,
               -0.6011853042930119800000000*age_1*fh_diab,
               -0.0000519927442172335000000*age_2*antipsy,
               0.0007102643855968814100000*age_2*learndiff,
               0.0013508364599531669000000*age_2*statins,
               -0.0011797722394560309000000*age_2*bmi_1,
               0.0002147150913931929100000*age_2*bmi_2,
               0.0004914185594087803400000*age_2*fh_diab)

    # Risk Score #
    score <- sum(ethnicity, smoking, bmi, age, townsend, antipsy, steroids, cvd, learndiff, schizobipo, statins, hypertension, fh_diab, int)
    risk <- 100*(1 - 0.978732228279114^exp(score))
  }

  ## Output ##
  return(risk)
}
