#================================#
#                                #
#### QDIABETES-2018 (MODEL C) ####
#                                #
#================================#

### Notes ###
# - hba1c >= 15 & hba1c < 48
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40

QDRC <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, hba1c = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = F, steroids = F, cvd = F, gestdiab = F, learndiff = F, schizobipo = F, pcos = F, statins = F, hypertension = F, fh_diab = F){
  ## Stop Conditions ##
  if(any(c(is.null(gender), is.null(age), is.null(hba1c)))) stop("gender, age & hba1c must be specified")
  if(is.null(bmi) & any(c(is.null(height), is.null(weight)))) stop("Either bmi or height & weight must be specified")
  stopifnot(gender %in% c("Male", "Female"))
  stopifnot(ethnicity %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other"))
  stopifnot(smoking %in% c("Non", "Ex", "Light", "Moderate", "Heavy"))
  stopifnot(hba1c >= 15 & hba1c < 48)
  stopifnot(age >= 25 & age < 85)
  stopifnot(height >= 1.40 & height <= 2.10)
  stopifnot(weight >= 40 & weight <= 180)
  # stopifnot(townsend >= -6 & townsend <= 11)

  ## BMI Pre-Processing ##
  if(all(c(!is.null(bmi), !is.null(height), !is.null(weight)))) warning("bmi, height & weight all specified, height & weight ignored", call. = F)
  if(is.null(bmi)) bmi <- weight/height^2
  stopifnot(bmi >= 40/2.10^2 & bmi <= 180/1.4^2)
  if(bmi < 20){
    warning("bmi < 20. Setting bmi == 20", call. = F)
    bmi <- 20
  }
  if(bmi > 40){
    warning("bmi > 40. Setting bmi == 40", call. = F)
    bmi <- 40
  }

  ## Female ##
  if(gender == "Female"){
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 0.5990951599291540800000000,
      Pakistani = 0.7832030965635389300000000,
      Bangladeshi = 1.1947351247960103000000000,
      OtherAsian = 0.7141744699168143300000000,
      BlackCaribbean = 0.1195328468388768800000000,
      BlackAfrican = 0.0136688728784904270000000,
      Chinese = 0.5709226537693945500000000,
      Other = 0.1709107628106929200000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.0658482585100006730000000,
      Light = 0.1458413689734224000000000,
      Moderate = 0.1525864247480118700000000,
      Heavy = 0.3078741679661397600000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- dage^0.5
    age_2 <- dage^3
    age_1 <- age_1 - 2.123332023620606
    age_2 <- age_2 - 91.644744873046875
    age <- 3.5655214891947722000000000*age_1 - 0.0056158243572733135000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 2.571253299713135
    bmi_2 <- bmi_2 - 16.999439239501953
    bmi <- 2.5043028874544841000000000*bmi_1 - 0.0428758018926904610000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.391116052865982
    townsend <- 0.0358668220563482940000000*townsend

    # HbA1c #
    dhba1c <- hba1c/10
    hba1c_1 <- dhba1c^0.5
    hba1c_2 <- dhba1c
    hba1c_1 <- hba1c_1 - 1.886751174926758
    hba1c_2 <- hba1c_2 - 3.559829950332642
    hba1c <- 8.7368031307362184000000000*hba1c_1 - 0.0782313866699499700000000*hba1c_2

    # Binary Variables #
    antipsy <- 0.5497633311042200400000000*antipsy
    steroids <- 0.1687220550638970400000000*steroids
    cvd <- 0.1644330036273934400000000*cvd
    gestdiab <- 1.1250098105171140000000000*gestdiab
    learndiff <- 0.2891205831073965800000000*learndiff
    schizobipo <- 0.3182512249068407700000000*schizobipo
    pcos <- 0.3380644414098174500000000*pcos
    statins <- 0.4559396847381116400000000*statins
    hypertension <- 0.4040022295023758000000000*hypertension
    fh_diab <- 0.4428015404826031700000000*fh_diab

    # Interaction Terms #
    int <- sum(-0.8125434197162131300000000*age_1*antipsy,
               -0.9084665765269808200000000*age_1*learndiff,
               -1.8557960585560658000000000*age_1*statins,
               0.6023218765235252000000000*age_1*bmi_1,
               -0.0344950383968044700000000*age_1*bmi_2,
               -0.2727571351506187200000000*age_1*fh_diab,
               25.4412033227367150000000000*age_1*hba1c_1,
               -6.8076080421556107000000000*age_1*hba1c_2,
               0.0004665611306005428000000*age_2*antipsy,
               0.0008518980139928006500000*age_2*learndiff,
               0.0022627250963352537000000*age_2*statins,
               -0.0043386645663133425000000*age_2*bmi_1,
               0.0001162778561671208900000*age_2*bmi_2,
               0.0004354519795220774900000*age_2*fh_diab,
               -0.0522541355885925220000000*age_2*hba1c_1,
               0.0140548259061144530000000*age_2*hba1c_2)

    # Risk Score #
    score <- sum(ethnicity, smoking, bmi, age, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab, hba1c, int)
    risk <- 100*(1 - 0.988788545131683^exp(score))
  }

  ## Male ##
  if(gender == "Male"){
    if(any(pcos, gestdiab)) stop("'pcos' and 'gestdiab' must be set to FALSE for male 'gender'")
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 0.6757120705498780300000000,
      Pakistani = 0.8314732504966345600000000,
      Bangladeshi = 1.0969133802228563000000000,
      OtherAsian = 0.7682244636456048200000000,
      BlackCaribbean = 0.2089752925910850200000000,
      BlackAfrican = 0.3809159378197057900000000,
      Chinese = 0.3423583679661269500000000,
      Other = 0.2204647785343308300000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.1159289120687865100000000,
      Light = 0.1462418263763327100000000,
      Moderate = 0.1078142411249314200000000,
      Heavy = 0.1984862916366847400000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- log(dage)
    age_2 <- dage^3
    age_1 <- age_1 - 1.496392488479614
    age_2 <- age_2 - 89.048171997070313
    age <- 4.0193435623978031000000000*age_1 - 0.0048396442306278238000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi^2
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 6.817805767059326
    bmi_2 <- bmi_2 - 17.801923751831055
    bmi <- 0.8182916890534932500000000*bmi_1 - 0.1255880870135964200000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.515986680984497
    townsend <- 0.0252299651849007270000000*townsend

    # HbA1c #
    dhba1c <- hba1c/10
    hba1c_1 <- dhba1c^0.5
    hba1c_2 <- dhba1c
    hba1c_1 <- hba1c_1 - 1.900265336036682
    hba1c_2 <- hba1c_2 - 3.611008167266846
    hba1c <- 8.0511642238857934000000000*hba1c_1 - 0.1465234689391449500000000*hba1c_2

    # Binary Variables #
    antipsy <- 0.4554152522017330100000000*antipsy
    steroids <- 0.1381618768682392200000000*steroids
    cvd <- 0.1454698889623951800000000*cvd
    learndiff <- 0.2596046658040857000000000*learndiff
    schizobipo <- 0.2852378849058589400000000*schizobipo
    statins <- 0.4255195190118552500000000*statins
    hypertension <- 0.3316943000645931100000000*hypertension
    fh_diab <- 0.5661232594368061900000000*fh_diab

    # Interaction Terms #
    int <- sum(-1.0013331909079835000000000*age_1*antipsy,
               -0.8916465737221592700000000*age_1*learndiff,
               -1.7074561167819817000000000*age_1*statins,
               0.4507452747267244300000000*age_1*bmi_1,
               -0.1085185980916560100000000*age_1*bmi_2,
               -0.6141009388709716100000000*age_1*fh_diab,
               27.6705938271465650000000000*age_1*hba1c_1,
               -7.4006134846785434000000000*age_1*hba1c_2,
               0.0002245597398574240700000*age_2*antipsy,
               0.0006604436076569648200000*age_2*learndiff,
               0.0013873509357389619000000*age_2*statins,
               -0.0012224736160287865000000*age_2*bmi_1,
               0.0002266731010346126000000*age_2*bmi_2,
               0.0005060258289477209100000*age_2*fh_diab,
               -0.0592014581247543300000000*age_2*hba1c_1,
               0.0155920894851499880000000*age_2*hba1c_2)

    # Risk Score #
    score <- sum(ethnicity, smoking, bmi, age, townsend, antipsy, steroids, cvd, learndiff, schizobipo, statins, hypertension, fh_diab, hba1c, int)
    risk <- 100*(1 - 0.981181740760803^exp(score))
  }

  ## Output ##
  return(risk)
}
