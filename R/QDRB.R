#================================#
#                                #
#### QDIABETES-2018 (MODEL B) ####
#                                #
#================================#

### Notes ###
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - fpg >= 2 & fpg < 7
# - townsend >= -7.028634577 & townsend <= 13.3114711

QDRB <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, fpg = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE, gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE, pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE){
  ## Stop Conditions ##
  if(any(c(is.null(gender), is.null(age), is.null(fpg)))) stop("gender, age & fpg must be specified")
  if(is.null(bmi) & any(c(is.null(height), is.null(weight)))) stop("either bmi or height & weight must be specified")
  stopifnot(gender %in% c("Male", "Female"))
  stopifnot(ethnicity %in% c("WhiteNA", "Indian", "Pakistani", "Bangladeshi", "OtherAsian", "BlackCaribbean", "BlackAfrican", "Chinese", "Other"))
  stopifnot(smoking %in% c("Non", "Ex", "Light", "Moderate", "Heavy"))
  stopifnot(all(c(antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab) %in% c(FALSE, TRUE)))
  stopifnot(fpg >= 2 & fpg < 7)
  stopifnot(age >= 25 & age < 85)
  stopifnot(height >= 1.4 & height <= 2.1)
  stopifnot(weight >= 40 & weight <= 180)
  stopifnot(townsend >= -7.028634577 & townsend <= 13.3114711)
  stopifnot(antipsy %in% c(FALSE, TRUE))
  stopifnot(steroids %in% c(FALSE, TRUE))
  stopifnot(cvd %in% c(FALSE, TRUE))
  stopifnot(gestdiab %in% c(FALSE, TRUE))
  stopifnot(learndiff %in% c(FALSE, TRUE))
  stopifnot(schizobipo %in% c(FALSE, TRUE))
  stopifnot(pcos %in% c(FALSE, TRUE))
  stopifnot(statins %in% c(FALSE, TRUE))
  stopifnot(hypertension %in% c(FALSE, TRUE))
  stopifnot(fh_diab %in% c(FALSE, TRUE))

  ## BMI Pre-Processing ##
  if(all(c(!is.null(bmi), !is.null(height), !is.null(weight)))) warning("bmi, height & weight all specified, height & weight ignored", call. = FALSE)
  if(is.null(bmi)) bmi <- weight/height^2
  stopifnot(bmi >= 40/2.1^2 & bmi <= 180/1.4^2)
  if(bmi < 20){
    warning("bmi < 20. Setting bmi == 20.", call. = FALSE)
    bmi <- 20
  }
  if(bmi > 40){
    warning("bmi > 40. Setting bmi == 40.", call. = FALSE)
    bmi <- 40
  }

  ## Female ##
  if(gender == "Female"){
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 0.9898906127239111000000000,
      Pakistani = 1.2511504196326508000000000,
      Bangladeshi = 1.4934757568196120000000000,
      OtherAsian = 0.9673887434565966400000000,
      BlackCaribbean = 0.4844644519593178100000000,
      BlackAfrican = 0.4784214955360102700000000,
      Chinese = 0.7520946270805577400000000,
      Other = 0.4050880741541424400000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.0374156307236963230000000,
      Light = 0.2252973672514482800000000,
      Moderate = 0.3099736428023662800000000,
      Heavy = 0.4361942139496417500000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- dage^0.5
    age_2 <- dage^3
    age_1 <- age_1 - 2.123332023620606
    age_2 <- age_2 - 91.644744873046875
    age <- 3.7650129507517280000000000*age_1 - 0.0056043343436614941000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 2.571253299713135
    bmi_2 <- bmi_2 - 16.999439239501953
    bmi <- 2.4410935031672469000000000*bmi_1 - 0.0421526334799096420000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.391116052865982
    townsend <- 0.0358046297663126500000000*townsend

    # FPG #
    fpg_1 <- fpg^-1
    fpg_2 <- log(fpg)*fpg^-1
    fpg_1 <- fpg_1 - 0.208309367299080
    fpg_2 <- fpg_2 - 0.326781362295151
    fpg <- -2.1887891946337308000000000*fpg_1 - 69.9608419828660290000000000*fpg_2

    # Binary Variables #
    antipsy <- 0.4748378550253853400000000*antipsy
    steroids <- 0.3767933443754728500000000*steroids
    cvd <- 0.1967261568066525100000000*cvd
    gestdiab <- 1.0689325033692647000000000*gestdiab
    learndiff <- 0.4542293408951034700000000*learndiff
    schizobipo <- 0.1616171889084260500000000*schizobipo
    pcos <- 0.3565365789576717100000000*pcos
    statins <- 0.5809287382718667500000000*statins
    hypertension <- 0.2836632020122907300000000*hypertension
    fh_diab <- 0.4522149766206111600000000*fh_diab

    # Interaction Terms #
    int <- sum(-0.7683591642786522500000000*age_1*antipsy,
               -0.7983128124297588200000000*age_1*learndiff,
               -1.9033508839833257000000000*age_1*statins,
               0.4844747602404915200000000*age_1*bmi_1,
               -0.0319399883071813450000000*age_1*bmi_2,
               2.2442903047404350000000000*age_1*fpg_1,
               13.0068388699783030000000000*age_1*fpg_2,
               -0.3040627374034501300000000*age_1*fh_diab,
               0.0005194455624413476200000*age_2*antipsy,
               0.0003028327567161890600000*age_2*learndiff,
               0.0024397111406018711000000*age_2*statins,
               -0.0041572976682154057000000*age_2*bmi_1,
               0.0001126882194204252200000*age_2*bmi_2,
               0.0199345308534312550000000*age_2*fpg_1,
               -0.0716677187529306680000000*age_2*fpg_2,
               0.0004523639671202325400000*age_2*fh_diab)

    # Risk Score #
    score <- sum(ethnicity, smoking, bmi, age, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab, fpg, int)
    risk <- 100*(1 - 0.990905702114105^exp(score))
  }

  ## Male ##
  if(gender == "Male"){
    if(any(pcos, gestdiab)) stop("'pcos' and 'gestdiab' must be set to FALSE for male 'gender'")
    # Ethnicity #
    list_eth <- list(
      WhiteNA = 0,
      Indian = 1.0081475800686235000000000,
      Pakistani = 1.3359138425778705000000000,
      Bangladeshi = 1.4815419524892652000000000,
      OtherAsian = 1.0384996851820663000000000,
      BlackCaribbean = 0.5202348070887524700000000,
      BlackAfrican = 0.8579673418258558800000000,
      Chinese = 0.6413108960765615500000000,
      Other = 0.4838340220821504800000000
    )
    ethnicity <- list_eth[[ethnicity]]

    # Smoking #
    list_smok <- list(
      Non = 0,
      Ex = 0.1119475792364162500000000,
      Light = 0.3110132095412204700000000,
      Moderate = 0.3328898469326042100000000,
      Heavy = 0.4257069026941993100000000
    )
    smoking <- list_smok[[smoking]]

    # Age #
    dage <- age/10
    age_1 <- log(dage)
    age_2 <- dage^3
    age_1 <- age_1 - 1.496392488479614
    age_2 <- age_2 - 89.048171997070313
    age <- 4.1149143302364717000000000*age_1 - 0.0047593576668505362000000*age_2

    # BMI #
    dbmi <- bmi/10
    bmi_1 <- dbmi^2
    bmi_2 <- dbmi^3
    bmi_1 <- bmi_1 - 6.817805767059326
    bmi_2 <- bmi_2 - 17.801923751831055
    bmi <- 0.8169361587644297100000000*bmi_1 - 0.1250237740343336200000000*bmi_2

    # Townsend #
    townsend <- townsend - 0.515986680984497
    townsend <- 0.0253741755198943560000000*townsend

    # FPG #
    fpg_1 <- fpg^-0.5
    fpg_2 <- log(fpg)*fpg^-0.5
    fpg_1 <- fpg_1 - 0.448028832674026
    fpg_2 <- fpg_2 - 0.719442605972290
    fpg <- -54.8417881280971070000000000*fpg_1 - 53.1120784984813600000000000*fpg_2

    # Binary Variables #
    antipsy <- 0.4417934088889577400000000*antipsy
    steroids <- 0.3413547348339454100000000*steroids
    cvd <- 0.2158977454372756600000000*cvd
    learndiff <- 0.4012885027585300100000000*learndiff
    schizobipo <- 0.2181769391399779300000000*schizobipo
    statins <- 0.5147657600111734700000000*statins
    hypertension <- 0.2467209287407037300000000*hypertension
    fh_diab <- 0.5749437333987512700000000*fh_diab

    # Interaction Terms #
    int <- sum(-0.9502224313823126600000000*age_1*antipsy,
               -0.8358370163090045300000000*age_1*learndiff,
               -1.8141786919269460000000000*age_1*statins,
               0.3748482092078384600000000*age_1*bmi_1,
               -0.0909836579562487420000000*age_1*bmi_2,
               21.0117301217643340000000000*age_1*fpg_1,
               23.8244600447469740000000000*age_1*fpg_2,
               -0.6780647705291665800000000*age_1*fh_diab,
               0.0001472972077162874300000*age_2*antipsy,
               0.0006012919264966409100000*age_2*learndiff,
               0.0016393484911405418000000*age_2*statins,
               -0.0010774782221531713000000*age_2*bmi_1,
               0.0001911048730458310100000*age_2*bmi_2,
               -0.0390046079223835270000000*age_2*fpg_1,
               -0.0411277198058959470000000*age_2*fpg_2,
               0.0006257588248859499300000*age_2*fh_diab)

    # Risk Score #
    score <- sum(ethnicity, smoking, age, bmi, townsend, antipsy, steroids, cvd, learndiff, schizobipo, statins, hypertension, fh_diab, fpg, int)
    risk <- 100*(1 - 0.985019445419312^exp(score))
  }

  ## Output ##
  return(risk)
}
