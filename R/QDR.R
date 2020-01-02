#============================#
#                            #
#### QDIABETES-2018 (ALL) ####
#                            #
#============================#

### Notes ###
# - fpg >= 2 & fpg < 7
# - hba1c >= 15 & hba1c < 48
# - age >= 25 & age < 85
# - height >= 140 & height <= 210
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40

QDR <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, fpg = NULL, hba1c = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = F, steroids = F, cvd = F, gestdiab = F, learndiff = F, schizobipo = F, pcos = F, statins = F, hypertension = F, fh_diab = F){
  ## Warnings ##
  if(all(c(!is.null(fpg), !is.null(hba1c)))) warning("fpg & hba1c specified, hba1c ignored", call. = F)

  ## Algorithm Selection ##
  if(!is.null(fpg)){
    risk <- QDRB(gender, age, bmi, height, weight, fpg, ethnicity, smoking, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab)
  } else if(!is.null(hba1c)){
    risk <- QDRC(gender, age, bmi, height, weight, hba1c, ethnicity, smoking, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab)
  } else {
    risk <- QDRA(gender, age, bmi, height, weight, ethnicity, smoking, townsend, antipsy, steroids, cvd, gestdiab, learndiff, schizobipo, pcos, statins, hypertension, fh_diab)
  }

  ## Output ##
  return(round(risk, 2))
}
