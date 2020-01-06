#============================#
#                            #
#### QDIABETES-2018 (ALL) ####
#                            #
#============================#

### Notes ###
# - fpg >= 2 & fpg < 7
# - hba1c >= 15 & hba1c < 48
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40

QDR <- function(gender = NULL, age = NULL, bmi = NULL, height = NULL, weight = NULL, fpg = NULL, hba1c = NULL, ethnicity = "WhiteNA", smoking = "Non", townsend = 0, antipsy = FALSE, steroids = FALSE, cvd = FALSE, gestdiab = FALSE, learndiff = FALSE, schizobipo = FALSE, pcos = FALSE, statins = FALSE, hypertension = FALSE, fh_diab = FALSE){
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
  return(risk)
}
