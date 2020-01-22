#============================#
#                            #
#### QDIABETES-2018 (ALL) ####
#                            #
#============================#

### License Information ###
# The QDR function is part of the QDiabetes package, and is for
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
# - fpg >= 2 & fpg < 7
# - hba1c >= 15 & hba1c < 48
# - age >= 25 & age < 85
# - height >= 1.40 & height <= 2.10
# - weight >= 40 & weight <= 180
# - bmi == 20 if bmi < 20
# - bmi == 40 if bmi > 40
# - townsend >= -7.028634577 & townsend <= 13.3114711

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
