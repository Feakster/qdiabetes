#=================================#
#                                 #
#### HOOKS FOR NAMESPACE EVENTS ###
#                                 #
#=================================#

.onLoad <- function(libname, pkgname){
  invisible(.dat_oa)
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(
    paste(
      "Contains OS data (C) Crown copyright and database right 2019",
      "Contains Royal Mail data (C) Royal Mail copyright and database right 2019",
      "Source: Office for National Statistics licensed under the Open Government Licence v.3.0",
      sep = "\n"
    )
  )
}