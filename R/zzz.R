#=================================#
#                                 #
#### HOOKS FOR NAMESPACE EVENTS ###
#                                 #
#=================================#

.onLoad <- function(libname, pkgname){
  invisible(.dat_oa)
}

.onAttach <- function(libname, pkgname){
  setHook(packageEvent("QDiabetes", "attach"), function(...){
    packageStartupMessage(
      paste(
        "Contains OS data Crown copyright and database right 2019",
        "Contains Royal Mail data Royal Mail copyright and database right 2019",
        "Source: Office for National Statistics licensed under the Open Government Licence v.3.0",
        sep = "\n"
      )
    )
  })
}