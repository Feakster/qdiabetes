#=================================#
#                                 #
#### HOOKS FOR NAMESPACE EVENTS ###
#                                 #
#=================================#

.onAttach <- function(libname, pkgname){
  setHook(packageEvent("QDiabetes", "attach"), function(...){
    packageStartupMessage(
      "Contains OS data © Crown copyright and database right 2020",
      "Contains Royal Mail data © Royal Mail copyright and database right 2020",
      "Source: Office for National Statistics licensed under the Open Government Licence v.3.0"
    )
  })
}