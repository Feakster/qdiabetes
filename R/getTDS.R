#=====================================#
#                                     #
#### GET TOWSEND DEPRIVATION SCORE ####
#                                     #
#=====================================#

### License Information ###
# - Contains OS data © Crown copyright and database right 2020
# - Contains Royal Mail data © Royal Mail copyright and database right 2020
# - Source: Office for National Statistics licensed under the Open Government Licence v.3.0

### Notes ###
# - https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom
# Historic ^NPT (Newport) postcodes appear to be present in the postcode database alongside the newer counterparts.

# ### KISS Principles ###
# lon_pre <- paste0("[A-Z]{1,2}", "\\d", "[A-Z]")
# gen_pre <- paste0("[A-Z]{1,2}", "\\d{1,2}")
# npt_pre <- "NPT"
# gen_suff <- paste0("\\d", "[A-Z]{2}")
#
# pc_regex <- paste0("^(", "(", lon_pre, "|", gen_pre, "|", npt_pre, ")", gen_suff, ")$")

getTDS <- function(postcode = NULL){
  if(is.null(postcode)) stop("One or more values must be specified.")
  stopifnot(is.character(postcode))
  postcode <- gsub("\\s+", "", postcode)
  postcode <- toupper(postcode)
  if(any(!grepl("^(([A-Z]{1,2}\\d[A-Z]|[A-Z]{1,2}\\d{1,2}|NPT)\\d[A-Z]{2})$", postcode))) stop("Postcode is not in a recognisable format.")
  tds <- .dat_oa[match(postcode, .dat_oa$postcode), "tds"]
  if(any(is.na(tds))) stop("One or more postcodes could not be linked to a Townsend deprivation score.")
  return(tds)
}
