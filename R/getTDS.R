#=====================================#
#                                     #
#### GET TOWSEND DEPRIVATION SCORE ####
#                                     #
#=====================================#

getTDS <- function(postcode = NULL){
  if(is.null(postcode)) stop("One or more values must be specified.")
  stopifnot(is.character(postcode))
  postcode <- gsub("\\s+", "", postcode)
  postcode <- toupper(postcode)
  if(any(nchar(postcode) < 5 | nchar(postcode) > 7)) stop("Postcode is not in a recognisable format.")
  tds <- dat_oa[match(postcode, dat_oa$postcode), "tds"]
  if(any(is.na(tds))) stop("One or more postcodes could not be linked to a Townsend deprivation score.")
  return(tds)
}
