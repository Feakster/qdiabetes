#=====================================#
#                                     #
#### GET TOWSEND DEPRIVATION SCORE ####
#                                     #
#=====================================#

getTDS <- function(postcode = NULL){
  postcode <- gsub("\\s+", "", postcode)
  postcode <- toupper(postcode)
  if(nchar(postcode) < 5 | nchar(postcode) > 7) stop("Postcode is not in a recognisable format!")
  tds <- dat_oa[dat_oa$postcode == postcode, "tds"]
  if(length(tds) == 0){
    warning("Postcode not found!")
    tds <- NA
  }
  return(tds)
}
