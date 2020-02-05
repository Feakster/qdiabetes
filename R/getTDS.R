#=====================================#
#                                     #
#### GET TOWSEND DEPRIVATION SCORE ####
#                                     #
#=====================================#

### License Information ###
# - Contains OS data \U00A9 Crown copyright and database right 2020
# - Contains Royal Mail data \U00A9 Royal Mail copyright and database right 2020
# - Source: Office for National Statistics licensed under the Open Government Licence v.3.0

### Notes ###
# - https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom
# - https://www.doogal.co.uk/london_postcodes.php
# Historic ^NPT (Newport) postcodes appear to be present in the postcode database alongside the newer counterparts.
# - Partial postcodes must contain a minimum of the entire prefix.

# ### KISS Principles ###
# lon_pre <- paste0("[A-Z]{1,2}", "\\d", "[A-Z]")
# gen_pre <- paste0("[A-Z]{1,2}", "\\d{1,2}")
# npt_pre <- "NPT"
# gen_suff <- paste0("\\d", "[A-Z]{2}")
#
# pc_regex <- paste0("^(", "(", lon_pre, "|", gen_pre, "|", npt_pre, ")", gen_suff, ")$")

### Function ###
getTDS <- function(postcode = NULL, type = "full", squash = if(type != "regex") FALSE else TRUE){
  ## Stop Conditions & Warnings ##
  if(is.null(postcode)) stop("One or more values must be specified.")
  stopifnot(type %in% c("full", "prefix", "suffix", "regex"))
  stopifnot(is.logical(squash))
  stopifnot(is.character(postcode))
  if(type == "regex"){
    if(length(postcode) > 1) stop("Only the use of a single regex term is supported.")
    if(squash == FALSE) stop("'squash' must be set to TRUE when 'type' is set to 'regex'.")
    if(any(sapply(c(" ", "\\s", "\\S"), grepl, x = postcode, fixed = TRUE))) stop("The postcode database contains no white space.")
    if(grepl("CRO", postcode, ignore.case = TRUE)) stop("The 'CRO' prefix is no longer used for Croydon.")
    if(any(sapply(c("\\b", "\\B"), grepl, x = postcode, fixed = TRUE))) warning("The postcode database contains no white space. Use '^' '$'.")
    if(grepl("NPT", postcode, ignore.case = TRUE)) warning("'NPT' is the old prefix for Newport (Gwent). Consider using 'NP9'.")
  } else if(type %in% c("full", "prefix")){
    if(any(startsWith(postcode, "CRO"))) stop("The 'CRO' prefix is no longer used for Croydon.")
    if(any(startsWith(postcode, "NPT"))) warning("'NPT' is the old prefix for Newport (Gwent). Consider using 'NP9'.")
  }
  
  ## Pre-Processing ##
  if(type != "regex") postcode <- toupper(postcode)
  
  ## Matching ##
  if(type == "full"){
    # Full Matching #
    postcode <- gsub("\\s+", "", postcode) # Remove white space.
    if(any(!grepl("^(([A-Z]{1,2}\\d[A-Z]|[A-Z]{1,2}\\d{1,2}|NPT)\\d[A-Z]{2})$", postcode))) stop("One or more postcodes are not in a recognisable format.")
    dat_tmp <- .dat_oa[match(postcode, .dat_oa$postcode), ]
    if(squash == TRUE){
      tds <- median(dat_tmp[, "tds"])
    } else {
      tds <- dat_tmp[, "tds"]
    }
  } else if(type == "prefix"){
    # Prefix Matching #
    if(any(!grepl("^([A-Z]{1,2}\\d[A-Z]|[A-Z]{1,2}\\d{1,2}|NPT)$", postcode))) stop("One or more postcode prefixes are not in a recognisable format.")
    dat_tmp <- .dat_oa[.dat_oa$post_pre %in% postcode, ]
    if(squash == TRUE){
      tds <- median(dat_tmp[, "tds"])
    } else {
      tds <- with(dat_tmp, tapply(tds, post_pre, median))
      tds <- tds[match(postcode, names(tds))]
      tds <- as.vector(tds)
    }
  } else if(type == "suffix"){
    # Suffix Matching #
    if(any(!grepl("^\\d[A-Z]{2}$", postcode))) stop("One or more postcode suffixes are not in a recognisable format.")
    dat_tmp <- .dat_oa[.dat_oa$post_suff %in% postcode, ]
    if(squash == TRUE){
      tds <- median(dat_tmp[, "tds"])
    } else {
      tds <- with(dat_tmp, tapply(tds, post_suff, median))
      tds <- tds[match(postcode, names(tds))]
      tds <- as.vector(tds)
    }
  } else {
    # Regex Matching #
    dat_tmp <- .dat_oa[grepl(postcode, .dat_oa$postcode), ]
    tds <- median(dat_tmp[, "tds"])
  }
  
  ## Output Checks ##
  if(any(is.na(tds))) stop("One or more postcodes could not be linked to a Townsend deprivation score.")
  
  ## Named Output ##
  if(length(tds) > 1){
    names(tds) <- postcode
  }
  
  ## Output ##
  return(tds)
}
