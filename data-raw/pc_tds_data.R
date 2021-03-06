#=================================#
#                                 #
#### POSTCODE TO TOWNSEND DATA ####
#                                 #
#=================================#


### License Information ###
# - Contains OS data (C) Crown copyright and database right 2019
# - Contains Royal Mail data (C) Royal Mail copyright and database right 2019
# - Source: Office for National Statistics licensed under the Open Government Licence v.3.0


### Temporary Directory ###
dir_tmp <- tempdir()


### Notes ###
## Postcode-to-Region Data ##
# - http://geoportal1-ons.opendata.arcgis.com/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-february-2019-lookup-in-the-uk
## Region-to-Townsend Data ##
# - (2011) https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores
# - (2001) https://census.ukdataservice.ac.uk/get-data/related/deprivation


### Postcode Data ###
url_pc <- "https://www.arcgis.com/sharing/rest/content/items/c479d770cba14845a0e43db4e3eb5afa/data"
download.file(url_pc, file.path(dir_tmp, "postcode_to_region.zip")); rm(url_pc)
name_old <- unzip(file.path(dir_tmp, "postcode_to_region.zip"), list = T)[["Name"]]
unzip(file.path(dir_tmp, "postcode_to_region.zip"), exdir = dir_tmp)
invisible(file.rename(file.path(dir_tmp, name_old), file.path(dir_tmp, "postcode_to_region.csv"))); rm(name_old)
dat_pc <- read.delim(file.path(dir_tmp, "postcode_to_region.csv"),
                     sep = ",",
                     header = T,
                     na.strings = "",
                     colClasses = "character")
dat_pc <- dat_pc[c("pcds", "oa11cd", "lsoa11cd")]
names(dat_pc) <- c("postcode", "oa_code", "lsoa_code")
dat_pc <- na.omit(dat_pc)
dat_pc <- dat_pc[!grepl("^BT\\d", dat_pc[["postcode"]], perl = TRUE), ] # Remove NI Postcodes.


### Townsend Data ###
## Output Area ##
url_oa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1195/download"
download.file(url_oa, file.path(dir_tmp, "oa_to_tds.csv")); rm(url_oa)
dat_oa <- fread(file.path(dir_tmp, "oa_to_tds.csv"),
                sep = ",",
                header = T,
                na.strings = "",
                select = c("geo_code", "TDS"),
                blank.lines.skip = T)
setnames(dat_oa, tolower(names(dat_oa)))
setnames(dat_oa, "geo_code", "oa_code")
dat_oa <- na.omit(dat_oa)


## Lower Layer Super Output Area ##
url_lsoa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1194/download"
download.file(url_lsoa, file.path(dir_tmp, "lsoa_to_tds.csv")); rm(url_lsoa)
dat_lsoa <- read.delim(file.path(dir_tmp, "lsoa_to_tds.csv"),
                       sep = ",",
                       header = TRUE,
                       na.strings = "",
                       stringsAsFactors = FALSE)
dat_lsoa <- dat_lsoa[c("GEO_CODE", "TDS")]
names(dat_lsoa) <- tolower(names(dat_lsoa))
names(dat_lsoa)[names(dat_lsoa) == "geo_code"] <- "lsoa_code"


### Merge Datasets ###
dat_oa <- merge(dat_pc[, .(postcode, oa_code)], dat_oa, by = "oa_code", all = FALSE)
dat_lsoa <- merge(dat_pc[c("postcode", "lsoa_code")], dat_lsoa, by = "lsoa_code", all = FALSE)
rm(dat_pc)


### Sanitize White Space ###
dat_oa <- dat_oa[, postcode := gsub("\\s+", "", postcode)]
dat_lsoa[["postcode"]] <- gsub("\\s+", "", dat_lsoa[["postcode"]], perl = TRUE)


### Add Prefix & Suffix Columns ###
dat_oa[, post_pre := gsub("\\w{3}$", "", postcode)]
dat_lsoa[["post_pre"]] <- gsub("\\w{3}$", "", dat_lsoa[["postcode"]], perl = TRUE)
dat_oa[, post_suff := regmatches(postcode, regexpr("\\w{3}$", postcode))]
dat_lsoa[["post_suff"]] <- regmatches(dat_lsoa[["postcode"]], regexpr("\\w{3}$", dat_lsoa[["postcode"]], perl = TRUE))


### Convert Prefix and Suffix Columns to Factor ###
dat_oa[, post_pre := factor(post_pre)]
dat_lsoa[["post_pre"]] <- factor(dat_lsoa[["post_pre"]])
dat_oa[, post_suff := factor(post_suff)]
dat_lsoa[["post_suff"]] <- factor(dat_lsoa[["post_suff"]])


### Sort ###
dat_oa <- dat_oa[, .(postcode, post_pre, post_suff, tds)]
dat_lsoa <- dat_lsoa[c("postcode", "post_pre", "post_suff", "tds")]
setkey(dat_oa, post_pre, post_suff)
dat_lsoa <- dat_lsoa[order(dat_lsoa[["post_pre"]], dat_lsoa[["post_suff"]]), ]


### Make Unexported Objects ###
.dat_oa <- as.data.frame(dat_oa); rm(dat_oa)
.dat_lsoa <- dat_lsoa; rm(dat_lsoa)

### Tidy Up ###
rm(dir_tmp)
