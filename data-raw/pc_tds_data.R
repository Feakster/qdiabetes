#=================================#
#                                 #
#### POSTCODE TO TOWNSEND DATA ####
#                                 #
#=================================#

### License Information ###
# - Contains OS data © Crown copyright and database right 2019
# - Contains Royal Mail data © Royal Mail copyright and database right 2019
# - Source: Office for National Statistics licensed under the Open Government Licence v.3.0

### Libraries ###
library(data.table)

### Options ###
setDTthreads(0)

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
dat_pc <- fread(file.path(dir_tmp, "postcode_to_region.csv"), sep = ",",
                header = T,
                na.strings = "",
                select = c("pcds", "oa11cd", "lsoa11cd"),
                colClasses = "character",
                blank.lines.skip = T)
setnames(dat_pc, c("pcds", "oa11cd", "lsoa11cd"), c("postcode", "oa_code", "lsoa_code"))
dat_pc <- na.omit(dat_pc)
dat_pc <- dat_pc[!{postcode %like% "^BT\\d"}] # Remove NI Postcodes.

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

# ## Lower Layer Super Output Area ##
# url_lsoa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1194/download"
# download.file(url_lsoa, file.path(dir_tmp, "lsoa_to_tds.csv")); rm(url_lsoa)
# dat_lsoa <- fread(file.path(dir_tmp, "lsoa_to_tds.csv"),
#                   sep = ",",
#                   header = T,
#                   na.strings = "",
#                   select = c("GEO_CODE", "TDS"),
#                   blank.lines.skip = T)
# setnames(dat_lsoa, tolower(names(dat_lsoa)))
# setnames(dat_lsoa, "geo_code", "lsoa_code")

### Merge Datasets ###
dat_oa <- merge(dat_pc[, .(postcode, oa_code)], dat_oa, by = "oa_code", all = F)
# dat_lsoa <- merge(dat_pc[, .(postcode, lsoa_code)], dat_lsoa, by = "lsoa_code", all = F)
rm(dat_pc)

### Sanitize White Space ###
dat_oa <- dat_oa[, postcode := gsub("\\s+", "", postcode)]
# dat_lsoa <- dat_lsoa[, postcode := gsub("\\s+", "", postcode)]

### Sort ###
dat_oa <- dat_oa[, .(postcode, tds)]
# dat_lsoa <- dat_lsoa[, .(postcode, tds)]
setkey(dat_oa, postcode)
# setkey(dat_lsoa, postcode)

### Export ###
.dat_oa <- as.data.frame(dat_oa); rm(dat_oa)
# .dat_lsoa <- as.data.frame(dat_lsoa); rm(dat_lsoa)
save(list = ls(pattern = "^\\.dat_(ls)?oa$", all.names = T), file = "R/sysdata.rda", version = 2, compress = "xz")
rm(.dat_oa)
# rm(.dat_lsoa)
rm(dir_tmp)
