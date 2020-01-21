#=================================#
#                                 #
#### POSTCODE TO TOWNSEND DATA ####
#                                 #
#=================================#

### Libraries ###
library(data.table)

### Options ###
setDTthreads(0)

### Notes ###
# - https://data.gov.uk/dataset/9b090605-9861-4bb4-9fa4-6845daa2de9b/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-february-2018-lookup-in-the-uk

### Postcode Data ###
# http://geoportal1-ons.opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0
url_pc <- "http://geoportal1-ons.opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0.csv"
dat_pc <- fread(url_pc, sep = ",",
                header = T,
                na.strings = "",
                select = c("pcds", "oa11"),
                colClasses = "character",
                blank.lines.skip = T); rm(url_pc)
setnames(dat_pc, c("pcds", "oa11"), c("postcode", "oa_code"))
dat_pc <- na.omit(dat_pc)

### Townsend Data ###
# https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores
url_oa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1195/download"
dat_oa <- fread(url_oa,
                sep = ",",
                header = T,
                na.strings = "",
                select = c("geo_code", "TDS"),
                blank.lines.skip = T); rm(url_oa)
setnames(dat_oa, tolower(names(dat_oa)))
setnames(dat_oa, "geo_code", "oa_code")
dat_oa <- na.omit(dat_oa)

### Merge Datasets ###
dat_oa <- merge(dat_pc, dat_oa, by = "oa_code", all = F); rm(dat_pc)

### Sanitize White Space ###
dat_oa <- dat_oa[, postcode := gsub("\\s+", "", postcode)]

### Sort ###
dat_oa <- dat_oa[, .(postcode, tds)]
setkey(dat_oa, postcode)

### Export ###
.dat_oa <- as.data.frame(dat_oa); rm(dat_oa)
save(.dat_oa, file = "R/sysdata.rda", version = 2, compress = "xz"); rm(.dat_oa)
