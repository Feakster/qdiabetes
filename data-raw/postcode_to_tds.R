library(data.table)
setDTthreads(6L)

# https://data.gov.uk/dataset/9b090605-9861-4bb4-9fa4-6845daa2de9b/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-february-2018-lookup-in-the-uk
# http://geoportal1-ons.opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0
url_pc <- "http://geoportal1-ons.opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0.csv"
dt_pc <- fread(url_pc, sep = ",",
               header = T,
               na.strings = "",
               select = c("pcds", "oa11", "lsoa11cd", "msoa11cd", "lsoa11nm", "msoa11nm"),
               colClasses = "character",
               blank.lines.skip = T); rm(url_pc)
setnames(dt_pc,
         c("pcds", "oa11", "lsoa11cd", "msoa11cd", "lsoa11nm", "msoa11nm"),
         c("postcode", "oa_code", "lsoa_code", "msoa_code", "lsoa_name", "msoa_name"))

# https://www.statistics.digitalresources.jisc.ac.uk/dataset/2011-uk-townsend-deprivation-scores
url_oa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1195/download"

dt_oa <- fread(url_oa,
               sep = ",",
               header = T,
               na.strings = "",
               select = c("geo_code", "TDS"),
               blank.lines.skip = T); rm(url_oa)
setnames(dt_oa, tolower(names(dt_oa)))
setnames(dt_oa, "geo_code", "oa_code")

url_lsoa <- "https://www.statistics.digitalresources.jisc.ac.uk/node/1194/download"
dt_lsoa <- fread(url_lsoa,
                 sep = ",",
                 header = T,
                 na.strings = "",
                 select = c("GEO_CODE", "TDS"),
                 blank.lines.skip = T); rm(url_lsoa)
setnames(dt_lsoa, tolower(names(dt_lsoa)))
setnames(dt_lsoa, "geo_code", "lsoa_code")

dt_lsoa <- merge(dt_pc, dt_lsoa[, .(lsoa_code, lsoa_name, tds)], by = "lsoa_code", all = T)

geocode <- ptl[pcds %like% "^OX2\\s+6PX$", lsoa11cd]
tds <- ltt[GEO_CODE == geocode, TDS]
tds
