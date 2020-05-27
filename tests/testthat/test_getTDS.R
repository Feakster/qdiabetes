#======================#
#                      #
#### getTDS() TESTS ####
#                      #
#======================#

# ### Comparator Data ###
dat_oa <- QDiabetes:::.dat_oa

# pc_known <- c("OX26GG", "OX26NW", "OX37LF", "OX26PN", "OX14AR")
# tds <- dat_oa[match(pc_known, dat_oa$postcode), "tds"]
# names(tds) <- dat_oa[match(pc_known, dat_oa$postcode), "postcode"]
# 
# pc_full <- gsub("(\\w{3})$", " \\1", pc_known)
# pc_pre <- gsub("\\w{3}$", "", pc_known)
# m <- regexpr("\\w{3}$", pc_known)
# pc_suff <- regmatches(pc_known, m); rm(m)

### Input Classes ###
expect_error(getTDS(),
             regexp = "One or more values must be specified",
             label = "is.null(postcode)")
expect_error(getTDS(postcode = 23),
             regexp = "is\\.character\\(postcode\\) is not TRUE",
             label = "!is.character(postcode)")
expect_error(getTDS(postcode = "OX3 7LF", type = "something"),
             regexp = "type %in% c\\([^\\)]+\\) is not TRUE",
             label = "!{type %in% c('full', 'prefix', 'suffix', 'regex')}")
expect_error(getTDS(postcode = "OX3 7LF", squash = 1),
             regexp = "is\\.logical\\(squash\\) is not TRUE",
             label = "!is.logical(squash)")

### Vector Lengths ###
expect_length(getTDS(c("OX2 6GG", "OX2 6NW", "OX3 7LF")), 3)

### Validity of All Database Postcodes ###
suppressWarnings({
  expect_equal(length(getTDS(dat_oa$postcode)), nrow(dat_oa),
               label = "Database Matches",
               expected.label = "Database Rows")
})

### Output Type ###
expect_type(getTDS(postcode = "OX2 6GG"), "double")

### Regex Type Parameter ###
expect_equal(getTDS(postcode = "^OX3\\d[A-Z]{2}", type = "regex"),
             getTDS(postcode = "OX3", type = "prefix"),
             label = "regex search",
             expected.label = "prefix search")
expect_equal(getTDS(postcode = "7LF$", type = "regex"),
             getTDS(postcode = "7LF", type = "suffix"),
             label = "regex search",
             expected.label = "suffix search")

### Squash Parameter ###
expect_equal(getTDS(postcode = "OX3 7LF", squash = T), getTDS(postcode = "OX3 7LF"))
expect_equal(getTDS(postcode = "OX3", type = "prefix", squash = T), getTDS(postcode = "OX3", type = "prefix"))
expect_equal(getTDS(postcode = "7LF", type = "suffix", squash = T), getTDS(postcode = "7LF", type = "suffix"))

### Tidy Up ###
rm(dat_oa)
