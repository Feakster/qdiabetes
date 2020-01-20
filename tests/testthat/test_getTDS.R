#======================#
#                      #
#### getTDS() TESTS ####
#                      #
#======================#

### Input Classes ###
expect_error(getTDS(), regexp = "One or more values must be specified", label = "is.null(postcode)")
expect_error(getTDS(23), label = "!is.character(postcode)")

### Vector Lengths ###
expect_length(getTDS(c("OX2 6GG", "OX2 6NW", "OX3 7LF")), 3)

### Validity of All Database Postcodes ###
expect_equal(length(getTDS(dat_oa$postcode)), nrow(dat_oa), label = "Database Matches", expected.label = "Database Rows")

### Output Type ###
expect_type(getTDS("OX2 6GG"), "double")
