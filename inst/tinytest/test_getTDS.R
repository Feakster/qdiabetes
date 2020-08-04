#======================#
#                      #
#### getTDS() TESTS ####
#                      #
#======================#

# ### Comparator Data ###
dat_oa <- QDiabetes:::.dat_oa

### Input Classes ###
expect_error(
  current = getTDS(),
  pattern = "One or more values must be specified",
  info = "is.null(postcode)"
)
expect_error(
  current = getTDS(postcode = 23),
  pattern = "is\\.character\\(postcode\\) is not TRUE",
  info = "!is.character(postcode)"
)
expect_error(
  current = getTDS(postcode = "OX3 7LF", type = "something"),
  pattern = "type %in% c\\([^\\)]+\\) is not TRUE",
  info = "!{type %in% c('full', 'prefix', 'suffix', 'regex')}"
)
expect_error(
  current = getTDS(postcode = "OX3 7LF", squash = 1),
  pattern = "is\\.logical\\(squash\\) is not TRUE",
  info = "!is.logical(squash)"
)

### Vector Lengths ###
expect_identical(
  current = length(getTDS(c("OX2 6GG", "OX2 6NW", "OX3 7LF"))),
  target = 3L,
  info = "Output length"
)

### Validity of All Database Postcodes ###
suppressWarnings({
  expect_identical(
    current = length(getTDS(dat_oa[["postcode"]])),
    target = nrow(dat_oa),
    info = "Database rows"
  )
})

### Output Type ###
expect_true(
  current = is.double(getTDS(postcode = "OX2 6GG")),
  info = "Output type"
)

### Regex Type Parameter ###
expect_identical(
  current = getTDS(postcode = "^OX3\\d[A-Z]{2}", type = "regex"),
  target = getTDS(postcode = "OX3", type = "prefix"),
  info = "Regex prefix search"
)
expect_identical(
  current = getTDS(postcode = "7LF$", type = "regex"),
  target = getTDS(postcode = "7LF", type = "suffix"),
  info = "Regex suffix search"
)

### Squash Parameter ###
expect_identical(
  current = getTDS(postcode = "OX3 7LF", squash = T),
  target = getTDS(postcode = "OX3 7LF"),
  info = "Squash full postcode"
)
expect_identical(
  current = getTDS(postcode = "OX3", type = "prefix", squash = T),
  target = getTDS(postcode = "OX3", type = "prefix"),
  info = "Squash prefix"
)
expect_identical(
  current = getTDS(postcode = "7LF", type = "suffix", squash = T),
  target = getTDS(postcode = "7LF", type = "suffix"),
  info = "Squash suffix"
)

### Tidy Up ###
rm(dat_oa)
