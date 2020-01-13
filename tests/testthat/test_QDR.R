#===================#
#                   #
#### QDR() TESTS ####
#                   #
#===================#

### Test Functions ###
tQDR <- function(...){QDR(gender = "Female", age = 60, height = 1.83, weight = 90, ...)}
tQDRA <- function(...){QDRA(gender = "Female", age = 60, height = 1.83, weight = 90, ...)}
tQDRB <- function(...){QDRB(gender = "Female", age = 60, height = 1.83, weight = 90, fpg = 4.5, ...)}
tQDRC <- function(...){QDRC(gender = "Female", age = 60, height = 1.83, weight = 90, hba1c = 31.5, ...)}

### Control Sequence Routing ###
expect_identical(tQDR(), tQDRA())
expect_identical(tQDR(fpg = 4.5), tQDRB())
expect_identical(tQDR(hba1c = 31.5), tQDRC())
expect_warning(expect_identical(tQDR(fpg = 4.5, hba1c = 31.5), tQDRB()))

### Tidy Up ###
rm(list = ls(pattern = "^tQDR[A-C]?$"))
