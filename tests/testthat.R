library(testthat)
library(devtools)
#library(cliR)
#devtools::load_all()

#test_check("cliR")
devtools::test(reporter = "progress")
