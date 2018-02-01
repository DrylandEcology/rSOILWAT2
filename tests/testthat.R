# Set R_TESTS to empty string because tests work with devtools::test() and error out with
# devtools::check(). See https://github.com/hadley/testthat/issues/86 and
# https://github.com/hadley/testthat/issues/144
# Remove following line when that issue in R is fixed.
Sys.setenv("R_TESTS" = "")

library("testthat")
library("rSOILWAT2")

test_check("rSOILWAT2", reporter = SummaryReporter)
