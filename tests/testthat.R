library("testthat")
library("rSOILWAT2")

test_check("rSOILWAT2", reporter = SummaryReporter, encoding = "UTF-8")
