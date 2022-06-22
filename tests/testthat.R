library("testthat")
library("rSOILWAT2") # nolint: unused_import_linter

test_check("rSOILWAT2", reporter = SummaryReporter, encoding = "UTF-8")
