#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
library("methods")  # in case this code is run via 'Rscript'
library("devtools")
library("testthat")

devtools::clean_dll()
devtools::load_all()

testthat::test_file("tests/testthat/test_soil_temperature_fails.R")
