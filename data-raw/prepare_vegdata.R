#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
library("methods")  # in case this code is run via 'Rscript'
stopifnot(requireNamespace("pkgbuild"))
stopifnot(requireNamespace("pkgload"))
stopifnot(requireNamespace("usethis"))

pkgbuild::clean_dll()
pkgload::load_all()


dir_definf <- file.path("data-raw", "vegdata")


#--- Vegetation biomass
sw2_tr_VegBiom <- utils::read.csv(
  file = file.path(dir_definf, "Vegetation_MeanMonthly_v5.csv"),
  skip = 1,
  row.names = 1,
  stringsAsFactors = FALSE
)

usethis::use_data(sw2_tr_VegBiom, internal = FALSE, overwrite = TRUE)
