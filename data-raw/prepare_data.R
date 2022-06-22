#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
# load package "methods" in case this script is run via 'Rscript'
library("methods") # nolint: unused_import_linter.
# these packages are not listed by `rSOILWAT2`:
stopifnot(
  requireNamespace("pkgbuild"), # nolint: missing_package_linter.
  requireNamespace("pkgload"),  # nolint: missing_package_linter.
  requireNamespace("usethis") # nolint: missing_package_linter.
)

pkgbuild::clean_dll() # nolint: namespace_linter.
pkgload::load_all() # nolint: namespace_linter.


dir_definf <- file.path("data-raw", "data")


#--- Vegetation biomass
sw2_tr_VegBiom <- utils::read.csv(
  file = file.path(dir_definf, "Vegetation_MeanMonthly_v5.csv"),
  skip = 1,
  row.names = 1,
  stringsAsFactors = FALSE
)

# nolint start: namespace_linter.
usethis::use_data(sw2_tr_VegBiom, internal = FALSE, overwrite = TRUE)
# nolint end

#--- Vegetation rooting profile
tmp_trco_desc <- utils::read.csv(
  file = file.path(dir_definf, "TranspirationCoefs_v2.csv"),
  nrows = 1,
  stringsAsFactors = FALSE
)

tmp_trco_data <- utils::read.csv(
  file = file.path(dir_definf, "TranspirationCoefs_v2.csv"),
  skip = 2,
  stringsAsFactors = FALSE
)
colnames(tmp_trco_data) <- colnames(tmp_trco_desc)

sw2_trco_table <- list(
  desc = tmp_trco_desc,
  data = tmp_trco_data
)

# nolint start: namespace_linter.
usethis::use_data(sw2_trco_table, internal = FALSE, overwrite = TRUE)
# nolint end



#--- Yearly atmospheric CO2 concentration values
sw2_tr_CO2a <- utils::read.csv(
  file = file.path(dir_definf, "AtmosCO2.csv"),
  stringsAsFactors = FALSE
)

# nolint start: namespace_linter.
usethis::use_data(sw2_tr_CO2a, internal = FALSE, overwrite = TRUE)
# nolint end
