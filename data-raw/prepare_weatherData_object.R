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


# weather data object
dir_definf <- file.path("data-raw", "weatherData")

weatherData <- rSOILWAT2::getWeatherData_folders(
  LookupWeatherFolder = "data-raw",
  weatherDirName = "weatherData",
  filebasename = "weath"
)

stopifnot(rSOILWAT2::dbW_check_weatherData(weatherData))



#--- Compare weather to previous version
prev_weatherData <- rSOILWAT2::weatherData

res_cmp <- waldo::compare(weatherData, prev_weatherData, max_diffs = Inf)


#--- Save weather (if different from previous)
if (length(res_cmp) > 0) {
  message("Update package example weather data:")
  print(res_cmp)

  # nolint start: namespace_linter.
  usethis::use_data(weatherData, internal = FALSE, overwrite = TRUE)
  # nolint end: namespace_linter.
}
