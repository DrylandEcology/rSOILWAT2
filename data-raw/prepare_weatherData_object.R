#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
# load package "methods" in case this script is run via 'Rscript'
library("methods") # nolint: unused_import_linter.
stopifnot(
  requireNamespace("pkgbuild"),
  requireNamespace("pkgload"),
  requireNamespace("usethis")
)

pkgbuild::clean_dll()
pkgload::load_all()


# weather data object
dir_definf <- file.path("data-raw", "weatherData")

weatherData <- rSOILWAT2::getWeatherData_folders("data-raw", "weatherData",
  "weath")

usethis::use_data(weatherData, internal = FALSE)
