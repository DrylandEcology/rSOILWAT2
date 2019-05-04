#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
library("methods")  # in case this code is run via 'Rscript'
stopifnot(requireNamespace("pkgbuild"))
stopifnot(requireNamespace("pkgload"))
stopifnot(requireNamespace("usethis"))

pkgbuild::clean_dll()
pkgload::load_all()


# weather data object
dir_definf <- file.path("data-raw", "weatherData")

weatherData <- rSOILWAT2::getWeatherData_folders("data-raw", "weatherData",
  "weath")

usethis::use_data(weatherData, internal = FALSE)
