#!/usr/bin/env Rscript

# weather data object
dir_definf <- file.path("data-raw", "weatherData")

weatherData <- rSOILWAT2::getWeatherData_folders("data-raw", "weatherData",
  "weath")

devtools::use_data(weatherData, internal = FALSE)
