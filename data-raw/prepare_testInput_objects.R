#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
library("methods")  # in case this code is run via 'Rscript'
stopifnot(requireNamespace("pkgbuild"))
stopifnot(requireNamespace("pkgload"))
stopifnot(requireNamespace("usethis"))

pkgbuild::clean_dll()
pkgload::load_all()


#--- INPUTS
dSOILWAT2_inputs <- "testing"
dir_orig <- file.path("src", "SOILWAT2", dSOILWAT2_inputs)
dir_in <- file.path("inst", "extdata")
dir_backup <- sub("extdata", "extdata_copy", dir_in)
dir_out <- file.path("tests", "test_data")

tests <- 1:4
examples <- paste0("example", tests)


#-----------------------
#--- BACKUP PREVIOUS FILES
print(paste("Create backup of 'inst/extdata/' as", shQuote(dir_backup)))
dir.create(dir_backup, showWarnings = FALSE)
stopifnot(dir.exists(dir_backup))
file.copy(from = dir_in, to = dir_backup, recursive = TRUE, copy.mode = TRUE,
  copy.date = TRUE)

unlink(dir_in, recursive = TRUE)
dir.create(dir_in, showWarnings = FALSE)
stopifnot(dir.exists(dir_in))


#-----------------------
#--- COPY AND CREATE EXTDATA EXAMPLES FROM ORIGINAL SOILWAT2 INPUTS
for (it in seq_along(tests)) {
  file.copy(from = dir_orig, to = dir_in, recursive = TRUE, copy.mode = TRUE,
    copy.date = TRUE)
  file.rename(from = file.path(dir_in, dSOILWAT2_inputs), to = file.path(dir_in,
    examples[it]))
}

# example1: default run
  # nothing to do

# example2: use Markov weather generator
  # Turn on weather generator
  ftemp <- file.path(dir_in, examples[2], "Input", "weathsetup.in")
  fin <- readLines(ftemp)
  line <- grep("Markov process for missing weather", fin, ignore.case = TRUE)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line], 1, 1) <- "1"
  writeLines(fin, con = ftemp)

  # Use partial weather data
  unlink(file.path(dir_in, examples[2], "Input", "data_weather"),
    recursive = TRUE)

  ftemp <- file.path(dir_in, examples[2], "files.in")
  fin <- readLines(ftemp)
  line <- grep("data file containing historical weather", fin,
    ignore.case = TRUE)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  fin[line] <- sub("Input/data_weather/weath",
    "Input/data_weather_missing/weath", x = fin[line])
  writeLines(fin, con = ftemp)


# example3: use soil temperature
  ftemp <- file.path(dir_in, examples[3], "Input", "siteparam.in")
  fin <- readLines(ftemp)
  line <- grep("flag, 1 to calculate soil_temperature", fin)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line], 1, 1) <- "1"
  writeLines(fin, con = ftemp)

# example4: turn on CO2-effects
  ftemp <- file.path(dir_in, examples[4], "Input", "siteparam.in")
  fin <- readLines(ftemp)
  line <- grep("Use biomass multiplier", fin)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line + 1], 1, 1) <- "1"
  line <- grep("Use water-usage efficiency multiplier", fin)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line + 1], 1, 1) <- "1"
  writeLines(fin, con = ftemp)



#-----------------------
#--- USE EXTDATA EXAMPLES AS BASIS FOR UNIT-TESTS
for (it in seq_along(tests)) {
  #---rSOILWAT2 inputs using development version
  sw_input <- sw_inputDataFromFiles(file.path(dir_in, examples[it]),
    files.in = "files.in")

  sw_weather <- slot(sw_input, "weatherHistory")
  slot(sw_input, "weatherHistory") <- list(new("swWeatherData"))

  #---Files for unit testing
  saveRDS(sw_weather, file = file.path(dir_out,
    paste0("Ex", tests[it], "_weather.rds")))
  saveRDS(sw_input, file = file.path(dir_out,
    paste0("Ex", tests[it], "_input.rds")))
}


#-----------------------
#--- USE DEFAULT EXTDATA EXAMPLE AS PACKAGE DATA
sw_exampleData <- sw_inputDataFromFiles(file.path(dir_in, examples[1]),
  files.in = "files.in")
usethis::use_data(sw_exampleData, internal = FALSE, overwrite = TRUE)


#-----------------------
#--- DELETE ALL BUT DEFAULT EXAMPLE FROM PACKAGE (to minimize space)
for (it in seq_along(tests)[-1]) {
  unlink(file.path(dir_in, examples[it]), recursive = TRUE)
}

#-----------------------
print(paste("NOTE: Remove", shQuote(dir_backup), "before pushing to",
  "repository if script worked well."))
