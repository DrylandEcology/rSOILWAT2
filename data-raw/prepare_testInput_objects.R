#!/usr/bin/env Rscript

# Run this script from the top-level of the source package, e.g.,
# ```
#   cd rSOILWAT2/
#   Rscript data-raw/prepare_testInput_objects.R
# ```

#--- Load development version of rSOILWAT2 ------
# load package "methods" in case this script is run via 'Rscript'
library("methods") # nolint: unused_import_linter.

# these packages are not listed by `rSOILWAT2`:
# nolint start: missing_package_linter.
stopifnot(
  requireNamespace("pkgbuild"),
  requireNamespace("pkgload"),
  requireNamespace("usethis"),
  requireNamespace("waldo")
)
# nolint end

# nolint start: namespace_linter.
pkgbuild::clean_dll()
pkgload::load_all()
# nolint end



#--- Define tests/examples ------
tests <- 1:5
examples <- paste0("example", tests)

cns <- c("WeatherGenerator", "SoilTemp", "CO2Effects", "TiltedSurface")
define_ex <- rbind(
  ex1 = c(FALSE, TRUE, TRUE, FALSE),
  ex2 = c(TRUE, TRUE, TRUE, FALSE),
  ex3 = c(FALSE, TRUE, TRUE, FALSE),
  ex4 = c(FALSE, TRUE, TRUE, FALSE),
  ex5 = c(FALSE, TRUE, TRUE, TRUE)
)
colnames(define_ex) <- cns


#--- Inputs ------
dSOILWAT2_inputs <- "testing"
dir_orig <- file.path("src", "SOILWAT2", dSOILWAT2_inputs)
dir_backup <- "backup"
dir_extdata <- file.path("inst", "extdata")
dir_pkgdata <- "data"
dir_testdata <- file.path("tests", "test_data")

list_backups <- lapply(
  c(dir_extdata, dir_pkgdata, dir_testdata),
  function(x) {
    list(
      orig = x,
      delete_orig = if (identical(x, dir_extdata)) {
        list.files(x, full.names = TRUE)
      }
    )
  }
)





#--- Backup previous version ------
dir.create(dir_backup, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(dir_backup))

for (k in seq_along(list_backups)) {
  message(
    "Create backup of ", shQuote(list_backups[[k]][["orig"]]),
    " at ", shQuote(dir_backup)
  )

  file.copy(
    from = list_backups[[k]][["orig"]],
    to = dir_backup,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  unlink(
    list_backups[[k]][["delete_orig"]],
    recursive = TRUE
  )

  dir.create(list_backups[[k]][["orig"]], showWarnings = FALSE)
  stopifnot(dir.exists(list_backups[[k]][["orig"]]))
}


#------ Helper functions -----
compare_objects <- function(new, old) {
  # Compare to previous version
  res_cmp <- waldo::compare(old, new)

  # Ignore "timestamp"
  has_timestamp_diff <- grepl("timestamp", res_cmp, fixed = TRUE)

  # Ignore difference in version less than minor
  has_version_diff <- rSOILWAT2::check_version(
    new,
    rSOILWAT2::get_version(old),
    level = "minor"
  )

  list(
    res_waldo = res_cmp,
    resave =
      length(res_cmp) > sum(has_timestamp_diff) + sum(has_version_diff)
  )
}

toggleWeatherGenerator <- function(path, activate = FALSE) {
  ftmp <- file.path(path, "Input", "weathsetup.in")
  fin <- readLines(ftmp)
  line <- grep(
    "Activate/deactivate weather generator",
    fin,
    ignore.case = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line + 1], 1, 1) <- if (activate) "1" else "0"
  writeLines(fin, con = ftmp)
}

setPartialWeatherData <- function(path) {
  unlink(
    file.path(path, "Input", "data_weather"),
    recursive = TRUE
  )

  ftmp <- file.path(path, "files.in")
  fin <- readLines(ftmp)
  line <- grep(
    "historical weather data",
    fin,
    ignore.case = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  fin[line] <- sub(
    file.path("Input", "data_weather", "weath"),
    file.path("Input", "data_weather_missing", "weath"),
    x = fin[line]
  )
  writeLines(fin, con = ftmp)
}

toggleSoilTemperature <- function(path, activate = TRUE) {
  ftmp <- file.path(path, "Input", "siteparam.in")
  fin <- readLines(ftmp)
  line <- grep(
    "flag, 1 to calculate soil_temperature",
    fin,
    fixed = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line], 1, 1) <- if (activate) "1" else "0"
  writeLines(fin, con = ftmp)
}

toggleCO2Effects <- function(path, activate = TRUE) {
  ftmp <- file.path(path, "Input", "siteparam.in")
  fin <- readLines(ftmp)
  line <- grep(
    "biomass multiplier",
    fin,
    fixed = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line + 1], 1, 1) <- if (activate) "1" else "0"
  line <- grep(
    "water-usage efficiency multiplier",
    fin,
    fixed = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line + 1], 1, 1) <- if (activate) "1" else "0"
  writeLines(fin, con = ftmp)
}

toggleSurfaceTilt <- function(path, tilt = FALSE, slope = 30, aspect = -45) {
  ftmp <- file.path(path, "Input", "siteparam.in")
  fin <- readLines(ftmp)

  line <- grep("slope (degrees)", fin, fixed = TRUE)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  tmp <- if (tilt) as.character(slope) else "0"
  stopifnot(nchar(tmp) <= 2)
  substr(fin[line], 1, 2) <- paste0(
    tmp,
    rep("\t", max(0, 2 - nchar(tmp))),
    collapse = ""
  )

  line <- grep(
    "aspect = surface azimuth angle (degrees)",
    fin,
    fixed = TRUE
  )
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  tmp <- if (tilt) as.character(aspect) else "NAN"
  stopifnot(nchar(tmp) <= 4)
  substr(fin[line], 1, 4) <- paste0(
    tmp,
    rep("\t", max(0, 4 - nchar(tmp))),
    collapse = ""
  )
  writeLines(fin, con = ftmp)
}


#------- Loop over examples/tests, setup, and create test objects------
for (it in seq_along(tests)) {
  message("\n", examples[it], " ----------------------------------")

  dir_ex <- file.path(dir_extdata, examples[it])

  #--- Create raw example input files from original SOILWAT2 inputs ------
  file.copy(
    from = dir_orig,
    to = dir_extdata,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  file.rename(
    from = file.path(dir_extdata, dSOILWAT2_inputs),
    to = dir_ex
  )



  #--- Modify input files for tests ------
  #--- * example1: default run ------

  #--- * example2: use Markov weather generator ------
  if (define_ex[it, "WeatherGenerator"]) {
    toggleWeatherGenerator(dir_ex, activate = TRUE)
    setPartialWeatherData(dir_ex)
  }

  #--- * example4: turn on CO2-effects ------
  toggleCO2Effects(dir_ex, activate = define_ex[it, "CO2Effects"])

  #--- * example3: use soil temperature ------
  toggleSoilTemperature(dir_ex, activate = define_ex[it, "SoilTemp"])

  #--- * example5: tilted surface ------
  toggleSurfaceTilt(dir_ex, tilt = define_ex[it, "TiltedSurface"])



  #--- Base unit tests on default SOILWAT2 inputs ------

  #---rSOILWAT2 inputs using development version
  sw_input <- rSOILWAT2::sw_inputDataFromFiles(
    dir = dir_ex,
    files.in = "files.in"
  )


  #--- Use default SOILWAT2 data as (default) package data ------
  if (it == 1) {
    sw_exampleData <- sw_input

    res_cmp <- compare_objects(sw_exampleData, old = rSOILWAT2::sw_exampleData)

    # Save default package data (if different from previous)
    if (res_cmp[["resave"]]) {
      print(res_cmp[["waldo_cmp"]])

      message("Update default package data: 'sw_exampleData'")

      # nolint start: namespace_linter.
      usethis::use_data(sw_exampleData, internal = FALSE, overwrite = TRUE)
      # nolint end
    }
  }


  #--- Obtain weather data ------
  sw_weather <- if (define_ex[it, "WeatherGenerator"]) {
    # Deal with weather generator (obtain weather input with missing values)
    toggleWeatherGenerator(dir_ex, activate = FALSE)

    sw_input2 <- rSOILWAT2::sw_inputDataFromFiles(
      dir = dir_ex,
      files.in = "files.in"
    )

    slot(sw_input2, "weatherHistory")

  } else {
    slot(sw_input, "weatherHistory")
  }

  #--- Compare weather to previous version
  res_cmp <- waldo::compare(
    readRDS(
      file.path(
        dir_backup,
        basename(dir_testdata),
        paste0("Ex", tests[it], "_weather.rds")
      )
    ),
    sw_weather
  )


  #--- Save weather for unit testing (if different from previous)
  if (length(res_cmp) > 0) {
    print(res_cmp)

    saveRDS(
      object = sw_weather,
      file = file.path(dir_testdata, paste0("Ex", tests[it], "_weather.rds"))
    )
  }


  #--- Compare input to previous version
  slot(sw_input, "weatherHistory") <- list(new("swWeatherData"))

  res_cmp <- compare_objects(
    sw_input,
    old = readRDS(
      file.path(
        dir_backup,
        basename(dir_testdata),
        paste0("Ex", tests[it], "_input.rds")
      )
    )
  )

  #--- Save input for unit testing (if different from previous)
  if (res_cmp[["resave"]]) {
    print(res_cmp[["waldo_cmp"]])

    saveRDS(
      object = sw_input,
      file = file.path(dir_testdata, paste0("Ex", tests[it], "_input.rds"))
    )
  }


  #--- Run rSOILWAT2 with yearly output and save it as reference output
  if (!rSOILWAT2::swWeather_UseMarkov(sw_input)) {
    rSOILWAT2::swOUT_TimeStepsForEveryKey(sw_input) <- 3

    rdy <- rSOILWAT2::sw_exec(
      inputData = sw_input,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    #--- Compare ouput to previous version
    res_cmp <- compare_objects(
      rdy,
      old = readRDS(
        file.path(
          dir_backup,
          basename(dir_testdata),
          paste0("Ex", tests[it], "_output.rds")
        )
      )
    )

    # Save test output (if different from previous)
    if (res_cmp[["resave"]]) {
      print(res_cmp[["waldo_cmp"]])

      saveRDS(
        object = rdy,
        file = file.path(dir_testdata, paste0("Ex", tests[it], "_output.rds"))
      )
    }
  }


  #--- Only keep default SOILWAT2 files as example input files ------
  if (it != 1) {
    unlink(file.path(dir_ex), recursive = TRUE)
  }
}


message(
  "NOTE: Remove ",
  shQuote(dir_backup),
  " before pushing to repository if script worked well."
)
