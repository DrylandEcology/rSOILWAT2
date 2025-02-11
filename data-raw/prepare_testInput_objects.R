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
tests <- 1:6
examples <- paste0("example", tests)

cns <- c(
  "WeatherGenerator",
  "SoilTemp",
  "CO2Effects",
  "TiltedSurface",
  "VegEstab"
)
define_ex <- rbind(
  ex1 = c(FALSE, TRUE, TRUE, FALSE, FALSE),
  ex2 = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  ex3 = c(FALSE, TRUE, TRUE, FALSE, FALSE),
  ex4 = c(FALSE, TRUE, TRUE, FALSE, FALSE),
  ex5 = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  ex6 = c(FALSE, TRUE, TRUE, FALSE, TRUE)
)
colnames(define_ex) <- cns


#--- Inputs ------
dSOILWAT2_inputs <- file.path("tests", "example")
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
compare_objects <- function(new, old, tolerance = 1e-9) {
  # Compare to previous version
  res_cmp <- waldo::compare(old, new, tolerance = tolerance, max_diffs = Inf)

  # Ignore "timestamp"
  has_timestamp_diff <- grepl("timestamp", res_cmp, fixed = TRUE)

  # Ignore "version"
  has_version_diff <- grepl("version", res_cmp, fixed = TRUE)


  list(
    res_waldo = res_cmp,
    resave =
      length(res_cmp) > sum(has_timestamp_diff) + sum(has_version_diff)
  )
}

#' @param classic Logical value.
#'
#' @section Classic vs. non-classic files:
#'   * classic file lines: "value   # comment" where tag matches in the comment
#'   * non-classic file line: "tag value  # comment" where tag matches tag
setTxtInput <- function(filename, tag, value, classic = FALSE) {
  value <- paste(value, collapse = " ")
  # suppress warnings about incomplete final lines
  fin <- suppressWarnings(readLines(filename))
  line <- grep(
    pattern = if (isTRUE(classic)) tag else paste0("^", tag, " "),
    x = fin,
    ignore.case = TRUE
  )
  stopifnot(length(line) == 1L, line > 0L, line <= length(fin))
  posComment <- regexpr("#", text = fin[[line]], fixed = TRUE)
  res <- if (isTRUE(classic)) as.character(value) else paste(tag, value)
  fin[[line]] <- if (posComment > 0L) {
    paste0(
      res,
      strrep(" ", times = max(1, posComment - 1L - nchar(res))),
      substr(fin[[line]], start = posComment, stop = 1e3)
    )
  } else {
    res
  }
  writeLines(fin, con = filename)
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
  ftmp <- file.path(path, "Input", "modelrun.in")
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


toggleVegEstab <- function(path, activate = TRUE) {
  ftemp <- file.path(path, "Input", "estab.in")
  fin <- readLines(ftemp)
  line <- grep("calculate and output establishment", fin, fixed = TRUE)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  substr(fin[line], 1, 1) <- if (activate) "1" else "0"
  writeLines(fin, con = ftemp)

  ftemp <- file.path(path, "Input", "outsetup.in")
  fin <- readLines(ftemp)
  line <- grep("establishment results", fin, fixed = TRUE)
  stopifnot(length(line) == 1, line > 0, line < length(fin))
  fin[line] <- sub(
    pattern = "AVG", # "AVG" is the new SOILWAT2 default example since v8.0.0
    replacement = if (activate) "AVG" else "OFF",
    x = fin[line],
    fixed = TRUE
  )
  writeLines(fin, con = ftemp)
}


setSWRC <- function(
  path, swrc_name = "Campbell1974", ptf_name = "Cosby1984AndOthers"
) {
  fnameSWRCp <- switch(
    EXPR = swrc_name,
    Campbell1974 = "swrc_params.in",
    vanGenuchten1980 = "swrc_params_vanGenuchten1980.in",
    FXW = "swrc_params_FXW.in",
    stop(shQuote(swrc_name), " is not implemented.")
  )

  setTxtInput(
    filename = file.path(path, "files.in"),
    tag = "# Input for soil water retention curve",
    value = file.path("Input", fnameSWRCp),
    classic = TRUE
  )

  setTxtInput(
    filename = file.path(path, "Input", "siteparam.in"),
    tag = "# Specify soil water retention curve",
    value = swrc_name,
    classic = TRUE
  )

  setTxtInput(
    filename = file.path(path, "Input", "siteparam.in"),
    tag = "# Specify pedotransfer function",
    value = ptf_name,
    classic = TRUE
  )

  setTxtInput(
    filename = file.path(path, "Input", "siteparam.in"),
    tag = "# Are SWRC parameters for the mineral soil component",
    value = 1,
    classic = TRUE
  )
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
    from = file.path(dir_extdata, basename(dSOILWAT2_inputs)),
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

  #--- * example6: vegetation establishment ------
  toggleVegEstab(dir_ex, activate = define_ex[it, "VegEstab"])



  #--- Base unit tests on default SOILWAT2 inputs ------

  #---rSOILWAT2 inputs using development version
  sw_input <- rSOILWAT2::sw_inputDataFromFiles(
    dir = dir_ex,
    files.in = "files.in"
  )


  if (it == 1) {
    #--- * Use default SOILWAT2 data as (default) package data ------
    sw_exampleData <- sw_input

    res_cmp <- compare_objects(sw_exampleData, old = rSOILWAT2::sw_exampleData)

    # Save default package data (if different from previous)
    if (res_cmp[["resave"]]) {
      message("Update default package data: 'sw_exampleData'")

      print(res_cmp[["res_waldo"]])

      # nolint start: namespace_linter.
      usethis::use_data(sw_exampleData, internal = FALSE, overwrite = TRUE)
      # nolint end
    }


    #--- * Data of organic matter SWRCp for each SWRC ------
    swrcs <- rSOILWAT2::list_matched_swrcs_ptfs()
    swrcn <- unique(swrcs[["SWRC"]])

    sw2_list_omSWRCp <- lapply(
      swrcn,
      function(swrc_name) {
        dir_tmp <- tempdir()
        dir_tmp_sw <- file.path(dir_tmp, basename(dir_ex))

        file.copy(from = dir_ex, to = dir_tmp, recursive = TRUE)
        on.exit(unlink(dir_tmp_sw, recursive = TRUE))

        setSWRC(
          path = dir_tmp_sw,
          swrc_name = swrc_name,
          ptf_name = swrcs[swrcs[["SWRC"]] %in% swrc_name, "PTF"][[1L]]
        )

        tmpin <- rSOILWAT2::sw_inputDataFromFiles(
          dir = dir_tmp_sw,
          files.in = "files.in"
        )

        tmpin@soils@omSWRCp
      }
    )

    names(sw2_list_omSWRCp) <- swrcn

    res_cmp <- compare_objects(
      sw2_list_omSWRCp, old = rSOILWAT2::sw2_list_omSWRCp
    )

    # Save data for organic matter SWRCp (if different from previous)
    if (res_cmp[["resave"]]) {
      message("Update data of organic matter SWRCp: 'sw2_list_omSWRCp'")

      print(res_cmp[["res_waldo"]])

      # nolint start: namespace_linter.
      usethis::use_data(sw2_list_omSWRCp, internal = FALSE, overwrite = TRUE)
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

  stopifnot(rSOILWAT2::dbW_check_weatherData(sw_weather))


  #--- Compare weather to previous version
  res_cmp <- waldo::compare(
    readRDS(
      file.path(
        dir_backup,
        basename(dir_testdata),
        paste0("Ex", tests[it], "_weather.rds")
      )
    ),
    sw_weather,
    max_diffs = Inf
  )


  #--- Save weather for unit testing (if different from previous)
  if (length(res_cmp) > 0) {
    message("Update weather data (example ", it, ") for tests:")
    print(res_cmp)

    saveRDS(
      object = sw_weather,
      file = file.path(dir_testdata, paste0("Ex", tests[it], "_weather.rds"))
    )
  }


  #--- Compare input to previous version
  set_WeatherHistory(sw_input) <- weatherHistory()

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
    message("Update input data (example ", it, ") for tests:")
    print(res_cmp[["res_waldo"]])

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

    #--- Compare output to previous version
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
      message("Update SOILWAT2 output (example ", it, ") for tests:")
      print(res_cmp[["res_waldo"]])

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

print(paste(
  "NOTE: Copy",
  "'Ex1_input.rds' to 'versioned_swInputData/' as 'Ex1_input_vX.Y.Z.rds'",
  "if significant changes to any class occurred."
))
