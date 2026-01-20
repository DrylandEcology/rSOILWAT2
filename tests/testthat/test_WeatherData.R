
path_example1 <- system.file("extdata", "example1", package = "rSOILWAT2")
dir_weather <- list.files(
  file.path(path_example1, "Input"),
  pattern = "data_weather"
)

test_that("Test data availability", {
  expect_gt(length(dir_weather), 0)
})


test_that("Weather data check", {
  expect_false(dbW_check_weatherData(NA))
  expect_false(dbW_check_weatherData(NULL))
  expect_false(dbW_check_weatherData(1))
  expect_false(dbW_check_weatherData(list()))
  expect_false(dbW_check_weatherData(list(1)))
  expect_false(dbW_check_weatherData(swWeatherData()))
  expect_false(dbW_check_weatherData(weatherHistory()))
  expect_false(dbW_check_weatherData(
    swWeatherData(rSOILWAT2::weatherData[[1]])
  ))

  expect_true(dbW_check_weatherData(rSOILWAT2::weatherData))
  expect_true(dbW_check_weatherData(weatherHistory(rSOILWAT2::weatherData)))
  expect_true(dbW_check_weatherData(
    list(swWeatherData(rSOILWAT2::weatherData[[1]]))
  ))
  expect_true(dbW_check_weatherData(
    get_WeatherHistory(rSOILWAT2::sw_exampleData)
  ))
  expect_true(dbW_check_weatherData(weatherHistory(), check_all = FALSE))

  expect_true(
    validObject_weatherHistory(get_WeatherHistory(rSOILWAT2::sw_exampleData))
  )
  expect_true(validObject_weatherHistory(rSOILWAT2::weatherData))
  expect_true(validObject_weatherHistory(list(swWeatherData())))
  expect_true(validObject_weatherHistory(NULL))
})

test_that("Missing weather data", {
  expect_true(is_missing_weather(NA))
  expect_true(is_missing_weather(NaN))
  expect_true(is_missing_weather(999))

  expect_false(is_missing_weather(0))
  expect_false(is_missing_weather(-5))
  expect_false(is_missing_weather(100))
})

# Set daily input flags for each data source
get_dif <- function(dataset) {
  template_dailyInputFlags <- c(rep(TRUE, 3L), rep(FALSE, 11L))
  switch(
    EXPR = dataset,
    minimalInputs = template_dailyInputFlags,
    daymet = {
      dif <- template_dailyInputFlags
      dif[13L] <- TRUE # ACTUAL_VP
      dif[14L] <- TRUE # SHORT_WR, desc_rsds = 2
      dif
    },
    gridmet = {
      dif <- template_dailyInputFlags
      dif[5L] <- TRUE # WIND_SPEED
      dif[9L] <- TRUE # REL_HUMID_MAX
      dif[10L] <- TRUE # REL_HUMID_MIN
      dif[14L] <- TRUE # SHORT_WR, desc_rsds = 1
      dif
    },
    "maca-type1" = {
      dif <- template_dailyInputFlags
      dif[6L] <- TRUE # WIND_EAST
      dif[7L] <- TRUE # WIND_NORTH
      dif[9L] <- TRUE # REL_HUMID_MAX
      dif[10L] <- TRUE # REL_HUMID_MIN
      dif[14L] <- TRUE # SHORT_WR, desc_rsds = 1
      dif
    },
    "maca-type2" = {
      dif <- template_dailyInputFlags
      dif[6L] <- TRUE # WIND_EAST
      dif[7L] <- TRUE # WIND_NORTH
      dif[11L] <- TRUE # SPEC_HUMID
      dif[14L] <- TRUE # SHORT_WR, desc_rsds = 1
      dif
    }
  )
}

# Set fixes for each data source
get_fwd <- function(dataset) {
  template_fixWeatherData <- rep(FALSE, 3L)
  switch(
    EXPR = dataset,
    minimalInputs = template_fixWeatherData,
    daymet = {
      tmp <- template_fixWeatherData
      tmp[2L:3L] <- TRUE # fix percent, max radiation
      tmp
    },
    gridmet = template_fixWeatherData,
    "maca-type1" = template_fixWeatherData,
    "maca-type2" = {
      tmp <- template_fixWeatherData
      tmp[2L] <- TRUE # fix percent
      tmp
    }
  )
}

get_descRSDS <- function(dataset) {
  switch(
    EXPR = dataset,
    minimalInputs = 0L,
    daymet = 2L, # flux density over the daylight period
    gridmet = 1L, # flux density over 24-hour period
    "maca-type1" = 1L, # flux density over 24-hour period
    "maca-type2" = 1L # flux density over 24-hour period
  )
}

get_useMonthlies <- function(dataset) {
  template_useMonthlies <- rep(TRUE, 3L)
  switch(
    EXPR = dataset,
    minimalInputs = template_useMonthlies,
    daymet = {
      tmp <- template_useMonthlies
      tmp[[1L]] <- FALSE # use radiation instead
      tmp[[3L]] <- FALSE # use vapor pressure instead
      tmp
    },
    gridmet = ,
    "maca-type1" = ,
    "maca-type2" = {
      tmp <- template_useMonthlies
      tmp[] <- FALSE
      tmp
    }
  )
}


test_that("Weather data sources", {
  template_swin <- rSOILWAT2::sw_exampleData

  siteElevation <- swSite_IntrinsicSiteParams(template_swin)[[3L]]

  # see data-raw/prepare_testInput_objects.R
  add_weather_sources <- c(
    "minimalInputs", "daymet", "gridmet", "maca-type1", "maca-type2"
  )


  for (ws in add_weather_sources) {

    ws_dailyInputFlags <- get_dif(ws)
    ws_fixWeatherData <- get_fwd(ws)
    ws_useMonthlies <- get_useMonthlies(ws)

    weatherDirName <- switch(
      EXPR = ws,
      minimalInputs = "data_weather",
      grep(ws, dir_weather, value = TRUE)
    )

    # suppress warnings "relative humidity set to 100%" related
    # to specific humidity
    sww <- list(
      C = suppressWarnings(
        rSOILWAT2::getWeatherData_folders(
          LookupWeatherFolder = file.path(path_example1, "Input"),
          weatherDirName = weatherDirName,
          filebasename = "weath",
          dailyInputFlags = ws_dailyInputFlags,
          correctWeatherValues = ws_fixWeatherData,
          elevation = siteElevation,
          method = "C"
        )
      ),

      R = suppressWarnings(
        rSOILWAT2::getWeatherData_folders(
          LookupWeatherFolder = file.path(path_example1, "Input"),
          weatherDirName = weatherDirName,
          filebasename = "weath",
          dailyInputFlags = ws_dailyInputFlags,
          elevation = siteElevation,
          method = "R"
        )
      )
    )

    expect_true(rSOILWAT2::dbW_check_weatherData(sww[["C"]]))
    expect_true(rSOILWAT2::dbW_check_weatherData(sww[["R"]]))

    years <- rSOILWAT2::get_years_from_weatherData(sww[["C"]])

    expect_identical(
      years,
      rSOILWAT2::get_years_from_weatherData(sww[["R"]])
    )


    calc_difs <- lapply(sww, rSOILWAT2::calc_dailyInputFlags)

    expect_equal(
      ws_dailyInputFlags,
      calc_difs[["R"]],
      ignore_attr = "names"
    )

    expect_true(
      all(
        intersect(which(calc_difs[["C"]]), which(calc_difs[["R"]])) %in%
          which(ws_dailyInputFlags)
      )
    )


    #--- Prepare simulation run with specified weather data
    swin <- template_swin

    swYears_EndYear(swin) <- max(years)
    swYears_StartYear(swin) <- min(years)

    swin@weather@correctWeatherValues <- ws_fixWeatherData
    swin@weather@desc_rsds <- get_descRSDS(ws)
    swin@weather@use_cloudCoverMonthly <- ws_useMonthlies[[1L]]
    swin@weather@use_windSpeedMonthly <- ws_useMonthlies[[2L]]
    swin@weather@use_humidityMonthly <- ws_useMonthlies[[3L]]

    #--- Run and check simulation with specified weather data
    rd <- list()

    for (method in c("C", "R")) {
      swin@weather@dailyInputFlags <- calc_difs[[method]]

      rd[[method]] <- rSOILWAT2::sw_exec(
        inputData = swin,
        weatherList = sww[[method]],
        echo = FALSE,
        quiet = TRUE
      )

      expect_true(rSOILWAT2::check_version(rd[[method]], level = "minor"))
      expect_s4_class(rd[[method]], "swOutput")
      expect_false(rSOILWAT2::has_soilTemp_failed())
      expect_true(all(rSOILWAT2::sw_out_flags() %in% slotNames(rd[[method]])))
    }


    #--- Expect identical simulation output independent of reading method
    vars <- grep(
      pattern = "timestamp",
      x = slotNames(rd[["C"]]),
      value = TRUE,
      invert = TRUE,
      fixed = TRUE
    )

    for (var in vars) {
      expect_identical(slot(rd[["C"]], var), slot(rd[["R"]], var))
    }
  }
})


test_that("Weather data object conversions", {
  # nolint start: commented_code_linter.
  #--- * Test backwards compatible behavior ------
  # see \url{https://github.com/DrylandEcology/rSOILWAT2/issues/236}:
  # "dbW_dataframe_to_weatherData() is not backwards compatible with v6.0.0"
  # nolint end: commented_code_linter.

  set.seed(54)
  N <- 365
  doys <- seq_len(N)
  tmean <- -5 + 20 * sinpi(doys / N) + rnorm(n = N, sd = 2)

  tmp_meteo <- cbind(
    Year = rep(2019, times = N),
    DOY = doys,
    Tmax_C = tmean + 4 + rnorm(n = N, sd = 1),
    Tmin_C = tmean - 4 + rnorm(n = N, sd = 1),
    PPT_cm = 0.1 * rgamma(n = N, shape = 1, rate = 1)
  )


  expect_true(
    rSOILWAT2::dbW_check_weatherData(
      rSOILWAT2::dbW_dataframe_to_weatherData(
        weatherDF = tmp_meteo,
        weatherDF_dataColumns = c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      )
    )
  )


  expect_true(
    rSOILWAT2::dbW_check_weatherData(
      rSOILWAT2::dbW_dataframe_to_weatherData(
        weatherDF = tmp_meteo
      )
    )
  )



  #--- * Test conversion round trip ------
  # swWeatherHistory -> data frame -> swWeatherHistory

  res <- rSOILWAT2::dbW_dataframe_to_weatherData(
    weatherDF = rSOILWAT2::dbW_weatherData_to_dataframe(
      rSOILWAT2::weatherData
    )
  )

  expect_true(rSOILWAT2::dbW_check_weatherData(res))

  expect_identical(res, rSOILWAT2::weatherData)

})




test_that("Weather data substitution", {
  # Load example data
  path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
  dif <- c(rep(TRUE, 3L), rep(FALSE, 11L))
  dif[13L] <- TRUE # ACTUAL_VP
  dif[14L] <- TRUE # SHORT_WR, desc_rsds = 2
  wdata <- rSOILWAT2::getWeatherData_folders(
    LookupWeatherFolder = file.path(path_example1, "Input"),
    weatherDirName = grep(
      "data_weather_daymet",
      x = dir_weather,
      value = TRUE,
      fixed = TRUE
    ),
    filebasename = "weath",
    startYear = 1980,
    endYear = 1981,
    dailyInputFlags = dif
  )

  # Prepare example data
  x0 <- x <- dbW_weatherData_to_dataframe(wdata)
  dif0 <- calc_dailyInputFlags(x0)

  # Set June-August of 1980 as missing
  ids_1980 <- x[, "Year"] == 1980
  ids_missing <- ids_1980 & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
  x[ids_missing, -(1:2)] <- NA

  # Test: substitute missing values of all variables
  expect_identical(
    dbW_substituteWeather(x, x0[ids_1980, ], return_weatherDF = TRUE),
    x0
  )

  # Test: substitute missing values of some variables
  var_test <- "shortWR"
  expect_identical(
    dbW_substituteWeather(
      x,
      subData = x0[ids_1980, ],
      vars_substitute = var_test,
      return_weatherDF = TRUE
    )[, var_test],
    x0[, var_test]
  )

  # Test: substitute missing values if only some variables are available
  vars_has <- c("Year", "DOY", "Tmax_C", "shortWR")
  expect_identical(
    dbW_substituteWeather(
      x,
      subData = x0[ids_1980, vars_has],
      return_weatherDF = TRUE
    )[, vars_has],
    x0[, vars_has]
  )

  expect_warning(
    dbW_substituteWeather(
      x,
      subData = x0[ids_1980, vars_has],
      vars_substitute = weather_dataColumns(),
      return_weatherDF = TRUE
    ),
    regexp = "Not all requested variables present"
  )

  # Test: match rows if "by" variables differ
  expect_identical(
    dbW_substituteWeather(
      x,
      subData = data.frame(
        annus = x0[, "Year"],
        dies = x0[, "DOY"],
        x0[, setdiff(colnames(x0), c("Year", "DOY"))]
      )[ids_1980, ],
      by_weatherData = c("Year", "DOY"),
      by_subData = c("annus", "dies"),
      return_weatherDF = TRUE
    ),
    x0
  )

})


test_that("Weather data fixing", {
  x0 <- as.data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
  dif <- calc_dailyInputFlags(x0)
  vars <- names(dif)[dif]


  #--- * Check no change to no missing values ------
  xf <- dbW_fixWeather(x0, return_weatherDF = TRUE)
  expect_identical(xf[["weatherData"]], x0)
  expect_true(all(is.na(xf[["meta"]])))


  #--- * Check interpolation and substitution ------
  x1 <- x0

  # * Expect short missing spell to interpolate (except precipitation)
  # Set May 23-24 of 1981 as missing
  tmp <- x1[, "Year"] == 1981
  ids_to_interp <- tmp & x1[, "DOY"] >= 144 & x1[, "DOY"] <= 145
  x1[ids_to_interp, -(1:2)] <- NA

  # * Expect long missing spell to substitute
  # Set June-August of 1980 as missing
  tmp <- x1[, "Year"] == 1980
  ids_to_sub <- tmp & x1[, "DOY"] >= 153 & x1[, "DOY"] <= 244
  x1[ids_to_sub, -(1:2)] <- NA


  xf <- dbW_fixWeather(
    weatherData = x1,
    subData = x0,
    new_endYear = max(x1[["Year"]]) + 1L, # expect long term daily mean
    nmax_interp = 5L,
    return_weatherDF = TRUE
  )

  expect_false(anyNA(xf[["weatherData"]][, vars]))
  expect_all_true(xf[["weatherData"]][["PPT_cm"]] >= 0)

  ids_has <- seq_len(nrow(x0))
  expect_identical(
    xf[["weatherData"]][ids_has[!ids_to_interp], vars],
    x0[!ids_to_interp, vars]
  )

  tmpc <- as.list(table(xf[["meta"]]))
  expect_identical(
    tmpc[["interpolateLinear (<= 5 days)"]],
    sum(ids_to_interp) * length(setdiff(vars, "PPT_cm"))
  )
  expect_identical(
    tmpc[["fixedValue"]],
    sum(ids_to_interp) # precipitation
  )
  expect_identical(
    tmpc[["substituteData"]],
    sum(ids_to_sub) * length(vars)
  )
  expect_identical(
    tmpc[["longTermDailyMean"]],
    365L * length(vars)
  )


  #--- * Check switched min/max daily temperature ------
  x2 <- x0

  #--- Weather with switched min/max daily temperature
  ids1 <- x2[, "Year"] == 1949 & x2[, "DOY"] >= 144 & x2[, "DOY"] <= 145
  x2[ids1, c("Tmin_C", "Tmax_C")] <- x2[ids1, c("Tmax_C", "Tmin_C")]
  Ns1 <- sum(ids1)

  # Sort
  xf <- dbW_fixWeather(x2, correctWeatherValues = TRUE, return_weatherDF = TRUE)

  # Expect correct values
  expect_identical(xf[["weatherData"]], as.data.frame(x0))

  tmpc <- as.list(table(xf[["meta"]]))
  expect_identical(tmpc[["correctedValue"]], expected = 2L * Ns1)


  #--- * Check substitute and min/max daily temperature ------
  x0s <- x0

  #--- Weather with switched min/max daily temperature
  ids2 <- which(ids_to_sub)[1L:2L]
  x0s[ids2, c("Tmin_C", "Tmax_C")] <- x0s[ids2, c("Tmax_C", "Tmin_C")]
  x1[ids2, c("Tmin_C", "Tmax_C")] <- x0[ids2, c("Tmax_C", "Tmin_C")]
  Ns2 <- length(ids2)

  # Fix and sort
  xf <- dbW_fixWeather(
    weatherData = x1,
    subData = x0,
    new_endYear = max(x1[["Year"]]) + 1L, # expect long term daily mean
    nmax_interp = 5L,
    correctWeatherValues = TRUE,
    return_weatherDF = TRUE
  )

  expect_false(anyNA(xf[["weatherData"]][, vars]))

  ids_has <- seq_len(nrow(x0))
  expect_identical(
    xf[["weatherData"]][ids_has[!ids_to_interp], vars],
    x0[!ids_to_interp, vars]
  )

  tmpc <- as.list(table(xf[["meta"]]))
  expect_identical(
    tmpc[["interpolateLinear (<= 5 days)"]],
    sum(ids_to_interp) * length(setdiff(vars, "PPT_cm"))
  )
  expect_identical(
    tmpc[["fixedValue"]],
    sum(ids_to_interp) # precipitation
  )
  expect_identical(
    tmpc[["substituteData"]],
    sum(ids_to_sub) * length(vars) - 2L * Ns2
  )
  expect_identical(
    tmpc[["correctedValue"]],
    2L * Ns2
  )
})

test_that("Weather data fixing without negative values", {
  skip_if_offline()

  #--- * Issue #258: negative precipitation ------
  mm_dm <- try(
    rSOILWAT2::sw_meteo_obtain_DayMet(
      x = c(longitude = -114.1204, latitude = 38.35762),
      start_year = 1990,
      end_year = 2020
    )
  )

  if (!is.null(mm_dm) && !inherits(mm_dm, "try-error")) {
    wd <- rSOILWAT2::dbW_fixWeather(
      mm_dm[["weatherDF"]],
      return_weatherDF = TRUE
    )

    expect_all_true(wd[["weatherData"]][["PPT_cm"]] >= 0)
  } else {
    skip("sw_meteo_obtain_DayMet() failed.")
  }

  #--- * negative shortwave radiation ------
  mm_dm <- try(
    rSOILWAT2::sw_meteo_obtain_DayMet(
      x = c(longitude = -111.8952, latitude = 44.07488),
      start_year = 1990,
      end_year = 2020
    )
  )

  if (!is.null(mm_dm) && !inherits(mm_dm, "try-error")) {
    wd <- rSOILWAT2::dbW_fixWeather(
      mm_dm[["weatherDF"]],
      squashToBounds = TRUE,
      return_weatherDF = TRUE
    )

    expect_all_true(wd[["weatherData"]][["shortWR"]] >= 0)
  } else {
    skip("sw_meteo_obtain_DayMet() failed.")
  }

})


test_that("Weather data scaling", {
  swin <- rSOILWAT2::sw_exampleData

  ref <- sw_exec(swin)

  scalingParams <- setdiff(
    colnames(swWeather_MonScalingParams(swin)),
    "ShortWR" # ShortWR only works if we provide daily radiation as input
  )

  for (k in seq_along(scalingParams)) {
    x <- swin
    swWeather_MonScalingParams(x)[, scalingParams[[k]]] <- 2
    res <- sw_exec(x)

    # Expect that the scaled simulation produces different SWC than the default
    expect_false(isTRUE(all.equal(res@SWCBULK@Day, ref@SWCBULK@Day)))
  }
})
