
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
})

test_that("Missing weather data", {
  expect_true(is_missing_weather(NA))
  expect_true(is_missing_weather(NaN))
  expect_true(is_missing_weather(999))

  expect_false(is_missing_weather(0))
  expect_false(is_missing_weather(-5))
  expect_false(is_missing_weather(100))
})


test_that("Weather data sources", {
  template_swin <- rSOILWAT2::sw_exampleData

  # see data-raw/prepare_testInput_objects.R
  add_weather_sources <- c("minimalInputs", "daymet", "gridmet", "maca")
  template_dailyInputFlags <- c(rep(TRUE, 3L), rep(FALSE, 11L))

  for (ws in add_weather_sources) {
    ws_dailyInputFlags <- switch(
      EXPR = ws,
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
      maca = {
        dif <- template_dailyInputFlags
        dif[6L] <- TRUE # WIND_EAST
        dif[7L] <- TRUE # WIND_NORTH
        dif[9L] <- TRUE # REL_HUMID_MAX
        dif[10L] <- TRUE # REL_HUMID_MIN
        dif[14L] <- TRUE # SHORT_WR, desc_rsds = 1
        dif
      }
    )

    weatherDirName <- switch(
      EXPR = ws,
      minimalInputs = "data_weather",
      grep(ws, dir_weather, value = TRUE)
    )

    sww <- list(
      C = rSOILWAT2::getWeatherData_folders(
        LookupWeatherFolder = file.path(path_example1, "Input"),
        weatherDirName = weatherDirName,
        filebasename = "weath",
        dailyInputFlags = ws_dailyInputFlags,
        method = "C"
      ),

      R = rSOILWAT2::getWeatherData_folders(
        LookupWeatherFolder = file.path(path_example1, "Input"),
        weatherDirName = weatherDirName,
        filebasename = "weath",
        dailyInputFlags = ws_dailyInputFlags,
        method = "R"
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

    if (ws == "minimalInputs") {
      swin@weather@desc_rsds <- 0L
      swin@weather@use_cloudCoverMonthly <- TRUE
      swin@weather@use_windSpeedMonthly <- TRUE
      swin@weather@use_humidityMonthly <- TRUE

    } else if (ws == "daymet") {
      swin@weather@desc_rsds <- 2L # flux density over the daylight period
      swin@weather@use_cloudCoverMonthly <- FALSE # use radiation instead
      swin@weather@use_windSpeedMonthly <- TRUE
      swin@weather@use_humidityMonthly <- FALSE # use vapor pressure instead

    } else if (ws == "gridmet") {
      swin@weather@desc_rsds <- 1L # flux density over 24-hour period
      swin@weather@use_cloudCoverMonthly <- FALSE # use radiation instead
      swin@weather@use_windSpeedMonthly <- FALSE # has daily wind
      swin@weather@use_humidityMonthly <- FALSE # has humidity

    } else if (ws == "maca") {
      swin@weather@desc_rsds <- 1L # flux density over 24-hour period
      swin@weather@use_cloudCoverMonthly <- FALSE # use radiation instead
      swin@weather@use_windSpeedMonthly <- FALSE # has daily wind
      swin@weather@use_humidityMonthly <- FALSE # has humidity

    }


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
  #--- * Test backwards compatible behavior ------
  # see \url{https://github.com/DrylandEcology/rSOILWAT2/issues/236}:
  # "dbW_dataframe_to_weatherData() is not backwards compatible with v6.0.0"

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
    ),
    round = FALSE
  )

  expect_true(rSOILWAT2::dbW_check_weatherData(res))

  expect_identical(res, rSOILWAT2::weatherData)

})
