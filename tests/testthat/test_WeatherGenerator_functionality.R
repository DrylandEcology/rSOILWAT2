context("rSOILWAT2 weather generator")

#---INPUTS
path_extdata <- file.path("..", "..", "inst", "extdata")
if (!dir.exists(path_extdata)) {
  path_extdata <- system.file("extdata", package = "rSOILWAT2")
}


dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", expect_gt(length(tests), 0))

sw_weather <- lapply(
  tests,
  function(it) readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))
)


#---TESTS


test_that("Weather generator: estimate input parameters", {
  for (k in seq_along(tests)) {
    test_dat <- sw_weather[[k]]
    test_df <- data.frame(dbW_weatherData_to_dataframe(test_dat, valNA = NULL))

    if (anyNA(test_df)) {
      expect_warning(res <- dbW_estimate_WGen_coefs(test_df,
        propagate_NAs = TRUE),
        "Insufficient weather data to estimate values")

      expect_message(res <- dbW_estimate_WGen_coefs(test_df,
        propagate_NAs = FALSE,
        imputation_type = "mean"),
        "Impute missing")

    } else {
      res <- dbW_estimate_WGen_coefs(test_df)
    }

    # Check that generated weather generator inputs pass requirements
    # no NAs:
    expect_false(anyNA(res[["mkv_doy"]]))
    expect_false(anyNA(res[["mkv_woy"]]))

    # validity tests ok
    sw_in <- rSOILWAT2::sw_exampleData
    expect_equal(swMarkov_Prob(sw_in) <- res[["mkv_doy"]], res[["mkv_doy"]])
    expect_equal(swMarkov_Conv(sw_in) <- res[["mkv_woy"]], res[["mkv_woy"]])
  }
})


test_that("Weather generator: generate weather", {
  for (k in seq_along(tests)) {
    test_dat <- sw_weather[[k]]
    years <- get_years_from_weatherData(test_dat)
    n <- length(test_dat)

    wout <- list()

    # Case 1: generate weather for dataset and impute missing values
    wout[[1]] <- dbW_generateWeather(test_dat,
      imputation_type = "mean",
      imputation_span = 5)

    # Case 2: generate weather based on partial dataset,
    #   use estimated weather generator coefficients from full dataset
    wgen_coeffs <- dbW_estimate_WGen_coefs(test_dat,
      imputation_type = "mean",
      imputation_span = 5)
    wout[[2]] <- dbW_generateWeather(test_dat[(n - 5):n],
      years = years[length(years)] + 0:10 - 5,
      wgen_coeffs = wgen_coeffs)

    # Case 3: generate weather based only on estimated weather generator
    #   coefficients from full dataset
    x_empty <- list(new("swWeatherData"))
    wout[[3]] <- dbW_generateWeather(x_empty,
      years = years[length(years)] + 30:40,
      wgen_coeffs = wgen_coeffs)

    # Expectations
    for (k in seq_along(wout)) {
      x <- wout[[k]]
      iyrs <- seq_along(x)

      for (i in iyrs) {
        # It is a valid object of class "swWeatherData"
        expect_true(swWeatherData_validity(x[[i]]))

        # Prepare weather data.frame
        wdf <- slot(x[[i]], "data")
        wdf <- set_missing_weather(wdf)

        # It meets weather data requirements
        expect_silent(
          check_weather(
            weather = wdf,
            required_variables = c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
        ))

        # There are no missing data
        expect_false(anyNA(wdf))
      }
    }
  }
})


test_that("Weather generator (integration tests): compare input/output", {
  skip_if_not(identical(tolower(Sys.getenv("RSOILWAT_INTEGRATIONTESTS")),
    "true"))

  dir_inttests <- file.path("..", "rSOILWAT_IntegrationTestOutput")
  dir.create(dir_inttests, showWarnings = FALSE)

  time_steps <- c("Year", "Month", "Week", "Day")


  #--- Load observed weather data
  obs_weather <- readRDS(file.path(dir_test_data, "Ex1_weather.rds"))
  obs_df <- dbW_weatherData_to_dataframe(obs_weather)


  #--- Generate weather data with N repeats
  N <- 100
  wgen_df <- list()
  set.seed(127)

  # Prepare rSOILWAT2 input object with no weather data
  sw_in <- readRDS(file.path(dir_test_data, "Ex1_input.rds"))
  swWeather_UseMarkov(sw_in) <- TRUE
  res <- dbW_estimate_WGen_coefs(obs_df, imputation_type = "mean")
  swMarkov_Prob(sw_in) <- res[["mkv_doy"]]
  swMarkov_Conv(sw_in) <- res[["mkv_woy"]]
  set_swWeatherData(sw_in) <- new("swWeatherData")

  wgen_df <- replicate(N, {
    res <- sw_exec(inputData = sw_in)

    out <- lapply(time_steps, function(it) {
      temp <- slot(slot(res, "TEMP"), it)

      data.frame(
        if (it == "Year") {
          temp[, "Year", drop = FALSE]
        } else {
          temp[, c("Year", it)]
        },
        Tmax_C = temp[, "max_C"],
        Tmin_C = temp[, "min_C"],
        PPT_cm = slot(slot(res, "PRECIP"), it)[, "ppt"]
      )
    })
    names(out) <- time_steps
    out
  }, simplify = FALSE)


  #--- Comparison
  compare_weather(ref_weather = obs_df, weather = wgen_df, N = N,
    path = dir_inttests, tag = "IntegrationTest-WeatherGenerator")

})
