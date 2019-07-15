context("rSOILWAT2 weather generator")

#---INPUTS
path_extdata <- file.path("..", "..", "inst", "extdata")
if (!dir.exists(path_extdata)) {
  path_extdata <- system.file("extdata", package = "rSOILWAT2")
}


dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", expect_gt(length(tests), 0))

sw_weather <- lapply(tests, function(it)
  readRDS(file.path(dir_test_data, paste0(it, "_weather.rds"))))


#---TESTS
test_that("Weather generator: estimate input parameters", {
  for (k in seq_along(tests)) {
    test_dat <- sw_weather[[k]]
    test_df <- data.frame(dbW_weatherData_to_dataframe(test_dat, valNA = NULL))

    if (anyNA(test_df)) {
      expect_warning(res <- dbW_estimate_WGen_coefs(test_df),
        "Insufficient weather data to estimate values")

      expect_message(res <- dbW_estimate_WGen_coefs(test_df, na.rm = TRUE,
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

