
#---INPUTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", {
  expect_gt(length(tests), 0)
})



#---TESTS
test_that("Weather generator: estimate input parameters", {

  weatherGenerator_dataColumns <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")

  for (k in seq_along(tests)) {
    test_dat <- readRDS(
      file.path(dir_test_data, paste0(tests[k], "_weather.rds"))
    )

    test_df <- data.frame(dbW_weatherData_to_dataframe(test_dat, valNA = NULL))

    if (anyNA(test_df[, weatherGenerator_dataColumns(), drop = FALSE])) {
      # We have NAs that propagate
      # --> warnings: "Insufficient weather data to estimate values [...]"
      res <- suppressWarnings(
        dbW_estimate_WGen_coefs(test_df, propagate_NAs = TRUE)
      )
      expect_true(all(is.na(res[["mkv_woy"]][, -1])))
      expect_true(all(is.na(res[["mkv_doy"]][, -1])))

      # We have NAs that we impute
      # --> messages: "Impute missing [...]"
      res <- suppressMessages(
        dbW_estimate_WGen_coefs(
          test_df,
          propagate_NAs = FALSE,
          imputation_type = "mean"
        )
      )

    } else {
      res <- dbW_estimate_WGen_coefs(test_df)
    }

    # Check that generated weather generator inputs pass requirements
    # no NAs:
    expect_false(anyNA(res[["mkv_doy"]]))
    expect_false(anyNA(res[["mkv_woy"]]))

    # validity tests ok
    sw_in <- rSOILWAT2::sw_exampleData
    expect_equal(
      swMarkov_Prob(sw_in) <- res[["mkv_doy"]],
      res[["mkv_doy"]]
    )
    expect_equal(
      swMarkov_Conv(sw_in) <- res[["mkv_woy"]],
      res[["mkv_woy"]]
    )
  }
})


test_that("Weather generator: generate weather", {
  digits <- 9L

  for (k in seq_along(tests)) {
    test_dat <- readRDS(
      file.path(dir_test_data, paste0(tests[k], "_weather.rds"))
    )
    years <- get_years_from_weatherData(test_dat)
    n <- length(test_dat)

    wout <- list()

    # Case 1: generate weather for dataset and impute missing values
    wout[[1]] <- suppressMessages(
      dbW_generateWeather(
        test_dat,
        imputation_type = "mean",
        imputation_span = 5,
        digits = digits
      )
    )

    # Case 2: generate weather based on partial dataset,
    #   use estimated weather generator coefficients from full dataset
    wgen_coeffs <- suppressMessages(
      dbW_estimate_WGen_coefs(
        test_dat,
        imputation_type = "mean",
        imputation_span = 5
      )
    )

    wout[[2]] <- dbW_generateWeather(
      test_dat[(n - 5):n],
      years = years[length(years)] + 0:10 - 5,
      wgen_coeffs = wgen_coeffs
    )

    # Case 3: generate weather based only on estimated weather generator
    #   coefficients from full dataset
    x_empty <- weatherHistory()
    wout[[3]] <- dbW_generateWeather(
      x_empty,
      years = years[length(years)] + 30:40,
      wgen_coeffs = wgen_coeffs
    )


    #--- Expectations
    for (ke in seq_along(wout)) {
      iyrs <- seq_along(wout[[ke]])

      for (i in iyrs) {
        # It is a valid object of class "swWeatherData"
        expect_true(validObject(wout[[ke]][[i]]))

        # Prepare weather data.frame
        wdf <- set_missing_weather(slot(wout[[ke]][[i]], "data"))

        # It meets weather data requirements
        expect_silent(
          check_weather(
            weather = wdf,
            required_variables = c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
          )
        )

        # There are no missing data
        expect_false(
          anyNA(wdf[, weatherGenerator_dataColumns(), drop = FALSE])
        )
      }
    }


    #--- Expect that values remain unchanged
    # wgen-variables: on days where all wgen-variables are non-missing
    # Non-wgen variables: any non-missing value remain unchanged

    wout1_df <- rSOILWAT2::dbW_weatherData_to_dataframe(wout[[1L]])
    test_dat1_df <- rSOILWAT2::dbW_weatherData_to_dataframe(test_dat)

    ids_wgen <- which(
      colnames(test_dat1_df) %in% rSOILWAT2::weatherGenerator_dataColumns()
    )
    tmp <- apply(
      !is_missing_weather(test_dat1_df[, ids_wgen, drop = FALSE]),
      MARGIN = 1L,
      FUN = all
    )
    isnot_missing_wgen <- as.matrix(data.frame(
      row = rep(which(tmp), times = length(ids_wgen)),
      col = rep(ids_wgen, each = sum(tmp))
    ))

    ids_nowgen <- which(
      !colnames(test_dat1_df) %in% rSOILWAT2::weatherGenerator_dataColumns()
    )
    isnot_missing_nowgen <- which(
      !is_missing_weather(test_dat1_df[, ids_nowgen, drop = FALSE]),
      arr.ind = TRUE
    )
    isnot_missing_nowgen[, "col"] <- ids_nowgen[isnot_missing_nowgen[, "col"]]

    isnot_missing <- rbind(isnot_missing_wgen, isnot_missing_nowgen)


    expect_equal(
      test_dat1_df[isnot_missing],
      wout1_df[isnot_missing],
      tolerance = 10 ^ (-digits)
    )
  }
})


test_that("Weather generator (integration tests): compare input/output", {
  skip_if_not(
    identical(
      tolower(Sys.getenv("RSOILWAT_INTEGRATIONTESTS")),
      "true"
    )
  )

  tag <- "IntegrationTest-WeatherGenerator"

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
  set_WeatherHistory(sw_in) <- weatherHistory()

  wgen_df <- replicate(
    N,
    {
      res <- sw_exec(inputData = sw_in)

      out <- lapply(
        time_steps,
        function(it) {
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
        }
      )

      names(out) <- time_steps
      out
    },
    simplify = FALSE
  )


  #--- Comparison
  suppressMessages(
    compare_weather(
      ref_weather = obs_df,
      weather = wgen_df,
      N = N,
      path = dir_inttests,
      tag = tag
    )
  )
  expect_length(
    list.files(
      path = dir_inttests,
      pattern = tag
    ),
    n = 4L
  )
})
