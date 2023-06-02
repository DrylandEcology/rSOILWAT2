

#--- Maintenance inputs ------
#  Copy "Ex1_input.rds" to "versioned_swInputData/" (with updated name)
#  if significant changes occurred.

test_that("Upgrade old rSOILWAT2 input objects", {
  #--- Locate versioned `swInputData` objects
  dir_test_data <- file.path("..", "test_data", "versioned_swInputData")
  fnames_vdata <- list.files(
    dir_test_data,
    pattern = "Ex1_input_v",
    full.names = TRUE
  )

  expect_gt(length(fnames_vdata), 0L)


  # Upgrade `swInputData`
  for (k in seq_along(fnames_vdata)) {
    xold <- readRDS(fnames_vdata[k])

    if (!suppressWarnings(check_version(xold))) {
      expect_error(validObject(xold))
    }

    expect_true(validObject(sw_upgrade(xold)))
  }
})



#--- Maintenance weather ------
#  Copy "Ex1_weather.rds" to "versioned_weatherData/" (with updated name)
#  if significant changes occurred.

test_that("Upgrade old rSOILWAT2 weather objects", {
  #--- Locate versioned `weatherData` objects
  dir_test_data <- file.path("..", "test_data", "versioned_weatherData")
  fnames_vdata <- list.files(
    dir_test_data,
    pattern = "Ex1_weather_v",
    full.names = TRUE
  )

  tmp <- gsub("Ex1_weather_v", "", basename(fnames_vdata), fixed = TRUE)
  tmp <- gsub(".rds", "", tmp, fixed = TRUE)
  vs <- as.numeric_version(tmp)

  expect_gt(length(fnames_vdata), 0L)


  # Upgrade weather data, i.e., lists of class `swWeatherData`
  for (k in seq_along(fnames_vdata)) {
    x <- readRDS(fnames_vdata[k])

    if (!check_version(vs[k])) {
      expect_false(dbW_check_weatherData(x))
    }

    expect_true(dbW_check_weatherData(upgrade_weatherHistory(x)))
  }
})
