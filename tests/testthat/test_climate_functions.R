context("Calculations of climate variables")

# Inputs
weatherList_year1980 <- readRDS(file.path("..", "test_data",
  "weatherList_year1980.rds"))
simTime2_year1980 <- readRDS(file.path("..", "test_data",
  "simTime2_year1980.rds"))

names_calc_SiteClimate <- c("meanMonthlyTempC", "minMonthlyTempC",
  "maxMonthlyTempC", "meanMonthlyPPTcm", "MAP_cm", "MAT_C", "dailyTempMin",
  "dailyTempMean", "dailyC4vars")
lengths_calc_SiteClimate <- c(12L, 12L, 12L, 12L, 1L, 1L, NA, NA, NA)

names_sw_dailyC4_TempVar <- c("Month7th_NSadj_MinTemp_C",
  "LengthFreezeFreeGrowingPeriod_NSadj_Days", "DegreeDaysAbove65F_NSadj_DaysC",
  "Month7th_NSadj_MinTemp_C.sd", "LengthFreezeFreeGrowingPeriod_NSadj_Days.sd",
  "DegreeDaysAbove65F_NSadj_DaysC.sd")


# Tests
test_that("Climate variables: calc_SiteClimate", {
  # Calculate climate variables for a one-year time period
  expect_silent(x <- calc_SiteClimate(weatherList_year1980, year.start = 1980,
    year.end = 1980))
  expect_named(x, names_calc_SiteClimate)
  itemp <- !is.na(lengths_calc_SiteClimate)
  expect_equal(unname(lengths(x))[itemp], lengths_calc_SiteClimate[itemp])
  expect_true(anyNA(x))

  # Include C4-relevant climate variables
  expect_silent(x <- calc_SiteClimate(weatherList_year1980, year.start = 1980,
    year.end = 1980, do_C4vars = TRUE, simTime2 = simTime2_year1980))
  expect_named(x, names_calc_SiteClimate)
  itemp <- !is.na(lengths_calc_SiteClimate)
  expect_equal(unname(lengths(x))[itemp], lengths_calc_SiteClimate[itemp])
  expect_true(!anyNA(x))
  expect_named(x[["dailyC4vars"]], names_sw_dailyC4_TempVar)

  # Updated version calculates `simTime2` if needed
  expect_silent(x2 <- calc_SiteClimate(weatherList_year1980,
    year.start = 1980, year.end = 1980, do_C4vars = TRUE))
  expect_identical(x, x2)

  # Error because time period does not match with available weather data
  expect_error(calc_SiteClimate(weatherList_year1980,
    year.start = 2000, year.end = 2010))
})
