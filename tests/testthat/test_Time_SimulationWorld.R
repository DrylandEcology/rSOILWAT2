context("Time in the simulation world")

#--- INPUTS
input_sim_time <- list(
  # test object 1: startyr is leap year
  startyr_leapyear = list(
    useyrs = yrs <- 1979:2010,
    no.usedy = length(days_in_years(yrs[1], yrs[length(yrs)])),
    no.usemo = length(yrs) * 12
  ),
  # test object 2: startyr is not leap year
  startyr_noleapyear = list(
    useyrs = yrs <- 1969:2000,
    no.usedy = length(days_in_years(yrs[1], yrs[length(yrs)])),
    no.usemo = length(yrs) * 12
  )
)


#--- TESTS
test_that("Obtain time information", {
  # Spinup of simulation
  expect_equal(getStartYear(1980), 1981L)
  expect_equal(getStartYear(0), 1L)
  expect_equal(getStartYear(0, 10), 10L)


  # Leap years
  expect_true(isLeapYear(2000))
  expect_true(isLeapYear(2016))
  expect_false(isLeapYear(2100))
  expect_false(isLeapYear(2003))


  # Sequence of month numbers for each day in the period
  expect_equal(
    seq_month_ofeach_day(list(1980, 1, 1), list(2010, 12, 31), tz = "UTC"),
    as.POSIXlt(days_in_years(1980, 2010))$mon + 1)


  # Describe simulation time
  st1 <- setup_time_simulation_run(list(simstartyr = 1979, startyr = 1980,
    endyr = 2010))
  ns <- names(st1)
  expect_equal(st1,
    setup_time_simulation_run(sim_time =
      list(spinup_N = 1, startyr = 1980, endyr = 2010))[ns]
  )
  expect_equal(st1,
    setup_time_simulation_run(sim_time =
      list(simstartyr = 1979, spinup_N = 1, endyr = 2010))[ns]
  )


  # Simulation time aggregation lists
  st2 <- list(N = list(), S = list())

  for (k in seq_along(input_sim_time)) {
    expect_silent(st2[["N"]] <- simTiming_ForEachUsedTimeUnit(
      useyrs = input_sim_time[[k]][["useyrs"]], latitude = 90))

    expect_silent(st2[["S"]] <- simTiming_ForEachUsedTimeUnit(
      useyrs = input_sim_time[[k]][["useyrs"]], latitude = -90))


    for (h in seq_along(st2)) {
      for (d in grep("ForEachUsedDay", names(st2[["N"]]), value = TRUE)) {
        info <- paste("For test =", names(input_sim_time)[k], "/ d =",
          shQuote(d), "/ hemisphere =", names(st2)[[h]])

        expect_equal(length(st2[[h]][[d]]), input_sim_time[[k]][["no.usedy"]],
          info = info)
      }

      for (d in grep("ForEachUsedMonth", names(st2[["N"]]), value = TRUE)) {
        info <- paste("For test =", names(input_sim_time)[k], "/ d =",
          shQuote(d), "/ hemisphere =", names(st2)[[h]])

        expect_equal(length(st2[[h]][[d]]), input_sim_time[[k]][["no.usemo"]],
          info = info)
      }
    }
  }

})


test_that("Check years", {
  expect_silent(x <- update_requested_years(2000, 2010, 1950, 2010,
    verbose = FALSE))
  expect_equal(x[["start_year"]], 2000L)
  expect_equal(x[["end_year"]], 2010L)

  expect_output(x <- update_requested_years(1940, 2010, 1950, 2010,
    verbose = TRUE),
    regexp = "requested start year")
  expect_equal(x[["start_year"]], 1950L)
  expect_equal(x[["end_year"]], 2010L)

  expect_output(x <- update_requested_years(2000, 2020, 1950, 2010,
    verbose = TRUE),
    regexp = "requested end year")
  expect_equal(x[["start_year"]], 2000L)
  expect_equal(x[["end_year"]], 2010L)
})
