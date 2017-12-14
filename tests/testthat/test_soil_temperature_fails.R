context("rSOILWAT2 soil temperature instability")

#---CONSTANTS
sw_TimeSteps <- c("Day", "Week", "Month", "Year")
temp <- list.files(".", pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", expect_gt(length(tests), 0))


for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(paste0(it, "_input.rds"))
  sw_weather <- readRDS(paste0(it, "_weather.rds"))

  #---TESTS
  test_that("Check weather", {
    expect_equivalent({
        dbW_df_day <- dbW_weatherData_to_dataframe(sw_weather)
        dbW_dataframe_to_monthly(dbW_df_day)
      }, dbW_weatherData_to_monthly(sw_weather))

    info <- paste("test-data", it)
    expect_true(all(dbW_df_day[, "Tmin_C"] > -100), info = info)
    expect_true(all(dbW_df_day[, "Tmax_C"] < +100), info = info)
  })


  test_that("Check soil temperature", {
    # Run SOILWAT
    expect_s4_class(rd <- sw_exec(inputData = sw_input, weatherList = sw_weather,
        echo = FALSE, quiet = TRUE), "swOutput")

      soiltemp <- slot(rd, "SOILTEMP")
      time_steps <- sw_TimeSteps[1 + soiltemp@TimeStep]

      for (k in seq_along(time_steps)) {
        x1 <- slot(soiltemp, time_steps[k])

        if (all(dim(x1) > 0)) {
          info <- paste("test-data", it, "- slot", time_steps[k])
          x <- x1[, seq.int(ncol(x1) - soiltemp@Columns + 1, ncol(x1))]

          expect_true(all(is.finite(x)), info = info)
          expect_true(all(x > -100), info = info)
          expect_true(all(x < +100), info = info)
        }
      }
  })
}
