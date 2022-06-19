context("rSOILWAT2 soil temperature instability")

#---CONSTANTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", expect_gt(length(tests), 0))

st_name <- rSW2_glovars[["kSOILWAT2"]][["OutKeys"]][["SW_SOILTEMP"]]

format_badData <- function(data, ids_bad) {
  if (any(ids_bad)) {
    paste(apply(round(data[ids_bad, ], 2), 1, paste, collapse = "/"),
      collapse = "; ")
  } else "all good"
}

for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

  #---TESTS
  test_that("Check weather", {
    dbW_df_day <- dbW_weatherData_to_dataframe(sw_weather)

    info <- paste("test-data", it)
    expect_true(all(dbW_df_day[, "Tmin_C"] > -100, na.rm = TRUE), info = info)
    expect_true(all(dbW_df_day[, "Tmax_C"] < +100, na.rm = TRUE), info = info)
  })


  test_that("Check soil temperature", {
    # Run SOILWAT
    rd <- sw_exec(inputData = sw_input, weatherList = sw_weather,
      echo = FALSE, quiet = TRUE)
    expect_s4_class(rd, "swOutput")
    expect_false(has_soilTemp_failed())

    Tsoil_data <- slot(rd, st_name)
    time_steps <- rSW2_glovars[["sw_TimeSteps"]][1 + Tsoil_data@TimeStep]

    for (k in seq_along(time_steps)) {
      info <- paste("test-data", it, "- slot", time_steps[k])

      Tsoil_data2 <- slot(Tsoil_data, time_steps[k])
      expect_true(all(dim(Tsoil_data2) > 0), info = info)

      ncol1 <- ncol(Tsoil_data2) - Tsoil_data@Columns
      icol <- seq.int(ncol1 + 1, ncol(Tsoil_data2))
      Tsoil <- Tsoil_data2[, icol]

      ids_bad <- apply(Tsoil, 1, function(x) !all(is.finite(x)))
      expect_false(
        any(ids_bad),
        info = paste(
          info,
          "check: is.finite",
          format_badData(Tsoil_data2, ids_bad)
        )
      )

      ids_bad <- apply(Tsoil, 1, function(x) any(x < -100))
      expect_false(
        any(ids_bad),
        info = paste(
          info,
          "check: Tsoil > -100",
          format_badData(Tsoil_data2, ids_bad)
        )
      )

      ids_bad <- apply(Tsoil, 1, function(x) any(x > +100))
      expect_false(
        any(ids_bad),
        info = paste(
          info,
          "check: Tsoil < +100",
          format_badData(Tsoil_data2, ids_bad)
        )
      )
    }
  })
}
