
#---CONSTANTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", {
  expect_gt(length(tests), 0)
})



test_that("Check soil temperature", {
  st_name <- rSW2_glovars[["kSOILWAT2"]][["OutKeys"]][["SW_SOILTEMP"]]

  format_badData <- function(data, ids_bad) {
    if (any(ids_bad)) {
      paste(
        apply(round(data[ids_bad, ], 2), 1, paste, collapse = "/"),
        collapse = "; "
      )
    } else {
      "all good"
    }
  }

  for (it in tests) {
    #---INPUTS
    sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
    sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

    #---Check weather
    dbW_df_day <- dbW_weatherData_to_dataframe(sw_weather)

    info <- paste("test-data", it)
    expect_true(all(dbW_df_day[, "Tmin_C"] > -100, na.rm = TRUE), info = info)
    expect_true(all(dbW_df_day[, "Tmax_C"] < +100, na.rm = TRUE), info = info)


    # Run SOILWAT
    rd <- sw_exec(
      inputData = sw_input,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )
    expect_s4_class(rd, "swOutput")
    expect_false(has_soilTemp_failed())

    Tsoil_data <- slot(rd, st_name)
    time_steps <- rSW2_glovars[["kSOILWAT2"]][["OutPeriods"]][
      1 + Tsoil_data@TimeStep
    ]

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
  }
})


#--- Check that min <= avg <= max soil temperature ------
test_that("Check min/avg/max soil temperature", {
  sw_in <- rSOILWAT2::sw_exampleData
  n_sl <- nrow(rSOILWAT2::swSoils_Layers(sw_in))

  sw_out <- rSOILWAT2::sw_exec(inputData = sw_in)

  tsoil <- slot(slot(sw_out, sw_out_flags()["sw_soiltemp"]), "Day")

  snow <- slot(slot(sw_out, sw_out_flags()["sw_snow"]), "Day")
  has_snow <- snow[, "snowdepth_cm"] > 0

  tsurf <- slot(slot(sw_out, sw_out_flags()["sw_temp"]), "Day")
  var_tsurf_avg <- "surfaceTemp_avg_C"
  var_tsurf_min <- "surfaceTemp_min_C"
  var_tsurf_max <- "surfaceTemp_max_C"


  #--- Expect that
  #  - if snow-free: average temperature > minimum temperature
  #  - if snow covered: average temperature >= minimum temperature

  # soil surface temperature
  expect_true(
    all(tsurf[!has_snow, var_tsurf_avg] > tsurf[!has_snow, var_tsurf_min])
  )
  expect_true(
    all(tsurf[has_snow, var_tsurf_avg] >= tsurf[has_snow, var_tsurf_min])
  )

  # soil temperature at layer depth
  for (k in seq_len(n_sl)) {
    var_avg <- paste0("Lyr_", k, "_avg_C")
    var_min <- paste0("Lyr_", k, "_min_C")

    expect_true(all(tsoil[!has_snow, var_avg] > tsoil[!has_snow, var_min]))
    expect_true(all(tsoil[has_snow, var_avg] >= tsoil[has_snow, var_min]))
  }


  #--- Expect that
  #  - if snow-free: average temperature < maximum temperature
  #  - if snow covered: average temperature <= maximum temperature

  # soil surface temperature
  expect_true(
    all(tsurf[!has_snow, var_tsurf_avg] < tsurf[!has_snow, var_tsurf_max])
  )
  expect_true(
    all(tsurf[has_snow, var_tsurf_avg] <= tsurf[has_snow, var_tsurf_max])
  )

  # soil temperature at layer depth
  for (k in seq_len(n_sl)) {
    var_avg <- paste0("Lyr_", k, "_avg_C")
    var_max <- paste0("Lyr_", k, "_max_C")

    expect_true(all(tsoil[!has_snow, var_avg] < tsoil[!has_snow, var_max]))
    expect_true(all(tsoil[has_snow, var_avg] <= tsoil[has_snow, var_max]))
  }
})
