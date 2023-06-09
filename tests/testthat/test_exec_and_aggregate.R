
#---CONSTANTS
tols <- list(
  aggregations = 1e-6,
  ranges = sqrt(.Machine$double.eps),
  compare_yearly = 1e-5
)

OutSum <- c("off", "sum", "mean", "fnl")
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


var_maybeZero <- c("ESTABL", "RUNOFF", "SOILTEMP", "SURFACEWATER", "LOG")
var_SumNotZero <- sw_out_flags()
var_SumNotZero <- var_SumNotZero[!(var_SumNotZero %in% var_maybeZero)]


expect_within <- function(
  object,
  expected,
  ...,
  info = NULL,
  tol = tols[["ranges"]],
  digits_N = 4L
) {

  robj <- range(object)
  rexp <- range(expected)

  # min of `object` is gte to minimum of `expected`:
  gte <- robj[1] - rexp[1] >= -tol
  # max of `object` is lte to max of `expected`:
  lte <- rexp[2] - robj[2] >= -tol
  within <- gte & lte

  expect_true(
    within,
    info = paste(
      info,
      if (!gte) {
        paste(
          "min =",
          signif(robj[2], digits_N),
          "smaller than expected", signif(rexp[1], digits_N)
        )
      },
      if (!lte) {
        paste(
          "max =",
          signif(robj[2], digits_N),
          "larger than expected", signif(rexp[2], digits_N)
        )
      }
    )
  )
}


for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

  # Reasonable limits
  soil <- swSoils_Layers(sw_input)
  layer_N <- nrow(soil)
  layer_widths <- diff(c(0, soil[, "depth_cm"]))

  Tmax <- 100 # C
  H2Omax <- 1000 # cm day-1

  if (swWeather_UseMarkov(sw_input)) {
    # Markov-weather generator is turned on to fill in missing weather data
    # see `data-raw/prepare_testInput_objects.R`
    weather_extremes <- data.frame(
      Tmax_C = c(-Tmax, Tmax),
      Tmin_C = c(-Tmax, Tmax),
      PPT_cm = c(0, H2Omax)
    )
  } else {
    nid <- 1:2
    temp <- dbW_weatherData_to_dataframe(sw_weather)[, -nid]
    weather_extremes <- apply(temp, 2, range)
  }

  var_limits2 <- data.frame(matrix(NA,
    nrow = 0,
    ncol = 2,
    dimnames = list(NULL, c("min", "max"))
  ))
  var_limits2["TEMP", ] <- c(
    max(-Tmax, weather_extremes[1, "Tmin_C"]),
    min(Tmax, weather_extremes[2, "Tmax_C"])
  )
  # Surface temperature goes beyond air temperature
  var_limits2["TEMP", ] <- c(-Tmax, Tmax)
  var_limits2["SOILTEMP", ] <- c(-Tmax, Tmax)
  var_limits2["PRECIP", ] <- c(0, min(H2Omax, weather_extremes[2, "PPT_cm"]))
  var_limits2["SOILINFT", ] <- c(0, H2Omax)
  var_limits2["EVAPSURFACE", ] <- c(0, H2Omax)
  var_limits2["INTERCEPTION", ] <- c(0, H2Omax)
  var_limits2["AET", ] <- c(0, H2Omax)
  var_limits2["PET", ] <- c(0, H2Omax)
  var_limits2["WETDAY", ] <- c(0, 1)
  var_limits2["SNOWPACK", ] <- c(0, Inf) # SWE is cumulative
  var_limits2["DEEPSWC", ] <- c(0, H2Omax)
  var_limits2["CO2EFFECTS", ] <- c(0, Inf)
  var_limits2["BIOMASS", ] <- c(0, Inf)

  tempSL <- data.frame(matrix(NA,
    nrow = layer_N,
    ncol = 2,
    dimnames = list(NULL, c("min", "max"))
  ))

  var_limitsSL <- list()

  x <- tempSL
  x[, c("min", "max")] <- rep(c(0, 1), each = layer_N)
  for (iv in c("VWCBULK", "VWCMATRIC")) var_limitsSL[[iv]] <- x

  x <- tempSL
  x[, "min"] <- 0
  x[, "max"] <- layer_widths

  ivars <- c("SWCBULK", "SWABULK", "SWAMATRIC", "SWA", "TRANSP", "EVAPSOIL")
  for (iv in ivars) {
    var_limitsSL[[iv]] <- x
  }

  x <- tempSL
  x[, c("min", "max")] <- rep(c(0, Inf), each = layer_N)
  var_limitsSL[["SWPMATRIC"]] <- x # units in `-bar`
  var_limitsSL[["LYRDRAIN"]] <- x


  #---TESTS
  info1 <- paste("test-data", it)

  dbW_df_day <- dbW_weatherData_to_dataframe(sw_weather)
  test_that("Check weather", {
    expect_equal(
      dbW_dataframe_to_monthly(dbW_df_day),
      dbW_weatherData_to_monthly(sw_weather),
      info = info1
    )

    if (anyNA(dbW_df_day)) {
      expect_equal(
        dbW_dataframe_to_monthly(dbW_df_day, na.rm = TRUE),
        dbW_weatherData_to_monthly(sw_weather, na.rm = TRUE),
        info = info1
      )
    }
  })


  #------ Run SOILWAT2
  test_that("Simulate and aggregate", {
    rd <- sw_exec(
      inputData = sw_input,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    # Check rSOILWAT2 output object
    expect_true(check_version(rd, level = "minor"))
    expect_s4_class(rd, "swOutput")
    expect_false(has_soilTemp_failed())
    expect_true(all(sw_out_flags() %in% slotNames(rd)))

    # Run silently/verbosely
    expect_silent(sw_exec(
      inputData = sw_input,
      weatherList = sw_weather,
      echo = FALSE, quiet = TRUE
    ))

    # This doesn't work; apparently, testthat::expect_message and similar
    # functions don't capture text written by LogError directly to the console.
    if (FALSE) {
      expect_message(sw_exec(
        inputData = sw_input,
        weatherList = sw_weather,
        echo = FALSE, quiet = FALSE
      ))
    }


    # Check that input weather is identical to output weather
    # (don't check missing days that the weather generator filled in)
    is_obs <- complete.cases(dbW_df_day)

    # Precipitation
    expect_equal(
      slot(slot(rd, "PRECIP"), "Day")[is_obs, "ppt"],
      dbW_df_day[is_obs, "PPT_cm"],
      info = info1
    )

    # Tmin
    expect_equal(
      slot(slot(rd, "TEMP"), "Day")[is_obs, "min_C"],
      dbW_df_day[is_obs, "Tmin_C"],
      info = info1
    )

    # Tmax
    expect_equal(
      slot(slot(rd, "TEMP"), "Day")[is_obs, "max_C"],
      dbW_df_day[is_obs, "Tmax_C"],
      info = info1
    )


    # Loop through output
    vars <- grep(
      pattern = "nrow|version|timestamp",
      x = slotNames(rd),
      value = TRUE,
      invert = TRUE
    )

    fun_agg <- OutSum[1 + slot(get_swOUT(sw_input), "sumtype")]
    # nolint start: expect_length_linter.
    expect_identical(length(vars), length(fun_agg))
    # nolint end

    for (k in seq_along(vars)) {
      x1 <- slot(rd, vars[k])
      info2 <- paste(info1, "- slot", vars[k])

      #--- Tests for daily and yearly output
      times <- c("Day", "Year")
      ch <- list(Day = 1:2, Year = 1)
      has_times <- list()

      for (its in times) {
        has_times[[its]] <- its %in% slotNames(x1)

        if (!has_times[[its]]) {
          next
        }

        x2 <- slot(x1, its)
        has_times[[its]] <- has_times[[its]] && nrow(x2) > 0

        if (has_times[[its]]) {
          info3 <- paste(info2, "- timestep", its)

          #--- Test: Output is not all zero
          if (vars[k] %in% var_SumNotZero) {
            expect_true(sum(abs(x2[, -ch[[its]]])) > 0, info = info3)
          }

          #--- Test: Values within reasonable limits
          if (its == "Day" || fun_agg[k] %in% c("mean", "fin")) {
            val_extremes <- apply(x2[, -ch[[its]], drop = FALSE], 2, range)

            if (vars[k] %in% rownames(var_limits2)) {
              expect_within(
                range(val_extremes),
                var_limits2[vars[k], ],
                info = info3
              )
            }

            if (vars[k] %in% names(var_limitsSL)) {
              for (isl in seq_len(layer_N)) {
                itemp <- grep(isl, colnames(val_extremes))

                if (length(itemp) > 0) {
                  # `itemp` could be empty because of soil-evaporation
                  expect_within(
                    range(val_extremes[, itemp]),
                    var_limitsSL[[vars[k]]][isl, ],
                    info = paste(info3, "- soillayer", isl)
                  )
                }
              }
            }
          }
        }
      }

      #--- Test: Compare aggregated daily against yearly output
      # Exclusions:
      #   * "ESTABL" produces only yearly output
      #   * SWP is not additive; SOILWAT uses soil water release curves
      if (all(unlist(has_times))) {
        if (fun_agg[k] %in% c("mean", "sum") &&
            !(vars[k] %in% c("SWPMATRIC", "ESTABL"))
        ) {

          # Aggregate daily to yearly values
          nid <- 1:2
          temp1d <- aggregate(
            x1@Day[, -nid],
            by = list(x1@Day[, 1]),
            FUN = fun_agg[k]
          )

          # Expect that aggregated daily are equal to SOILWAT2 yearly output
          expect_equal(
            data.matrix(temp1d[, -1]),
            data.matrix(x1@Year[, -1]),
            tolerance =  tols[["aggregations"]],
            info = info2
          )
        }

      } else {
        # slot 'vars[k]' contains
        #   - meta information:
        #     - "version", "timestamp"
        #     - "yr_nrow", "mo_nrow", "wk_nrow", "dy_nrow"
        #   - empty slot: "WTHR", "ALLH2O", "ALLVEG"
      }
    }
  })
}



#------ Run SOILWAT2 and compare yearly output to previous simulation run
test_that("Compare to previous runs", {
  for (it in tests) {
    info1 <- paste("test-data", it)

    #---INPUTS
    sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))

    if (!swWeather_UseMarkov(sw_input)) {
      sw_weather <- readRDS(
        file.path(dir_test_data, paste0(it, "_weather.rds"))
      )
      sw_output <- readRDS(file.path(dir_test_data, paste0(it, "_output.rds")))

      swOUT_TimeStepsForEveryKey(sw_input) <- 3 # produce yearly output only

      rdy <- sw_exec(
        inputData = sw_input,
        weatherList = sw_weather,
        echo = FALSE,
        quiet = TRUE
      )

      vars <- grep(
        pattern = "version|timestamp",
        x = slotNames(rdy),
        value = TRUE,
        invert = TRUE
      )

      # Expect SOILWAT2 output values to be equal to the stored values
      for (sv in vars) {
        expect_equal(
          object = slot(rdy, sv),
          expected = slot(sw_output, sv),
          tolerance = tols[["compare_yearly"]],
          info = paste(info1, "- slot", sv)
        )
      }

      # Expect version number and timestamp to be >= than stored copy
      expect_true(
        check_version(
          !!rdy,
          expected_version = !!get_version(sw_output),
          level = "patch"
        )
      )
      expect_gte(!!get_timestamp(rdy), !!get_timestamp(sw_output))
    }
  }
})
