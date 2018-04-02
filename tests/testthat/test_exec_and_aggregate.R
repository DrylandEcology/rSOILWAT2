context("rSOILWAT2 annual aggregation")

#---CONSTANTS
tol <- 1e-6
OutSum <- c("off", "sum", "mean", "fnl")
temp <- list.files(".", pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", expect_gt(length(tests), 0))

var_SumNotZero <- c("TEMP", "PRECIP", "SOILINFILT", "VWCBULK", "VWCMATRIC", "SWCBULK",
  "SWABULK", "SWAMATRIC", "SWA", "SWPMATRIC", "TRANSP", "EVAPSOIL", "EVAPSURFACE",
  "INTERCEPTION", "LYRDRAIN", "HYDRED", "ET", "AET", "PET", "WETDAY", "SNOWPACK",
  "DEEPSWC", "CO2EFFECTS")

expect_within <- function(object, expected, ..., info = NULL,
  tol = sqrt(.Machine$double.eps)) {

  robj <- range(object)
  rexp <- range(expected)

  gte <- robj[1] - rexp[1] >= -tol # min of `object` is gte to minimum of `expected`
  lte <- rexp[2] - robj[2] >= -tol # max of `object` is lte to max of `expected`
  within <- gte & lte

  expect_equivalent(within, TRUE, info = info)
}



for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(paste0(it, "_input.rds"))
  sw_weather <- readRDS(paste0(it, "_weather.rds"))

  # Reasonable limits
  soil <- swSoils_Layers(sw_input)
  layer_N <- nrow(soil)
  layer_widths <- diff(c(0, soil[, "depth_cm"]))

  Tmax <- 100 # C
  H2Omax <- 1000 # cm / day
  weather_extremes <- apply(dbW_weatherData_to_dataframe(sw_weather)[, -(1:2)], 2, range)

  var_limits2 <- data.frame(matrix(NA, nrow = 0, ncol = 2,
    dimnames = list(NULL, c("min", "max"))))
  var_limits2["TEMP", ] <- c(max(-Tmax, weather_extremes[1, "Tmin_C"]),
    min(Tmax, weather_extremes[2, "Tmax_C"]))
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

  tempSL <- data.frame(matrix(NA, nrow = layer_N, ncol = 2,
    dimnames = list(NULL, c("min", "max"))))

  var_limitsSL <- list()

  x <- tempSL
  x[, c("min", "max")] <- rep(c(0, 1), each = layer_N)
  for (iv in c("VWCBULK", "VWCMATRIC")) var_limitsSL[[iv]] <- x

  x <- tempSL
  x[, "min"] <- 0
  x[, "max"] <- layer_widths
  for (iv in c("SWCBULK", "SWABULK", "SWAMATRIC", "SWA", "TRANSP", "EVAPSOIL")) {
    var_limitsSL[[iv]] <- x
  }

  x <- tempSL
  x[, c("min", "max")] <- rep(c(0, Inf), each = layer_N)
  var_limitsSL[["SWPMATRIC"]] <- x # units [-bar]
  var_limitsSL[["LYRDRAIN"]] <- x


  #---TESTS
  info1 <- paste("test-data", it)
  test_that("Check weather", {
    expect_equivalent({
        dbW_df_day <- dbW_weatherData_to_dataframe(sw_weather)
        dbW_dataframe_to_monthly(dbW_df_day)
      }, dbW_weatherData_to_monthly(sw_weather), info = info1)
  })

  test_that("Simulate and aggregate", {
    # Run SOILWAT
    rd <- sw_exec(inputData = sw_input, weatherList = sw_weather, echo = FALSE,
      quiet = TRUE)
    expect_s4_class(rd, "swOutput")
    expect_false(has_soilTemp_failed())

    # Run silently/verbosely
    expect_silent(sw_exec(inputData = sw_input, weatherList = sw_weather, echo = FALSE,
        quiet = TRUE))

    # This doesn't work; apparently, testthat::expect_message and similar functions don't capture text written by LogError directly to the console.
    # expect_message(sw_exec(inputData = sw_input, weatherList = sw_weather, echo = FALSE, quiet = FALSE))

    # Loop through output
    temp <- slotNames(rd)
    vars <- temp[!grepl("nrow", temp)]
    fun_agg <- OutSum[1 + slot(get_swOUT(sw_input), "sumtype")]
    expect_true(length(vars) == length(fun_agg))

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
              expect_within(range(val_extremes), var_limits2[vars[k], ], info = info3)
            }

            if (vars[k] %in% names(var_limitsSL)) {
              for (isl in seq_len(layer_N)) {
                itemp <- grep(isl, colnames(val_extremes))

                if (length(itemp) > 0) {
                  # `itemp` could be empty because of soil-evaporation
                  expect_within(range(val_extremes[, itemp]),
                    var_limitsSL[[vars[k]]][isl, ],
                    info = paste(info3, "- soillayer", isl))
                }
              }
            }
          }
        }
      }

      #--- Test: Compare aggregated daily against yearly output
      # Exclusions:
      #   * "ESTABL" produces only yearly output
      #   * SWP is not additive; SOILWAT uses pedotransfer functions
      if (all(unlist(has_times))) {
        if (fun_agg[k] %in% c("mean", "sum") && !(vars[k] %in% c("SWPMATRIC", "ESTABL"))) {
          res_true <- matrix(TRUE, nrow = rd@yr_nrow, ncol = x1@Columns)
          expect_equivalent({
              temp1d <- aggregate(x1@Day[, -(1:2)], by = list(x1@Day[, 1]), FUN = fun_agg[k])
              diff1d <- data.matrix(x1@Year[, -1]) - data.matrix(temp1d[, -1])
              abs(diff1d) < tol
          }, res_true, info = info2)
        }

      } else {
        # slot 'vars[k]' contains
        #   - meta information: "yr_nrow", "mo_nrow", "wk_nrow", "dy_nrow"
        #   - empty slot: "WTHR", "ALLH2O", "ALLVEG"
      }
    }
  })
}
