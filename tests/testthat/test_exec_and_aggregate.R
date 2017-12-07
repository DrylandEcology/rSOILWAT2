context("rSOILWAT2 annual aggregation")

#---CONSTANTS
tol <- 1e-6
OutSum <- c("off", "sum", "mean", "fnl")
tests <- c("Ex1", "Ex2")

for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(paste0(it, "_input.rds"))
  sw_weather <- readRDS(paste0(it, "_weather.rds"))


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
      if (vars[k] == "SWPMATRIC") {
        next # SWP is not additive; SOILWAT uses pedotransfer functions
      }

      x1 <- slot(rd, vars[k])

      if ("Year" %in% slotNames(x1) && nrow(x1@Year) > 0 &&
        fun_agg[k] %in% c("mean", "sum")) {

        info2 <- paste(info1, "- slot", vars[k])
        res_true <- matrix(TRUE, nrow = rd@yr_nrow, ncol = x1@Columns)

        # Compare aggregated daily against yearly output
        expect_equivalent({
            temp1d <- aggregate(x1@Day[, -(1:2)], by = list(x1@Day[, 1]), FUN = fun_agg[k])
            diff1d <- data.matrix(x1@Year[, -1]) - data.matrix(temp1d[, -1])
            abs(diff1d) < tol
        }, res_true, info = info2)

      } else {
        # slot 'vars[k]' contains
        #   - meta information: "yr_nrow", "mo_nrow", "wk_nrow", "dy_nrow"
        #   - empty slot: "WTHR", "ALLH2O", "ALLVEG"
      }
    }
  })
}
