context("Derived output functions")


# Tests
test_that("Derived output: transpiration and evaporation", {
  sw_in <- rSOILWAT2::sw_exampleData

  # With 'AET' output activated
  tran1 <- get_transpiration(sw_exec(inputData = sw_in), "Month")
  evap1 <- get_evaporation(sw_exec(inputData = sw_in), "Month")

  # De-activate 'AET' output and re-calculated from 'TRANSP'
  deactivate_swOUT_OutKey(sw_in) <- "AET"
  tran2 <- get_transpiration(sw_exec(inputData = sw_in), "Month")
  evap2 <- get_evaporation(sw_exec(inputData = sw_in), "Month")


  # Output should be the same either way
  expect_equal(tran1, tran2)
  expect_equal(evap1, evap2)
})
