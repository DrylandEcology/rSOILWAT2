context("Derived output functions")


# Tests
test_that("Derived output: transpiration and evaporation", {
  sw_in <- rSOILWAT2::sw_exampleData

  # With 'AET' output activated
  sw_out1 <- sw_exec(inputData = sw_in)
  tran1 <- get_transpiration(sw_out1, "Month")
  evap1 <- get_evaporation(sw_out1, "Month")

  # De-activate 'AET' output and re-calculated from 'TRANSP'
  deactivate_swOUT_OutKey(sw_in) <- sw_out_flags()["sw_aet"]
  sw_out2 <- sw_exec(inputData = sw_in)
  tran2 <- get_transpiration(sw_out2, "Month")
  evap2 <- get_evaporation(sw_out2, "Month")


  # Output should be the same either way
  expect_equal(tran1, tran2)
  expect_equal(evap1, evap2)
})


test_that("Derived output: average soil/surface temperature", {
  sw_in <- rSOILWAT2::sw_exampleData
  n_soillayers <- nrow(swSoils_Layers(sw_in))

  # With 'SOILTEMP' output activated
  sw_out1 <- sw_exec(inputData = sw_in)

  st_avg1 <- get_soiltemp_avg(sw_out1, "Month")
  expect_equal(nrow(st_avg1), slot(sw_out1, "mo_nrow"))
  expect_equal(ncol(st_avg1), n_soillayers)

  sf_avg1 <- get_surfacetemp_avg(sw_out1, "Month")
  expect_length(sf_avg1, slot(sw_out1, "mo_nrow"))


  # With 'SOILTEMP' output de-activated
  deactivate_swOUT_OutKey(sw_in) <- sw_out_flags()["sw_soiltemp"]
  sw_out2 <- sw_exec(inputData = sw_in)

  expect_error(get_soiltemp_avg(sw_out2, "Month"))

  sf_avg2 <- get_surfacetemp_avg(sw_out2, "Month")
  expect_length(sf_avg2, slot(sw_out2, "mo_nrow"))


  # WITH 'TEMP' output de-activated
  deactivate_swOUT_OutKey(sw_in) <- sw_out_flags()["sw_temp"]
  sw_out3 <- sw_exec(inputData = sw_in)

  expect_error(get_soiltemp_avg(sw_out3, "Month"))
  expect_error(get_surfacetemp_avg(sw_out3, "Month"))
})
