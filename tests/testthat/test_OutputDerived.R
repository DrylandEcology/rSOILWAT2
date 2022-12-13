
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


test_that("Derived output: soil/surface temperature", {
  sw_in <- rSOILWAT2::sw_exampleData
  n_soillayers <- nrow(swSoils_Layers(sw_in))
  req_levels <- c("min", "avg", "max")

  #--- 'SOILTEMP' output de-activated
  # Expect error only if such output requested
  sw_indeact <- sw_in
  deactivate_swOUT_OutKey(sw_indeact) <- sw_out_flags()["sw_soiltemp"]
  sw_out <- sw_exec(inputData = sw_indeact)
  expect_error(get_soiltemp(sw_out, "Month"))
  expect_silent(get_soiltemp(sw_out, "Month", soillayers = NA))


  #--- 'TEMP' output de-activated
  # Expect error only if such output requested
  sw_indeact <- sw_in
  deactivate_swOUT_OutKey(sw_indeact) <- sw_out_flags()["sw_temp"]
  sw_out <- sw_exec(inputData = sw_indeact)
  expect_error(get_soiltemp(sw_out, "Month"))
  expect_silent(get_soiltemp(sw_out, "Month", surface = FALSE, soillayers = 1))


  #--- All simulation output activated
  sw_out <- sw_exec(inputData = sw_in)

  # All output turned off
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = FALSE,
    soillayers = NA
  )

  for (lvl in req_levels) {
    expect_null(st[[lvl]])
  }


  # All output turned off or non-existing soil layers requested
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = FALSE,
    soillayers = c(-1, 999)
  )

  for (lvl in req_levels) {
    expect_null(st[[lvl]])
  }



  # Expect surface soil temperature: `surface`
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = TRUE
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[1], "surfaceTemp")
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }

  # Expect surface soil temperature: `soillayers` include 0
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = FALSE,
    soillayers = 0
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[1], "surfaceTemp")
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }


  # Expect soil temperature: all soil layers: `soillayers` is `NULL`
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    surface = FALSE,
    levels = req_levels,
    soillayers = NULL
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_equal(ncol(st[[lvl]]), n_soillayers)
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }

  # Expect soil temperature: no soil layers: `soillayers` is `NA`
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    surface = TRUE,
    levels = req_levels,
    soillayers = NA
  )

  for (lvl in req_levels) {
    expect_equal(ncol(st[[lvl]]), 1)
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }


  # Expect soil temperature: some soil layers: `soillayers` is integer index
  req_sl <- c(1, 3:4)
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = FALSE,
    soillayers = req_sl
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_equal(ncol(st[[lvl]]), length(req_sl))
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }


  # Expect soil temperature: not all soil layers: `soillayers` is integer index
  req_sl <- c(1, 3:4, 999)
  st <- get_soiltemp(
    sw_out,
    timestep = "Month",
    levels = req_levels,
    surface = FALSE,
    soillayers = req_sl
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_lt(ncol(st[[lvl]]), length(req_sl))
    expect_equal(nrow(st[[lvl]]), slot(sw_out, "mo_nrow"))
  }
})
