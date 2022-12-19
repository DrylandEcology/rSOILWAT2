
# Tests
test_that("Derived output: transpiration and evaporation", {
  sw_in <- rSOILWAT2::sw_exampleData

  tp <- "Month"

  icols <- time_columns(tp)

  #--- With 'AET' output activated
  sw_out1 <- sw_exec(inputData = sw_in)
  tran1 <- get_transpiration(sw_out1, timestep = tp)
  evap1 <- get_evaporation(sw_out1, timestep = tp)


  #--- Request time information
  # -> Output should be the same either way
  expect_equal(
    get_transpiration(
      sw_out1,
      timestep = tp,
      keep_time = TRUE
    )[, -icols, drop = TRUE],
    tran1,
    tolerance = rSW2_glovars[["tol"]]
  )

  expect_equal(
    get_evaporation(
      sw_out1,
      timestep = tp,
      keep_time = TRUE
    )[, -icols, drop = TRUE],
    evap1,
    tolerance = rSW2_glovars[["tol"]]
  )


  #--- De-activate 'AET' output and re-calculated from 'TRANSP'
  # -> Output should be the same either way
  deactivate_swOUT_OutKey(sw_in) <- sw_out_flags()["sw_aet"]
  sw_out2 <- sw_exec(inputData = sw_in)

  expect_equal(
    get_transpiration(sw_out2, timestep = tp),
    tran1,
    tolerance = rSW2_glovars[["tol"]]
  )

  expect_equal(
    get_evaporation(sw_out2, timestep = tp),
    evap1,
    tolerance = rSW2_glovars[["tol"]]
  )


  #--- Request time information
  # -> Output should be the same either way
  expect_equal(
    get_transpiration(
      sw_out2,
      timestep = tp,
      keep_time = TRUE
    )[, -icols, drop = TRUE],
    tran1,
    tolerance = rSW2_glovars[["tol"]]
  )

  expect_equal(
    get_evaporation(
      sw_out2,
      timestep = tp,
      keep_time = TRUE
    )[, -icols, drop = TRUE],
    evap1,
    tolerance = rSW2_glovars[["tol"]]
  )
})


test_that("Derived output: soil/surface temperature", {
  sw_in <- rSOILWAT2::sw_exampleData
  n_soillayers <- nrow(swSoils_Layers(sw_in))
  req_levels <- c("min", "avg", "max")
  tp <- "Month"

  #--- 'SOILTEMP' output de-activated
  # Expect error only if such output requested
  sw_indeact <- sw_in
  deactivate_swOUT_OutKey(sw_indeact) <- sw_out_flags()["sw_soiltemp"]
  sw_out <- sw_exec(inputData = sw_indeact)
  expect_error(get_soiltemp(sw_out, timestep = tp))
  expect_silent(get_soiltemp(sw_out, timestep = tp, soillayers = NA))


  #--- 'TEMP' output de-activated
  # Expect error only if such output requested
  sw_indeact <- sw_in
  deactivate_swOUT_OutKey(sw_indeact) <- sw_out_flags()["sw_temp"]
  sw_out <- sw_exec(inputData = sw_indeact)
  expect_error(get_soiltemp(sw_out, timestep = tp))
  expect_silent(
    get_soiltemp(sw_out, timestep = tp, surface = FALSE, soillayers = 1L)
  )


  #--- All simulation output activated
  sw_out <- sw_exec(inputData = sw_in)

  # All output turned off
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
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
    timestep = tp,
    levels = req_levels,
    surface = FALSE,
    soillayers = c(-1L, 999L)
  )

  for (lvl in req_levels) {
    expect_null(st[[lvl]])
  }



  # Expect surface soil temperature: `surface`
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    levels = req_levels,
    surface = TRUE
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[1L], "surfaceTemp")
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }

  # Including time step values
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    levels = req_levels,
    surface = TRUE,
    keep_time = TRUE
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[1L + max(time_columns(tp))], "surfaceTemp")
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }

  # Expect surface soil temperature: `soillayers` include 0
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    levels = req_levels,
    surface = FALSE,
    soillayers = 0
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[1], "surfaceTemp")
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }


  # Expect soil temperature: all soil layers: `soillayers` is `NULL`
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    surface = FALSE,
    levels = req_levels,
    soillayers = NULL
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_identical(ncol(st[[lvl]]), n_soillayers)
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }

  # Including time step values
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    surface = FALSE,
    levels = req_levels,
    soillayers = NULL,
    keep_time = TRUE
  )

  icols <- time_columns(tp)
  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]])[-icols], "Lyr")
    expect_identical(ncol(st[[lvl]]), length(icols) + n_soillayers)
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }


  # Expect soil temperature: no soil layers: `soillayers` is `NA`
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    surface = TRUE,
    levels = req_levels,
    soillayers = NA
  )

  for (lvl in req_levels) {
    expect_identical(ncol(st[[lvl]]), 1L)
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }


  # Expect soil temperature: some soil layers: `soillayers` is integer index
  req_sl <- c(1, 3:4)
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    levels = req_levels,
    surface = FALSE,
    soillayers = req_sl
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_identical(ncol(st[[lvl]]), length(req_sl))
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }


  # Expect soil temperature: not all soil layers: `soillayers` is integer index
  req_sl <- c(1, 3:4, 999)
  st <- get_soiltemp(
    sw_out,
    timestep = tp,
    levels = req_levels,
    surface = FALSE,
    soillayers = req_sl
  )

  for (lvl in req_levels) {
    expect_match(colnames(st[[lvl]]), "Lyr")
    expect_lt(ncol(st[[lvl]]), length(req_sl))
    expect_identical(nrow(st[[lvl]]), nrow_output(sw_out, tp))
  }
})


test_that("Derived output: soil moisture", {
  timesteps <- c("Day", "Week", "Month", "Year")
  types <- c(
    sw_swcbulk = "swc",
    sw_vwcbulk = "vwc_bulk",
    sw_vwcmatric = "vwc_matric"
  )

  sw_in <- rSOILWAT2::sw_exampleData
  n_soillayers <- nrow(swSoils_Layers(sw_in))
  widths_cm <- diff(c(0., swSoils_Layers(sw_in)[, "depth_cm"]))
  fcoarse <- swSoils_Layers(sw_in)[, "gravel_content"]

  # Loop over `keep_time`
  for (kt in c(TRUE, FALSE)) {

    # Loop over time steps
    for (tp in timesteps) {
      #--- Requested soil moisture directly available
      sw_out <- sw_exec(inputData = sw_in)
      res0 <- lapply(
        types,
        function(type) {
          get_soilmoisture(sw_out, timestep = tp, type = type, keep_time = kt)
        }
      )

      for (k1 in seq_along(types)) {
        # Expect that columns represent soil layers and rows time steps
        expect_identical(
          ncol(res0[[k1]]),
          n_soillayers + if (kt) length(time_columns(tp)) else 0L
        )
        expect_identical(nrow(res0[[k1]]), nrow_output(sw_out, tp))
      }


      #--- Convert soil moisture among swc, vwc_bulk, and vwc_matric
      for (k2 in seq_along(types)) {
        sw_in1 <- sw_in
        # turn off the other soil moisture types
        for (k3 in seq_along(types)[-k2]) {
          deactivate_swOUT_OutKey(sw_in1) <-
            sw_out_flags()[[names(types)[[k3]]]]
        }
        sw_out <- sw_exec(inputData = sw_in1)

        #--- Derive soil information from `swInput`
        res1 <- lapply(
          types,
          function(type) {
            get_soilmoisture(
              sw_out,
              timestep = tp,
              type = type,
              swInput = sw_in1,
              keep_time = kt
            )
          }
        )

        for (k1 in seq_along(types)) {
          # Expect that calculated moisture is (almost) equal to direct version
          expect_equal(
            res1[[k1]],
            res0[[k1]],
            tolerance = sqrt(.Machine[["double.eps"]])
          )
        }

        #--- Derive soil information from `widths_cm` and `fcoarse`
        res2 <- lapply(
          types,
          function(type) {
            get_soilmoisture(
              sw_out,
              timestep = tp,
              type = type,
              widths_cm = widths_cm,
              fcoarse = fcoarse,
              keep_time = kt
            )
          }
        )

        for (k1 in seq_along(types)) {
          # Expect that calculated moisture is (almost) equal to direct version
          expect_equal(
            res2[[k1]],
            res0[[k1]],
            tolerance = sqrt(.Machine[["double.eps"]])
          )
        }

        #--- Expect error if calculation does not have enough soil information
        expect_error(
          get_soilmoisture(sw_out, timestep = tp, type = type)
        )
        expect_error(
          get_soilmoisture(
            sw_out,
            timestep = tp,
            type = type,
            widths_cm = widths_cm
          )
        )
        expect_error(
          get_soilmoisture(
            sw_out,
            timestep = to,
            type = type,
            fcoarse = fcoarse
          )
        )
      }
    }
  }
})
