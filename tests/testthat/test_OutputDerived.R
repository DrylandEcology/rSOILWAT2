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


  # Loop over time steps
  for (tp in timesteps) {
    id_out_nrows <- switch(
      EXPR = tp,
      Day = "dy_nrow",
      Week = "wk_nrow",
      Month = "mo_nrow",
      Year = "yr_nrow"
    )

    #--- Requested soil moisture directly available
    sw_out <- sw_exec(inputData = sw_in)
    res0 <- lapply(
      types,
      function(type) {
        get_soilmoisture(sw_out, timestep = tp, type = type)
      }
    )

    for (k1 in seq_along(types)) {
      # Expect that columns represent soil layers and rows time steps
      expect_identical(ncol(res0[[k1]]), n_soillayers)
      expect_identical(nrow(res0[[k1]]), slot(sw_out, id_out_nrows))
    }


    #--- Convert soil moisture among swc, vwc_bulk, and vwc_matric
    for (k2 in seq_along(types)) {
      sw_in1 <- sw_in
      # turn off the other soil moisture types
      for (k3 in seq_along(types)[-k2]) {
        deactivate_swOUT_OutKey(sw_in1) <- sw_out_flags()[[names(types)[[k3]]]]
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
            swInput = sw_in1
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
            fcoarse = fcoarse
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
        get_soilmoisture(sw_out, timestep = tp, type = "Month")
      )
      expect_error(
        get_soilmoisture(
          sw_out,
          timestep = tp,
          type = "Month",
          widths_cm = widths_cm
        )
      )
      expect_error(
        get_soilmoisture(
          sw_out,
          timestep = to,
          type = "Month",
          fcoarse = fcoarse
        )
      )
    }
  }
})
