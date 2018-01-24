context("rSOILWAT2 annual aggregation")

#---CONSTANTS
tol <- 1e-6
SW_OUTNPERIODS <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
OutPeriods <- c("Day", "Week", "Month", "Year")
veg_types <- c("tree", "shrub", "forbs", "grass")
temp <- list.files(".", pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", expect_gt(length(tests), 0))


for (it in tests) {
  #---INPUTS
  sw_input <- readRDS(paste0(it, "_input.rds"))
  swOUT_TimeStepsForEveryKey(sw_input) <- seq_len(SW_OUTNPERIODS) - 1
  sw_weather <- readRDS(paste0(it, "_weather.rds"))


  #---TESTS
  info1 <- paste("test-data:", it)

  test_that("Water balance & cycle", {
    # Run SOILWAT
    x <- sw_exec(inputData = sw_input, weatherList = sw_weather, echo = FALSE,
      quiet = TRUE)
    expect_s4_class(x, "swOutput")

    # Loop through time steps
    for (pd in seq_len(SW_OUTNPERIODS)) {
      info2 <- paste(info1, "/ time step:", OutPeriods[pd])

      # Get values
      aet <- slot(slot(x, "AET"), OutPeriods[pd])[, "evapotr_cm"]
      pet <- slot(slot(x, "PET"), OutPeriods[pd])[, "pet_cm"]

      # Get evaporation values
      temp <- slot(slot(x, "EVAPSURFACE"), OutPeriods[pd])
      Etotalsurf <- temp[, "evap_total"]
      Elitter <- temp[, "evap_litter"]
      Eponded <- temp[, "evap_surfaceWater"]
      Evegi <- temp[, paste0("evap_", veg_types), drop = FALSE]
      Eveg <- apply(Evegi, 1, sum)
      Etotalint <- Eveg + Elitter

      temp <- slot(slot(x, "EVAPSOIL"), OutPeriods[pd])
      Esoilj <- temp[, grep("Lyr", colnames(temp)), drop = FALSE]
      Esoil <- apply(Esoilj, 1, sum)

      Esnow <- slot(slot(x, "PRECIP"), OutPeriods[pd])[, "snowloss"]
      Etotal <- Etotalsurf + Esoil + Esnow

      # Get transpiration values
      temp <- slot(slot(x, "TRANSP"), OutPeriods[pd])
      Ttotalj <- temp[, grep("transp_total_Lyr", colnames(temp)), drop = FALSE]
      Ttotal <- apply(Ttotalj, 1, sum)
      Tvegij <- lapply(veg_types, function(v)
        temp[, grep(paste0("transp_", v, "_Lyr"), colnames(temp)), drop = FALSE])
      names(Tvegij) <- veg_types

      # Get other water flux values
      infiltration <- slot(slot(x, "SOILINFILT"), OutPeriods[pd])[, "soil_inf"]
      deepDrainage <- slot(slot(x, "DEEPSWC"), OutPeriods[pd])[, "lowLayerDrain_cm"]

      temp <- slot(slot(x, "LYRDRAIN"), OutPeriods[pd])
      temp <- temp[, grep("Lyr", colnames(temp)), drop = FALSE]
      percolationIn <- cbind(infiltration, temp)
      percolationOut <- cbind(temp, deepDrainage)

      temp <- slot(slot(x, "HYDRED"), OutPeriods[pd])
      hydraulicRedistribution <- temp[, grep("total_Lyr", colnames(temp)), drop = FALSE]

      temp <- slot(slot(x, "RUNOFF"), OutPeriods[pd])
      runoff <- apply(temp[, grep("runoff", colnames(temp)), drop = FALSE], 1, sum)
      runon <- apply(temp[, grep("runon", colnames(temp)), drop = FALSE], 1, sum)

      temp <- slot(slot(x, "PRECIP"), OutPeriods[pd])
      snowmelt <- temp[, "snowmelt"]
      rain <- temp[, "rain"]

      arriving_water <- rain + snowmelt + runon


      # Get state change values
      idelta1 <- seq_along(aet)[-length(aet)]
      idelta2 <- seq_along(aet)[-1]

      temp <- slot(slot(x, "SURFACEWATER"), OutPeriods[pd])
      surfaceWater <- temp[, "surfaceWater_cm"]
      delta_surfaceWater <- surfaceWater[idelta2] - surfaceWater[idelta1]

      temp <- slot(slot(x, "SWCBULK"), OutPeriods[pd])
      swcj <- temp[, grep("Lyr", colnames(temp)), drop = FALSE]
      delta_swcj <- swcj[idelta2, ] - swcj[idelta1, ] # today - yesterday
      n_soillayers <- ncol(swcj)

      temp <- matrix(0, nrow = nrow(Esoilj), ncol = n_soillayers)
      temp[, seq_len(ncol(Esoilj))] <- Esoilj
      Esoilj <- temp


      #--- Water balance checks
      # AET <= PET
      expect_true(aet <= pet, info = info2)

      # AET == E(total) + T(total)
      expect_equal(aet, Etotal + Ttotal, info = info2)

      # T(total) = sum of T(veg-type i from soil layer j)
      expect_equal(Ttotal, apply(sapply(Tvegij, function(x) apply(x, 1, sum)), 1, sum),
        info = info2)

      # E(total) = E(total bare-soil) + E(ponded water) + E(total litter-intercepted) +
      #            + E(total veg-intercepted) + E(snow sublimation)
      expect_equal(Etotal, Esoil + Eponded + Eveg + Elitter + Esnow, info = info2)

      # E(total surface) = E(ponded water) + E(total litter-intercepted) +
      #                    + E(total veg-intercepted)
      expect_equal(Etotalsurf, Eponded + Eveg + Elitter, info = info2)


      #--- Water cycling checks
      # infiltration = [rain + snowmelt + runon] - (runoff + intercepted + delta_surfaceWater + Eponded)
      expect_equal(infiltration[idelta2], arriving_water[idelta2] - (runoff[idelta2] +
        intercepted[idelta2] + delta_surfaceWater + Eponded[idelta2]), info = info2)

      # E(soil) + Ttotal = infiltration - (deepDrainage + delta(swc))
      expect_equal(Esoil[idelta2] + Ttotal[idelta2],
        infiltration[idelta2] - (deepDrainage[idelta2] + apply(delta_swcj, 1, sum)),
        info = info2)

      # for every soil layer j: delta(swc) =
      #   = infiltration/percolationIn + hydraulicRedistribution -
      #     (percolationOut/deepDrainage + transpiration + evaporation)
      for (j in seq_len(n_soillayers)) {
        expect_equal(delta_swcj[, j],
          percolationIn[idelta2, j] + hydraulicRedistribution[idelta2, j] -
          (percolationOut[idelta2, j] + Ttotalj[idelta2, j] + Esoilj[idelta2, j]),
          info = paste(info2, "/ soil layer:", j))
      }
    }
  })
}
