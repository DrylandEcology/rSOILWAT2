
# The 8 checks, implemented below, correspond to the checks in
# \var{SOILWAT/test/test_WaterBalance.cc}


#---CONSTANTS
tol <- 10 ^ (-rSW2_glovars[["kSOILWAT2"]][["kINT"]][["OUT_DIGITS"]])
SW_OUTNPERIODS <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
OutPeriods <- rSW2_glovars[["kSOILWAT2"]][["OutPeriods"]]
veg_types <- c("tree", "shrub", "forbs", "grass")
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)
test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


# List of (available) SWRC-PTF combinations
list_swrcs_ptfs <- unname(as.list(as.data.frame(t(
  rSOILWAT2::list_matched_swrcs_ptfs()
))))

tmp <- check_ptf_availability(
  sapply(list_swrcs_ptfs, `[`, j = 2),
  verbose = FALSE
)
list_swrcs_ptfs <- list_swrcs_ptfs[tmp]


aggregate_for_each_timestep <- function(x, dyt) {
  nid <- 1:2
  list(
    Day = x,
    Week = {
      temp <- if (NCOL(x) > 1) x[dyt[["nfw"]] - 1, ] else x[dyt[["nfw"]] - 1]
      temp <- aggregate(temp, by = dyt[["d"]][dyt[["nfw"]], c("Week", "Year")],
        FUN = sum)
      temp <- temp[, -nid]
    },
    Month = {
      temp <- if (NCOL(x) > 1) x[dyt[["nfm"]] - 1, ] else x[dyt[["nfm"]] - 1]
      temp <- aggregate(temp, by = dyt[["d"]][dyt[["nfm"]], c("Month", "Year")],
        FUN = sum)
      temp <- temp[, -nid]
    },
    Year = {
      temp <- if (NCOL(x) > 1) x[dyt[["nfy"]] - 1, ] else x[dyt[["nfy"]] - 1]
      temp <- aggregate(temp, by = list(dyt[["d"]][dyt[["nfy"]], "Year"]),
        FUN = sum)
      temp <- temp[, -1]
    })
}


#--- Loop over test cases ------
for (it in tests) {
  #---INPUTS
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))
  sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))



  # Request summed values for every time step
  # but turn off SWP (because summed VWC may be larger than theta_sat)
  deactivate_swOUT_OutKey(sw_input) <- rSOILWAT2::sw_out_flags()["sw_swp"]

  swOUT_TimeStepsForEveryKey(sw_input) <- seq_len(SW_OUTNPERIODS) - 1
  slot(slot(sw_input, "output"), "sumtype")[] <- 1L


  #--- Loop over SWRC-PTF combinations ------
  for (isp in seq_along(list_swrcs_ptfs)) {
    # Set SWRC/PTF
    rSOILWAT2::swSite_SWRCflags(sw_input) <- list_swrcs_ptfs[[isp]]


    #---TESTS
    info1 <- paste(
      "test-data:", it, "/",
      paste(list_swrcs_ptfs[[isp]], collapse = "-")
    )

    test_that("Water balance & cycle", {
      # Run SOILWAT (but some PTFs require live internet!)
      x <- try(
        sw_exec(
          inputData = sw_input,
          weatherList = sw_weather,
          echo = FALSE,
          quiet = TRUE
        ),
        silent = TRUE
      )

      if (inherits(x, "try-error")) {
        # Skip if it failed because PTF requires internet but we are offline
        if (isTRUE(grepl("requires live internet", x, fixed = TRUE))) {
          succeed(paste(info1, "requires live internet, skipping for now!"))
        } else {
          fail(paste(info1, x))
        }

      } else {

        expect_s4_class(x, "swOutput")


        # State change values which are directly re-aggregated from daily data
        N <- slot(x, "dy_nrow")
        Ns <- seq_len(N)
        idelta1 <- Ns[-N]
        idelta2 <- Ns[-1]

        temp <- slot(slot(x, "SURFACEWATER"), "Day")
        surfaceWater <- temp[, "surfaceWater_cm"]

        dates <- data.frame(temp[, c("Year", "Day")])
        dates[, "DOY"] <- dates[, "Day"]
        temp <- as.POSIXlt(seq.Date(
          from = as.Date(ISOdate(dates[1, "Year"], 1, 1)),
          to = as.Date(ISOdate(dates[nrow(dates), "Year"], 12, 31)),
          by = "day"
        ))
        dates[, "Month"] <- 1 + temp$mon
        dates[, "Day"] <- temp$mday
        # SOILWAT2 'weeks' are not calendar weeks as in
        #   \code{as.integer(format(temp, "%W"))}
        #   with \code{%U = US weeks}; \coe{%V = ISO 8601}; \code{%W = UK weeks}
        # instead SOILWAT2 numbers consecutive sets of 7-day periods
        dates[, "Week"] <- 1 + (dates[, "DOY"] - 1) %/% 7
        dyt <- list(
          d = dates,
          ids1 = idelta1,
          ids2 = idelta2,
          # not first year:
          nfy = which(temp <- dates[, "Year"] != dates[1, "Year"]),
          # not first month of first year:
          nfm = which(temp | dates[, "Month"] != dates[1, "Month"]),
          # not first week of first year:
          nfw = which(temp | dates[, "Week"] != dates[1, "Week"])
        )

        # change in ponded (surface) water
        list_delta_surfaceWater <- aggregate_for_each_timestep(
          x = surfaceWater[dyt[["ids2"]]] - surfaceWater[dyt[["ids1"]]],
          dyt = dyt
        )

        # change in soil moisture
        temp <- slot(slot(x, "SWCBULK"), "Day")
        swcj <- temp[, grep("Lyr", colnames(temp), fixed = TRUE), drop = FALSE]
        n_soillayers <- ncol(swcj)

        # today - yesterday:
        dy_delta_swcj <- swcj[dyt[["ids2"]], ] - swcj[dyt[["ids1"]], ]
        list_delta_swcj <- aggregate_for_each_timestep(x = dy_delta_swcj, dyt)
        list_delta_swc_total <- aggregate_for_each_timestep(
          x = rowSums(dy_delta_swcj),
          dyt = dyt
        )


        # Loop through time steps
        for (pd in seq_len(SW_OUTNPERIODS)) {
          info2 <- paste(info1, "/ time step:", OutPeriods[pd])

          # Get values
          ets <- slot(slot(x, "AET"), OutPeriods[pd])
          aet <- ets[, "evapotr_cm"]
          pet <- slot(slot(x, "PET"), OutPeriods[pd])[, "pet_cm"]

          temp <- seq_along(aet)
          idelta1 <- temp[-length(temp)]
          idelta2 <- temp[-1]

          # Get evaporation values
          temp <- slot(slot(x, "EVAPSURFACE"), OutPeriods[pd])
          Etotalsurf <- temp[, "evap_total"]
          Elitter <- temp[, "evap_litter"]
          Eponded <- temp[, "evap_surfaceWater"]
          Evegi <- temp[, paste0("evap_", veg_types), drop = FALSE]
          Eveg <- rowSums(Evegi)
          Etotalint <- Eveg + Elitter

          temp <- slot(slot(x, "EVAPSOIL"), OutPeriods[pd])
          ids <- grep("Lyr", colnames(temp), fixed = TRUE)
          Esoilj <- temp[, ids, drop = FALSE]
          Esoil <- rowSums(Esoilj)

          temp <- matrix(0, nrow = nrow(Esoilj), ncol = n_soillayers)
          temp[, seq_len(ncol(Esoilj))] <- Esoilj
          Esoilj <- temp

          Esnow <- slot(slot(x, "PRECIP"), OutPeriods[pd])[, "snowloss"]
          Etotal <- Etotalsurf + Esoil + Esnow

          # Get transpiration values
          temp <- slot(slot(x, "TRANSP"), OutPeriods[pd])
          ids <- grep("transp_total_Lyr", colnames(temp), fixed = TRUE)
          Ttotalj <- temp[, ids, drop = FALSE]
          Ttotal <- rowSums(Ttotalj)
          Tvegij <- lapply(
            veg_types,
            function(v) {
              ids <- grep(
                paste0("transp_", v, "_Lyr"),
                colnames(temp),
                fixed = TRUE
              )
              temp[, ids, drop = FALSE]
            }
          )
          names(Tvegij) <- veg_types


          #--- Check that calculated transpiration and evaporation matches
          # newly available ones from "AET" slot
          expect_equal(Ttotal, ets[, "tran_cm"], tolerance = tol)
          expect_equal(Esoil, ets[, "esoil_cm"], tolerance = tol)
          expect_equal(Esnow, ets[, "esnow_cm"], tolerance = tol)
          expect_equal(Eveg, ets[, "ecnw_cm"], tolerance = tol)
          expect_equal(Eponded + Elitter, ets[, "esurf_cm"], tolerance = tol)

          tmp_evars2 <- c("esoil_cm", "ecnw_cm", "esurf_cm", "esnow_cm")
          Etotal2 <- rowSums(ets[, tmp_evars2])
          expect_equal(Etotal, Etotal2, tolerance = tol)
          expect_equal(aet, ets[, "tran_cm"] + Etotal2, tolerance = tol)


          #--- Get other water flux values
          infiltration <- slot(
            slot(x, "SOILINFILT"),
            OutPeriods[pd]
          )[, "soil_inf"]
          deepDrainage <- slot(
            slot(x, "DEEPSWC"),
            OutPeriods[pd]
          )[, "lowLayerDrain_cm"]

          temp <- slot(slot(x, "LYRDRAIN"), OutPeriods[pd])
          ids <- grep("Lyr", colnames(temp), fixed = TRUE)
          temp <- temp[, ids, drop = FALSE]
          percolationIn <- cbind(infiltration, temp)
          percolationOut <- cbind(temp, deepDrainage)

          temp <- slot(slot(x, "HYDRED"), OutPeriods[pd])
          ctemp <- grep("total_Lyr", colnames(temp), fixed = TRUE)
          hydraulicRedistribution <- temp[, ctemp, drop = FALSE]

          temp <- slot(slot(x, "INTERCEPTION"), OutPeriods[pd])
          intercepted <- temp[, "int_total"]

          temp <- slot(slot(x, "RUNOFF"), OutPeriods[pd])
          ctemp <- grep("runoff", colnames(temp), fixed = TRUE)
          runoff <- rowSums(temp[, ctemp, drop = FALSE])
          ctemp <- grep("runon", colnames(temp), fixed = TRUE)
          runon <- rowSums(temp[, ctemp, drop = FALSE])

          temp <- slot(slot(x, "PRECIP"), OutPeriods[pd])
          snowmelt <- temp[, "snowmelt"]
          rain <- temp[, "rain"]

          arriving_water <- rain + snowmelt + runon


          # Get state change values
          delta_surfaceWater <- list_delta_surfaceWater[[OutPeriods[pd]]]
          delta_swcj <- list_delta_swcj[[OutPeriods[pd]]]
          delta_swc_total <- list_delta_swc_total[[OutPeriods[pd]]]


          #--- Water balance checks
          # (1) \code{AET <= PET}
          expect_true(all(aet < pet | abs(pet - aet) < tol), info = info2)

          # (2) \code{AET == E(total) + T(total)}
          expect_equal(aet, Etotal + Ttotal, info = info2)

          # (3) \code{T(total) = sum of T(veg-type i from soil layer j)}
          expect_equal(
            Ttotal,
            rowSums(sapply(Tvegij, rowSums)),
            info = info2
          )

          # (4) \code{E(total) = E(total bare-soil) + E(ponded water) +
          #     + E(total litter-intercepted) + E(total veg-intercepted) +
          #     + E(snow sublimation)}
          expect_equal(
            Etotal, Esoil + Eponded + Eveg + Elitter + Esnow,
            info = info2
          )

          # (5) \code{E(total surface) = E(ponded water) +
          #      + E(total litter-intercepted) + E(total veg-intercepted)}
          expect_equal(
            Etotalsurf, Eponded + Eveg + Elitter,
            info = info2
          )


          #--- Water cycling checks
          # (6) \code{infiltration = [rain + snowmelt + runon] -
          #     (runoff + intercepted + delta_surfaceWater + Eponded)}
          expect_equal(
            infiltration[idelta2], arriving_water[idelta2] -
            (runoff[idelta2] + intercepted[idelta2] + delta_surfaceWater +
            Eponded[idelta2]),
            info = info2
          )

          # (7) \code{E(soil) + Ttotal =
          #     infiltration - (deepDrainage + delta(swc))}
          expect_equal(
            Esoil[idelta2] + Ttotal[idelta2],
            infiltration[idelta2] - (deepDrainage[idelta2] + delta_swc_total),
            info = info2
          )

          # (8) for every soil layer j: \code{delta(swc) =
          #   = infiltration/percolationIn + hydraulicRedistribution -
          #     (percolationOut/deepDrainage + transpiration + evaporation)}
          for (j in seq_len(n_soillayers)) {
            expect_equal(
              delta_swcj[, j],
              percolationIn[idelta2, j] + hydraulicRedistribution[idelta2, j] -
              (percolationOut[idelta2, j] + Ttotalj[idelta2, j] +
              Esoilj[idelta2, j]),
              info = paste(info2, "/ soil layer:", j)
            )
          }
        }
      }
    })
  }
}
