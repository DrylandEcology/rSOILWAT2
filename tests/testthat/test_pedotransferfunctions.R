
# How the functions are applied in rSFSW2
# section: aggregation
#   - dailyRechargeExtremes
#     - case 1: SWPtoVWC(-0.033, texture$sand.top, texture$clay.top)
#   - dailySuitablePeriodsAvailableWater, dailySWPdrynessIntensity,
#     monthlySWAbulk
#     - case 1: SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top)
#   - mean doy: SWAbulk
#     - case 2: SWPtoVWC(index.SWPcrit, sand, clay)
#   - mean doy: SWPmatric
#     - case 3: VWCtoSWP(res.dailyMean[ir], textureDAgg$sand[al],
#               textureDAgg$clay[al]) # ir is vector; al is value
# section: functions
#   - get_SWPmatric_aggL
#     - case 3: VWCtoSWP(vwcmatric$top, texture$sand.top, texture$clay.top)
#     - case 3: VWCtoSWP(vwcmatric$aggMean.top, texture$sand.top,
#                 texture$clay.top)
#     - case 6: VWCtoSWP(vwcmatric$val[, -index.header], sand, clay)


# Inputs
# Table 2 from Cosby, B.J., Hornberger, G.M., Clapp, R.B. & Ginn, T.R. (1984).
# A statistical exploration of the relationships of soil moisture
# characteristics to the physical properties of soils. Water Resour Res, 20,
# 682-690.
texture <- data.frame(
  sand = c(0.92, 0.82, 0.58, 0.43, 0.17, 0.58, 0.32, 0.10, 0.52, 0.06, 0.22),
  clay = c(0.03, 0.06, 0.10, 0.18, 0.13, 0.27, 0.34, 0.34, 0.42, 0.47, 0.58)
)
row.names(texture) <- c(
  "Sand", "Loamy sand", "Sandy loam", "Loam",
  "Silty loam", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay",
  "Silty clay", "Clay"
)

# Field capacity and agricultural permanent wilting point
# for Campbell1974 and Cosby1984
swp_fix <- c(fc = -0.0333, pwp = -1.5) # MPa
vwc_fix <- data.frame(
  fc = c(
    0.103519295200457, 0.138084712513314, 0.210684319180335,
    0.276327910591054, 0.344767253784927, 0.259008902122202, 0.331526118930414,
    0.391036796958834, 0.292943352979446, 0.4058577839142, 0.368820489547312
  ),
  pwp = c(
    0.0325953572147933, 0.05064269086372, 0.0903291990594713,
    0.143273427070284, 0.163171562436244, 0.152236773973314, 0.210032386550814,
    0.248623511289573, 0.196521033130402, 0.282030801991246, 0.269525768616734
  )
)
row.names(vwc_fix) <- row.names(texture)

ftemp <- file.path("..", "test_data", "swp_values.rds")
if (file.exists(ftemp)) {
  swp_vals <- readRDS(ftemp)

} else {
  swp_vals <- unlist(lapply(
    row.names(texture),
    function(itext) {
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix,
        sand = texture[itext, "sand"],
        clay = texture[itext, "clay"]
      )
    }
  ))
  dim(swp_vals) <- c(nrow(vwc_fix), ncol(vwc_fix), nrow(texture))
  dimnames(swp_vals) <- list(
    row.names(texture),
    names(swp_fix),
    row.names(texture)
  )
  saveRDS(swp_vals, file = ftemp)
}

#--- Tests
test_that("Use SWRC to convert between VWC/SWP", {
  # 1a. x [len = 1] + soils [len = 1] --> res [len = 1]
  fcoarse <- rep(0., 1)

  for (ifix in names(swp_fix)) {
    for (itext in row.names(texture)) {
      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[itext, ifix],
          sand = texture[itext, "sand"],
          clay = texture[itext, "clay"]
        ),
        swp_fix[ifix],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[itext, ifix],
          fcoarse = fcoarse,
          swrc = list(
            swrc_name = "Campbell1974",
            swrcp = ptf_estimate(
              sand = texture[itext, "sand"],
              clay = texture[itext, "clay"],
              fcoarse = fcoarse,
              swrc_name = "Campbell1974",
              ptf_name = "Cosby1984AndOthers"
            )
          )
        ),
        swp_fix[ifix],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_swp_to_vwc(
          swp_MPa = swp_fix[ifix],
          sand = texture[itext, "sand"],
          clay = texture[itext, "clay"]
        ),
        vwc_fix[itext, ifix],
        ignore_attr = c("names", "dimnames")
      )
    }
  }

  # 1b. x [len = l] + soils [len = d] -> res [dim = l = d] where l = d
  fcoarse <- rep(0., nrow(texture))

  for (ifix in names(swp_fix)) {
    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        sand = texture[, "sand"],
        clay = texture[, "clay"]
      ),
      diag(swp_vals[, ifix, ]),
      ignore_attr = c("names", "dimnames")
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        fcoarse = fcoarse,
        swrc = list(
          swrc_name = "Campbell1974",
          swrcp = ptf_estimate(
            sand = texture[, "sand"],
            clay = texture[, "clay"],
            fcoarse = fcoarse,
            swrc_name = "Campbell1974",
            ptf_name = "Cosby1984AndOthers"
          )
        )
      ),
      diag(swp_vals[, ifix, ]),
      ignore_attr = c("names", "dimnames")
    )
  }


  # 2. x [len = 1] + soils [len = d] --> res [len = d]
  fcoarse <- rep(0., nrow(texture))

  for (ifix in names(swp_fix)) {
    for (itext in row.names(texture)) {
      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[itext, ifix],
          sand = texture[, "sand"],
          clay = texture[, "clay"]
        ),
        swp_vals[itext, ifix, ],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[itext, ifix],
          fcoarse = fcoarse,
          swrc = list(
            swrc_name = "Campbell1974",
            swrcp = ptf_estimate(
              sand = texture[, "sand"],
              clay = texture[, "clay"],
              fcoarse = fcoarse,
              swrc_name = "Campbell1974",
              ptf_name = "Cosby1984AndOthers"
            )
          )
        ),
        swp_vals[itext, ifix, ],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_swp_to_vwc(
          swp_MPa = swp_fix[ifix],
          sand = texture[, "sand"],
          clay = texture[, "clay"]
        ),
        unname(vwc_fix[, ifix])
      )
    }
  }


  # 3. x [len = l] + soils [len = 1] --> res [len = l]
  fcoarse <- rep(0., 1)

  for (ifix in names(swp_fix)) {
    for (itext in row.names(texture)) {
      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[, ifix],
          sand = texture[itext, "sand"],
          clay = texture[itext, "clay"]
        ),
        swp_vals[, ifix, itext],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_vwc_to_swp(
          vwcBulk = vwc_fix[, ifix],
          fcoarse = fcoarse,
          swrc = list(
            swrc_name = "Campbell1974",
            swrcp = ptf_estimate(
              sand = texture[itext, "sand"],
              clay = texture[itext, "clay"],
              fcoarse = fcoarse,
              swrc_name = "Campbell1974",
              ptf_name = "Cosby1984AndOthers"
            )
          )
        ),
        swp_vals[, ifix, itext],
        ignore_attr = c("names", "dimnames")
      )

      expect_equal(
        swrc_swp_to_vwc(
          swp_MPa = rep(swp_fix[ifix], nrow(texture)),
          sand = texture[itext, "sand"],
          clay = texture[itext, "clay"]
        ),
        rep(unname(vwc_fix[itext, ifix]), nrow(texture))
      )
    }
  }


  # 4a. x [len = l] + soils [len = d] -> res [dim = l x d] where l != d
  # (x vector repeated for each soil): probably not used
  fcoarse <- rep(0., nrow(texture) - 1L)

  for (ifix in names(swp_fix)) {
    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        sand = texture[-1, "sand"],
        clay = texture[-1, "clay"]
      ),
      unname(swp_vals[, ifix, -1])
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        fcoarse = fcoarse,
        swrc = list(
          swrc_name = "Campbell1974",
          swrcp = ptf_estimate(
            sand = texture[-1, "sand"],
            clay = texture[-1, "clay"],
            fcoarse = fcoarse,
            swrc_name = "Campbell1974",
            ptf_name = "Cosby1984AndOthers"
          )
        )
      ),
      swp_vals[, ifix, -1],
      ignore_attr = c("names", "dimnames")
    )
  }

  # 4b. x [len = l] + soils [len = d] -> res [dim = l x d] where l = d
  # (x vector repeated for each soil): probably not used
  fcoarse <- rep(0., nrow(texture))

  for (ifix in names(swp_fix)) {
    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        sand = texture[, "sand"],
        clay = texture[, "clay"],
        outer_if_equalsize = TRUE
      ),
      unname(swp_vals[, ifix, ])
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix[, ifix],
        fcoarse = fcoarse,
        swrc = list(
          swrc_name = "Campbell1974",
          swrcp = ptf_estimate(
            sand = texture[, "sand"],
            clay = texture[, "clay"],
            fcoarse = fcoarse,
            swrc_name = "Campbell1974",
            ptf_name = "Cosby1984AndOthers"
          )
        ),
        outer_if_equalsize = TRUE
      ),
      swp_vals[, ifix, ],
      ignore_attr = c("names", "dimnames")
    )
  }

  # 5. x [dim = l x d] + soils [len = 1] --> res [dim = l x d]
  fcoarse <- rep(0., 1)

  for (itext in row.names(texture)) {
    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix,
        sand = texture[itext, "sand"],
        clay = texture[itext, "clay"]
      ),
      unname(swp_vals[, , itext])
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc_fix,
        fcoarse = fcoarse,
        swrc = list(
          swrc_name = "Campbell1974",
          swrcp = ptf_estimate(
            sand = texture[itext, "sand"],
            clay = texture[itext, "clay"],
            fcoarse = fcoarse,
            swrc_name = "Campbell1974",
            ptf_name = "Cosby1984AndOthers"
          )
        )
      ),
      swp_vals[, , itext],
      ignore_attr = c("names", "dimnames")
    )
  }

  # 6. x [dim = l x d] + soils [len = d] --> res [dim = l x d]
  # (soils vectors repeated for each row of x)
  fcoarse <- rep(0., nrow(texture))

  for (ifix in names(swp_fix)) {
    vwc <- matrix(
      vwc_fix[, ifix],
      nrow = nrow(vwc_fix) - 1,
      ncol = nrow(texture),
      byrow = TRUE
    )
    res_expected <- matrix(
      swp_fix[ifix],
      nrow = nrow(vwc_fix) - 1,
      ncol = nrow(texture)
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc,
        sand = texture[, "sand"],
        clay = texture[, "clay"]
      ),
      res_expected
    )

    expect_equal(
      swrc_vwc_to_swp(
        vwcBulk = vwc,
        fcoarse = fcoarse,
        swrc = list(
          swrc_name = "Campbell1974",
          swrcp = ptf_estimate(
            sand = texture[, "sand"],
            clay = texture[, "clay"],
            fcoarse = fcoarse,
            swrc_name = "Campbell1974",
            ptf_name = "Cosby1984AndOthers"
          )
        )
      ),
      res_expected,
      ignore_attr = c("names", "dimnames")
    )
  }
})


test_that("Simulate with all SWRC/PTF combinations", {
  list_swrcs_ptfs <- unname(as.list(as.data.frame(t(
    rSOILWAT2::list_matched_swrcs_ptfs()
  ))))

  dir_test_data <- file.path("..", "test_data")
  tmp <- list.files(dir_test_data, pattern = "Ex")
  tmp <- sapply(strsplit(tmp, "_", fixed = TRUE), function(x) x[[1]])
  tests <- unique(tmp)
  expect_gt(length(tests), 0)


  #--- Loop over test cases ------
  for (it in tests[1]) {
    #---INPUTS
    sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))
    sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))

    # Just simulate for a few years to speed things up
    rSOILWAT2::swYears_EndYear(sw_input) <-
      rSOILWAT2::swYears_StartYear(sw_input) + 5


    #--- Loop over SWRC-PTF combinations ------
    for (isp in seq_along(list_swrcs_ptfs)) {

      # Set SWRC/PTF
      rSOILWAT2::swSite_SWRCflags(sw_input) <- list_swrcs_ptfs[[isp]]

      # Run SOILWAT
      x0 <- try(
        rSOILWAT2::sw_exec(
          inputData = sw_input,
          weatherList = sw_weather,
          echo = FALSE,
          quiet = TRUE
        ),
        silent = TRUE
      )


      if (inherits(x0, "try-error")) {
        succeed(
          paste(
            "No live access to",
            paste0(list_swrcs_ptfs[[isp]], collapse = "/"),
            ", skipping for now!"
          )
        )

      } else {
        # Check that we got correct output class
        expect_s4_class(x0, "swOutput")


        #--- Estimate SWRCp and set "has_swrcp" ----
        sw_input1 <- sw_input
        soils <- rSOILWAT2::swSoils_Layers(sw_input1)

        rSOILWAT2::swSite_hasSWRCp(sw_input1) <- TRUE

        rSOILWAT2::swSoils_SWRCp(sw_input1) <- rSOILWAT2::ptf_estimate(
          sand = soils[, "sand_frac"],
          clay = soils[, "clay_frac"],
          fcoarse = soils[, "gravel_content"],
          bdensity = soils[, "bulkDensity_g/cm^3"],
          swrc_name = list_swrcs_ptfs[[isp]][1],
          ptf_name = list_swrcs_ptfs[[isp]][2]
        )

        # Run SOILWAT
        x1 <- rSOILWAT2::sw_exec(
          inputData = sw_input1,
          weatherList = sw_weather,
          echo = FALSE,
          quiet = TRUE
        )
        expect_s4_class(x1, "swOutput")


        #--- Expect simulation output identical independent of
        # when SWRCp are estimated (before/during `sw_exec()`)
        vars <- grep(
          pattern = "version|timestamp",
          x = slotNames(x1),
          value = TRUE,
          invert = TRUE
        )

        for (sv in vars) {
          expect_equal(
            object = slot(x1, !!sv),
            expected = slot(x0, !!sv),
            tolerance = rSW2_glovars[["tol"]],
            info = paste0(
              it, ": ",
              paste0(list_swrcs_ptfs[[isp]], collapse = "/"),
              " - slot ",
              shQuote(sv)
            )
          )
        }
      }
    }
  }
})
