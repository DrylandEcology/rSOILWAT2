
#--- Tests
test_that("set_requested_flags:", {
  #--- INPUTS
  rswin <- rSOILWAT2::sw_exampleData

  std_values <- list(
    Composition = 0.5,
    Albedo = 0.5,
    Height = 0.5,
    HydRed = TRUE,
    SWPcrit_MPa = -0.5
  )

  used <- stats::setNames(
    rep(FALSE, rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]),
    unname(rSW2_glovars[["kSOILWAT2"]][["VegTypeNames2"]])
  )

  test_data <- list(
    Composition = list(
      use = stats::setNames(
        c(used, FALSE),
        paste0(c(names(used), "Bare Ground"), "_Composition")
      ),
      values = swProd_Composition(rswin)
    ),

    Albedo = list(
      use = stats::setNames(
        c(used, FALSE),
        paste0(c(names(used), "Bare Ground"), "_Albedo")
      ),
      values = swProd_Albedo(rswin)
    ),

    Height = list(
      use = stats::setNames(used, paste0(names(used), "_CanopyHeight")),
      values = t(swProd_CanopyHeight(rswin))
    ),

    HydRed = list(
      use = stats::setNames(used, paste0(names(used), "_HydRed")),
      values = swProd_HydrRedstro_use(rswin)
    ),

    SWPcrit_MPa = list(
      use = stats::setNames(used, paste0(names(used), "_SWPcrit_MPa")),
      values = swProd_CritSoilWaterPotential(rswin)
    )
  )

  test_args <- list(
    Composition = list(
      tag = "Composition",
      fun = "swProd_Composition",
      reset = TRUE,
      default = 0
    ),
    Albedo = list(
      tag = "Albedo",
      fun = "swProd_Albedo",
      reset = FALSE,
      default = NA
    ),
    Height = list(
      tag = "CanopyHeight",
      fun = "swProd_CanopyHeight",
      reset = FALSE,
      default = NA
    ),
    HydRed = list(
      tag = "HydRed",
      fun = "swProd_HydrRedstro_use",
      reset = FALSE,
      default = NA
    ),
    SWPcrit_MPa = list(
      tag = "SWPcrit_MPa",
      fun = "swProd_CritSoilWaterPotential",
      reset = FALSE,
      default = NA
    )
  )

  tests <- names(test_args)
  expect_equal(tests, names(test_data))


  #--- TESTS
  for (test in tests) {
    args <- c(test_args[[test]], list(swIn = rswin))

    # 1) Check: all use turned off
    dat <- test_data[[test]]

    expect_silent(temp <- do.call(set_requested_flags, args = c(args, dat)))
    expect_equal(temp, args[["swIn"]], info = test)

    for (k in 1:2) {
      dat <- test_data[[test]]

      if (isTRUE(length(dim(dat[["values"]])) > 1)) {
        # test = "Height" fails because dat[["values"]] is 2-dimensional
        dat[["use"]][[1L]] <- TRUE
        dat[["values"]][[1L]] <- std_values[[test]]
        expect_error(temp <- do.call(set_requested_flags, args = c(args, dat)))

      } else {
        ids <- if (k == 1) {
          # 2) Check: first and last use turned on
          c(1, length(dat[["use"]]))
        } else {
          # 3) Check: all use turned on
          seq_along(dat[["use"]])
        }
        dat[["use"]][ids] <- TRUE
        if (inherits(dat[["values"]], "logical")) {
          dat[["values"]][ids] <- !dat[["values"]][ids]
        } else {
          dat[["values"]][ids] <- std_values[[test]]
        }
        if (test_args[[test]][["reset"]] && length(dat[["values"]]) > 2) {
          dat[["values"]][seq_along(dat[["values"]])[-ids]] <-
            test_args[[test]][["default"]]
        }

        expect_silent(temp <- do.call(set_requested_flags, args = c(args, dat)))
        values <- utils::getFromNamespace(
          test_args[[test]][["fun"]], "rSOILWAT2"
        )(temp)
        expect_equal(values, dat[["values"]], info = test)
      }
    }

    # 4) Check: attempt to set bad values
    dat <- test_data[[test]]
    dat[["use"]][[1L]] <- TRUE
    dat[["values"]][[1]] <- NA
    expect_error(do.call(set_requested_flags, args = c(args, dat)))
  }
})
