context("rSOILWAT2 data access functions")


#--- Tests
test_that("set_requested_flags:", {
  #--- INPUTS
  rswin <- rSOILWAT2::sw_exampleData

  std_values <- list(Composition = 0.5, Albedo = 0.5, Height = 0.5, HydRed = TRUE,
    SWPcrit_MPa = -0.5)

  test_data <- list(
    Composition = list(
      use = c(Composition_GrassFraction = FALSE, Composition_ShrubFraction = FALSE,
        Composition_TreeFraction = FALSE, Composition_ForbFraction = FALSE,
        Composition_BareGround = FALSE),
      values = swProd_Composition(rswin)),

    Albedo = list(
      use = c(Grass_Albedo = FALSE, Shrub_Albedo = FALSE, Tree_Albedo = FALSE,
        Forb_Albedo = FALSE, BareGround_Albedo = FALSE),
      values = swProd_Albedo(rswin)),

    Height = list(
      use = c(Grass_CanopyHeight_Constant_cm = FALSE, Shrub_CanopyHeight_Constant_cm = FALSE,
        Tree_CanopyHeight_Constant_cm = FALSE, Forb_CanopyHeight_Constant_cm = FALSE),
      values = swProd_CanopyHeight(rswin)),

    HydRed = list(
      use = c(Grass_HydRed_OnOff = FALSE, Shrub_HydRed_OnOff = FALSE,
        Tree_HydRed_OnOff = FALSE, Forb_HydRed_OnOff = FALSE),
      values = swProd_HydrRedstro_use(rswin)),

    SWPcrit_MPa = list(
      use = c(Grass_SWPcrit_MPa = FALSE, Shrub_SWPcrit_MPa = FALSE,
        Tree_SWPcrit_MPa = FALSE, Forb_SWPcrit_MPa = FALSE),
      values = swProd_CritSoilWaterPotential(rswin))
  )

  test_args <- list(
    Composition = list(tag = "Composition", fun = "swProd_Composition", reset = TRUE,
      default = 0),
    Albedo = list(tag = "Albedo", fun = "swProd_Albedo", reset = FALSE,
      default = NA),
    Height = list(tag = "CanopyHeight_Constant", fun = "swProd_CanopyHeight",
      reset = FALSE, default = NA),
    HydRed = list(tag = "HydRed", fun = "swProd_HydrRedstro_use", reset = FALSE,
      default = NA),
    SWPcrit_MPa = list(tag = "SWPcrit_MPa", fun = "swProd_CritSoilWaterPotential",
      reset = FALSE, default = NA)
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
        dat[["use"]][1] <- TRUE
        dat[["values"]][1] <- std_values[[test]]
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
          dat[["values"]][seq_along(dat[["values"]])[-ids]] <- test_args[[test]][["default"]]
        }

        expect_silent(temp <- do.call(set_requested_flags, args = c(args, dat)))
        values <- utils::getFromNamespace(test_args[[test]][["fun"]], "rSOILWAT2")(temp)
        expect_equal(values, dat[["values"]], info = test)
      }
    }

    # 4) Check: attempt to set bad values
    dat <- test_data[[test]]
    dat[["use"]][1] <- TRUE
    dat[["values"]][1] <- NA
    expect_error(do.call(set_requested_flags, args = c(args, dat)))
  }
})

