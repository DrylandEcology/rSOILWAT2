

test_that("Manipulate soils", {
  sw_input <- rSOILWAT2::sw_exampleData

  #--- Check that high volume of gravel/coarse fragments
  # does not error out with
  # nolint start: commented_code_linter.
  #  `invalid value of (theta / theta(saturated)) ^ b = 0.000000 (must be != 0)`
  # nolint end
  # See https://github.com/DrylandEcology/SOILWAT2/issues/282
  # and https://github.com/DrylandEcology/rSOILWAT2/issues/170
  soil_data <- rSOILWAT2::swSoils_Layers(sw_input)
  soil_data[, "gravel_content"] <- 0.99
  rSOILWAT2::swSoils_Layers(sw_input) <- soil_data

  sw_out <- try(
    sw_exec(inputData = sw_input),
    silent = TRUE
  )

  # Check that run did not fail due to SWP to infinity error in SOILWAT2
  expect_s4_class(sw_out, "swOutput")
})


test_that("Unrealistic soils", {
  sw_input <- rSOILWAT2::sw_exampleData

  #--- Check negative evaporation coefficients
  # Expect rejection by validity test of S4 object
  # Expect that SOILWAT2 run fails
  # See https://github.com/DrylandEcology/rSOILWAT2/issues/58

  soil_data <- rSOILWAT2::swSoils_Layers(sw_input)
  soil_data[1, "EvapBareSoil_frac"] <- -0.8

  # Validity checks should throw an error
  expect_error(rSOILWAT2::swSoils_Layers(sw_input) <- soil_data)

  # However, we can avoid validity checks ...
  sw_input@soils@Layers[1, "EvapBareSoil_frac"] <- -0.8

  # but now the simulation run should fail
  expect_error(sw_exec(inputData = sw_input, quiet = TRUE))


  #--- Check zero evaporation coefficients
  sw_input@soils@Layers[, "EvapBareSoil_frac"] <- 0
  sw_out <- try(sw_exec(inputData = sw_input, quiet = TRUE), silent = TRUE)
  e_bs <- slot(slot(sw_out, "EVAPSOIL"), "Day")
  expect_gt(nrow(e_bs), 0L)
  if (getNamespaceVersion("rSOILWAT2") <= "6.0.0") {
    # rSOILWAT2: < v6.0.0: expect no output for esoil (interpreted as zero)
    expect_identical(ncol(e_bs), 2L) # Year DOY
  } else if (getNamespaceVersion("rSOILWAT2") < "6.6.0") {
    # rSOILWAT2: v6.0.0 - v6.5.0: expect one layer of 0s for esoil
    expect_identical(ncol(e_bs), 3L) # Year DOY Lyr_1
    expect_identical(sum(e_bs[, 3L]), 0)
  } else {
    # rSOILWAT2: >= v6.6.0: expect each soil layer with 0s for esoil
    # Year DOY Lyr_1 ... Lyr_n
    expect_identical(ncol(e_bs), 2L + nrow(sw_input@soils@Layers))
    expect_identical(sum(e_bs[, -(1L:2L)]), 0)
  }
})
