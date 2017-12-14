context("Carbon dioxide class")

#---TESTS
test_that("Manipulate swCarbon", {
  x <- new("swCarbon")
  expect_s4_class(x, "swCarbon")
  expect_equal(x, swClear(x))

  # We currently don't have querry/replacement functions with signature 'swCarbon'
  # All these methods operate on the 'swCarbon' slot of signature 'swInputData'
  xinput <- xinput2 <- new("swInputData")
  expect_s4_class(get_swCarbon(xinput), "swCarbon")
  co2 <- as.matrix(data.frame(Year = 1951:2000, CO2ppm = 360 + seq_len(50) / 2))
  swCarbon_CO2ppm(xinput) <- co2
  swCarbon_CO2ppm(xinput2) <- co2
  expect_equal(xinput, xinput2)

  # Get/set entire carbon class object
  cco2 <- get_swCarbon(xinput)
  cco2_new <- new("swCarbon")
  expect_false(isTRUE(all.equal(cco2, cco2_new)))
  cco2_new <- new("swCarbon", cco2)
  expect_equal(cco2, cco2_new)
  set_swCarbon(xinput2) <- cco2_new
  expect_equal(xinput, xinput2)

  # Set/querry flags
  swCarbon_Use_Bio(xinput2) <- 1L
  expect_equal(swCarbon_Use_Bio(xinput2), 1L)
  swCarbon_Use_Bio(xinput2) <- swCarbon_Use_Bio(xinput)
  expect_equal(swCarbon_Use_Bio(xinput2), swCarbon_Use_Bio(xinput))
  expect_error(swCarbon_Use_Bio(xinput2) <- 0.5)

  swCarbon_Use_WUE(xinput2) <- 1L
  expect_equal(swCarbon_Use_WUE(xinput2), 1L)
  swCarbon_Use_WUE(xinput2) <- swCarbon_Use_WUE(xinput)
  expect_equal(swCarbon_Use_WUE(xinput2), swCarbon_Use_WUE(xinput))
  expect_error(swCarbon_Use_WUE(xinput2) <- 0.5)

  # Set/querry scenario name
  swCarbon_Scenario(xinput2) <- "test_scenario"
  expect_equal(swCarbon_Scenario(xinput2), "test_scenario")
  swCarbon_Scenario(xinput2) <- swCarbon_Scenario(xinput)
  expect_equal(swCarbon_Scenario(xinput2), swCarbon_Scenario(xinput))
  expect_error(swCarbon_Scenario(xinput2) <- 0.5)

  # Set/querry delta year value
  swCarbon_DeltaYear(xinput2) <- 1950L
  expect_equal(swCarbon_DeltaYear(xinput2), 1950L)
  swCarbon_DeltaYear(xinput2) <- swCarbon_DeltaYear(xinput)
  expect_equal(swCarbon_DeltaYear(xinput2), swCarbon_DeltaYear(xinput))
  expect_error(swCarbon_DeltaYear(xinput2) <- 0.5)

  # Set/querry CO2 concentration
  swCarbon_CO2ppm(xinput2) <- co2
  expect_equal(swCarbon_CO2ppm(xinput2), co2)
  swCarbon_CO2ppm(xinput2) <- swCarbon_CO2ppm(xinput)
  expect_equal(swCarbon_CO2ppm(xinput2), swCarbon_CO2ppm(xinput))
  expect_error(swCarbon_CO2ppm(xinput2) <- 0.5)

  # Fail CO2 slot validity checks
  co2_ok <- co2
  co2[10, ] <- NA
  expect_error(swCarbon_CO2ppm(xinput2) <- co2)
  co2 <- co2_ok
  co2[20, "CO2ppm"] <- -5
  expect_error(swCarbon_CO2ppm(xinput2) <- co2)
  co2 <- co2_ok
  colnames(co2) <- c("year", "co2")
  expect_error(swCarbon_CO2ppm(xinput2) <- co2)
  co2 <- co2_ok
  co2[10, "Year"] <- 1789
  expect_error(swCarbon_CO2ppm(xinput2) <- co2)
})
