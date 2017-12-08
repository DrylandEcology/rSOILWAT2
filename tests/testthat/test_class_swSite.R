context("Site parameters class")

#---TESTS
test_that("Manipulate 'swSite' class", {
  x <- new("swSite")
  expect_s4_class(x, "swSite")

  # Tests for the 'swSite' slot of signature 'swInputData'
  xinput <- xinput2 <- new("swInputData")
  expect_s4_class(get_swSite(xinput), "swSite")

  site1 <- get_swSite(xinput)
  site2 <- new("swSite")
  expect_equal(site1, site2)
  set_swSite(xinput2) <- site1
  expect_equal(xinput, xinput2)

  # Slot 'ModelCoefficients'
  expect_equal(swSite_ModelCoefficients(xinput),
    swSite_ModelCoefficients(get_swSite(xinput)))

  mc <- mc_ok <- swSite_ModelCoefficients(xinput2)
  expect_equal(swSite_ModelCoefficients(xinput2), mc)

  mc["PETmultiplier"] <- 4
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(swSite_ModelCoefficients(xinput2), swSite_ModelCoefficients(site1))

  mc["PETmultiplier"] <- -1
  expect_error(swSite_ModelCoefficients(site1) <- mc)
  expect_error(swSite_ModelCoefficients(xinput2) <- mc)

  mc <- mc_ok
  mc["DailyRunoff"] <- 0.9
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(swSite_ModelCoefficients(xinput2), swSite_ModelCoefficients(site1))

  mc["DailyRunoff"] <- -1
  expect_error(swSite_ModelCoefficients(site1) <- mc)
  expect_error(swSite_ModelCoefficients(xinput2) <- mc)

  mc["DailyRunoff"] <- 1.5
  expect_error(swSite_ModelCoefficients(site1) <- mc)
  expect_error(swSite_ModelCoefficients(xinput2) <- mc)

  mc <- mc_ok
  mc["DailyRunon"] <- 4
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(swSite_ModelCoefficients(xinput2), swSite_ModelCoefficients(site1))

  mc["DailyRunon"] <- -1
  expect_error(swSite_ModelCoefficients(site1) <- mc)
  expect_error(swSite_ModelCoefficients(xinput2) <- mc)
})
