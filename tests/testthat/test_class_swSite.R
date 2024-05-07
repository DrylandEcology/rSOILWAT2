
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


#---TESTS
test_that("Manipulate 'swSite' class", {
  x <- new("swSite")
  expect_s4_class(x, "swSite")

  #--- Tests for the 'swSite' slot of signature 'swInputData'
  xinput <- xinput2 <- swInputData()
  expect_s4_class(get_swSite(xinput), "swSite")

  site1 <- get_swSite(xinput)
  site2 <- swSite()
  expect_equal(site1, site2)
  set_swSite(xinput2) <- site1
  expect_equal(xinput, xinput2)

  #--- Slot 'ModelCoefficients'
  expect_equal(
    swSite_ModelCoefficients(xinput),
    swSite_ModelCoefficients(get_swSite(xinput))
  )

  mc <- mc_ok <- swSite_ModelCoefficients(xinput2)
  expect_equal(swSite_ModelCoefficients(xinput2), mc)

  mc["PETmultiplier"] <- 4
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(
    swSite_ModelCoefficients(xinput2),
    swSite_ModelCoefficients(site1)
  )

  mc <- mc_ok
  mc["DailyRunoff"] <- 0.9
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(
    swSite_ModelCoefficients(xinput2),
    swSite_ModelCoefficients(site1)
  )

  mc <- mc_ok
  mc["DailyRunon"] <- 4
  swSite_ModelCoefficients(site1) <- mc
  swSite_ModelCoefficients(xinput2) <- mc
  expect_equal(
    swSite_ModelCoefficients(xinput2),
    swSite_ModelCoefficients(site1)
  )


  #--- Slot TranspirationRegions
  expect_equal(
    swSite_TranspirationRegions(xinput),
    swSite_TranspirationRegions(get_swSite(xinput))
  )

  mc <- mc_ok <- swSite_TranspirationRegions(xinput2)
  expect_equal(swSite_TranspirationRegions(xinput2), mc)

  mc[, "layer"] <- seq_len(nrow(mc))
  swSite_TranspirationRegions(xinput) <- mc
  expect_equal(swSite_TranspirationRegions(xinput), mc)
})


test_that("Run 'rSOILWAT2' with different 'swSite' inputs", {
  it <- tests[1]

  #---INPUTS
  sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

  # Set transpiration regions to numeric/integer layer values
  types <- c("as.double", "as.integer")

  for (ftype in types) {
    # Set type of transpiration regions
    mc <- mc_ok <- swSite_TranspirationRegions(sw_input)
    mc <- array(match.fun(ftype)(mc), dim = dim(mc), dimnames = dimnames(mc))
    swSite_TranspirationRegions(sw_input) <- mc

    # Run SOILWAT
    res <- sw_exec(
      inputData = sw_input,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    expect_s4_class(res, "swOutput")
  }
})
