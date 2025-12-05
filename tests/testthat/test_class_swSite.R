
#---INPUTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


sw_input <- readRDS(file.path(dir_test_data, paste0(tests[[1L]], "_input.rds")))
sw_weather <- readRDS(
  file.path(dir_test_data, paste0(tests[[1L]], "_weather.rds"))
)


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


test_that("Run 'rSOILWAT2' with different transpiration regions", {
  # Set transpiration regions to numeric/integer layer values
  types <- c("as.double", "as.integer")

  for (ftype in types) {
    swin <- sw_input

    # Set type of transpiration regions
    mc <- mc_ok <- swSite_TranspirationRegions(swin)
    mc <- array(match.fun(ftype)(mc), dim = dim(mc), dimnames = dimnames(mc))
    swSite_TranspirationRegions(swin) <- mc

    # Run SOILWAT
    res <- sw_exec(
      inputData = swin,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    expect_s4_class(res, "swOutput")
  }
})


test_that("Run 'rSOILWAT2' with different soil temperature boundary methods", {
  defaultType <- 0L
  types <- c(defaultType, 1L)

  for (ftype in types) {
    swin <- sw_input

    # Set type of soil temperature boundary condition
    swSite_SoilTempBoundaryMethod(swin) <- ftype

    # Run SOILWAT
    res <- sw_exec(
      inputData = swin,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    expect_s4_class(res, "swOutput")

    if (identical(ftype, defaultType)) {
      ts_default <- slot(slot(res, "SOILTEMP"), "Year")
    } else {
      expect_false(has_soilTemp_failed())
      # Expect non-default methods to produce different soil temperature values
      # than default run
      expect_gt(
        sum(abs(ts_default - slot(slot(res, "SOILTEMP"), "Year"))),
        0
      )
    }
  }
})


test_that("Run 'rSOILWAT2' with different PotEvCo inputs", {
  defaultType <- 0L
  types <- c(defaultType, 1L)

  for (ftype in types) {
    swin <- sw_input

    # Set method
    swSite_PotSoilEvCoMethod(swin) <- ftype
    if (ftype != 0L) {
      swSoils_Layers(swin)[, "EvapBareSoil_frac"] <- NA
    }

    # Run SOILWAT
    res <- sw_exec(
      inputData = swin,
      weatherList = sw_weather,
      echo = FALSE,
      quiet = TRUE
    )

    expect_s4_class(res, "swOutput")

    if (identical(ftype, defaultType)) {
      es_default <- slot(slot(res, "EVAPSOIL"), "Day")
    } else {
      expect_false(has_soilTemp_failed())
      # Expect both input methods to produce identical output
      expect_equal(es_default, slot(slot(res, "EVAPSOIL"), "Day"))
    }
  }
})
