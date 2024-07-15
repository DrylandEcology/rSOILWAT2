
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


test_that("Manipulate 'swSpinup' class", {
  x <- new("swSpinup")
  expect_s4_class(x, "swSpinup")

  #--- Tests for the 'swSpinup' slot of signature 'swInputData'
  xinput <- xinput2 <- swInputData()
  expect_s4_class(get_swSpinup(xinput), "swSpinup")

  sup <- get_swSpinup(xinput)
  sup2 <- swSpinup()
  expect_identical(sup, sup2)
  set_swSpinup(xinput2) <- sup
  expect_identical(xinput, xinput2)


  #--- Get/set slots
  sns <- slotNames(get_swSpinup(xinput))

  for (sn in sns) {
    fget <- match.fun(paste0("swSpinup_", sn))
    fset <- match.fun(paste0("swSpinup_", sn, "<-"))
    value <- if (identical(sn, "SpinupActive")) TRUE else 2L

    expect_identical(fget(sup), fget(xinput))

    sup <- fset(sup,  value)
    xinput <- fset(xinput,  value)
    expect_identical(fget(sup), fget(xinput))
  }
})




test_that("Run 'rSOILWAT2' with different 'swSpinup' inputs", {
  it <- tests[[1L]]

  #---INPUTS
  sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

  # Run spinup: mode c(1, 2), duration = c(1, 10), scope = c(1, 20)
  swSpinup_SpinupActive(sw_input) <- TRUE

  for (km in 1L:2L) {
    swSpinup_SpinupMode(sw_input) <- km

    for (kd in c(1L, 10L)) {
      swSpinup_SpinupDuration(sw_input) <- kd

      for (ks in c(1L, 20L)) {
        swSpinup_SpinupScope(sw_input) <- ks

        # Run SOILWAT
        res <- sw_exec(
          inputData = sw_input,
          weatherList = sw_weather,
          echo = FALSE,
          quiet = TRUE
        )

        expect_s4_class(res, "swOutput")
      }
    }
  }
})
