context("rSOILWAT2 object version tags")

#---TESTS
test_that("rSOILWAT2 object versions", {
  #--- Check for object classes "swInputData" and "swOutput"
  # Check version tag of input object
  sw_in <- rSOILWAT2::sw_exampleData
  expect_true(check_version(sw_in, level = "minor"))

  # Check version tag of output object
  sw_out <- sw_exec(inputData = sw_in, echo = FALSE, quiet = TRUE)
  expect_true(check_version(sw_out, level = "patch"))


  #--- Check for other object classes (ANY-method): return `NA`
  expect_equal(get_version(), NA)
  expect_equal(get_version(NA), NA)
  expect_equal(get_version(NULL), NA)
  expect_equal(get_version(1), NA)
  expect_equal(get_version(list()), NA)
  expect_equal(get_version(try(stop("error"), silent = TRUE)), NA)
  expect_equal(get_version(swSoils_Layers(sw_in)), NA)

  expect_false(check_version(NA))
})

test_that("rSOILWAT2 object timestamps", {
  #--- Check for object classes "swInputData" and "swOutput"
  # Check time stamp of input object
  sw_in <- rSOILWAT2::sw_exampleData
  expect_type(get_timestamp(sw_in), "double")
  expect_s3_class(format_timestamp(sw_in), "POSIXct")

  # Check version tag of output object
  t <- Sys.time()
  sw_out <- sw_exec(inputData = sw_in, echo = FALSE, quiet = TRUE)
  expect_type(get_timestamp(sw_out), "double")
  expect_s3_class(format_timestamp(sw_out), "POSIXct")
  expect_gt(format_timestamp(sw_out), t)


  #--- Check for other object classes (ANY-method): return `NA`
  expect_equal(get_timestamp(), NA)
  expect_equal(get_timestamp(NA), NA)
  expect_equal(get_timestamp(NULL), NA)
  expect_equal(get_timestamp(1), NA)
  expect_equal(get_timestamp(list()), NA)
  expect_equal(get_timestamp(try(stop("error"), silent = TRUE)), NA)
  expect_equal(get_timestamp(swSoils_Layers(sw_in)), NA)

  expect_equal(format_timestamp(NA), as.POSIXct(NA))
})
