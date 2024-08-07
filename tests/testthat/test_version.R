
#---TESTS
test_that("rSOILWAT2 object versions", {
  #--- Check for object classes "swInputData" and "swOutput"
  # Check version tag of input object
  sw_in <- rSOILWAT2::sw_exampleData
  expect_true(check_version(sw_in, level = "minor"))
  expect_true(
    check_version(sw_in, expected_version = "1.0.0", level = "minor")
  )

  # Check version tag of output object
  sw_out <- sw_exec(inputData = sw_in, echo = FALSE, quiet = TRUE)
  expect_true(check_version(sw_out, level = "patch"))
  expect_true(
    check_version(sw_out, expected_version = "1.0.0", level = "patch")
  )


  #--- devel: dash and dot are equivalent
  sw_in@version <- as.character(numeric_version("1.1.1-9000"))

  # devel level
  expect_true(
    check_version(sw_in, expected_version = "1.1.1.9000", level = "devel")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-9000", level = "devel")
  )

  #--- Check each level of a version
  sw_in@version <- as.character(numeric_version("1.1.1-900.1"))

  # devel level
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.1", level = "devel")
  )
  expect_false(
    check_version(sw_in, expected_version = "1.1.1-900.2", level = "devel")
  )

  # patch level
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.1", level = "patch")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.2", level = "patch")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1", level = "patch")
  )
  expect_false(
    check_version(sw_in, expected_version = "1.1.2", level = "patch")
  )

  # minor level
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.1", level = "minor")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.2", level = "minor")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1", level = "minor")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.2", level = "minor")
  )
  expect_false(
    check_version(sw_in, expected_version = "1.2.0", level = "minor")
  )

  # major level
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.1", level = "major")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1-900.2", level = "major")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.1", level = "major")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.1.2", level = "major")
  )
  expect_true(
    check_version(sw_in, expected_version = "1.2.0", level = "major")
  )
  expect_false(
    check_version(sw_in, expected_version = "2.0.0", level = "major")
  )


  #--- Check that numeric versions are passed through
  v <- as.numeric_version(getNamespaceVersion("rSOILWAT2"))
  expect_identical(get_version(v), as.character(v))
  v <- packageVersion("base")
  expect_identical(get_version(v), as.character(v))


  #--- Check for other object classes (ANY-method): return `NA_character_`
  expect_identical(get_version(), NA_character_)
  expect_identical(get_version(NA), NA_character_)
  expect_identical(get_version(NULL), NA_character_)
  expect_identical(get_version(character(0)), NA_character_)
  expect_identical(get_version(1), NA_character_)
  expect_identical(get_version(list()), NA_character_)
  expect_identical(
    get_version(try(stop("error"), silent = TRUE)),
    NA_character_
  )
  expect_identical(get_version(swSoils_Layers(sw_in)), NA_character_)

  expect_false(check_version(NA))
  expect_false(check_version(character(0)))
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


  #--- Check for other object classes (ANY-method): return `NA_real_`
  expect_equal(get_timestamp(), NA_real_)
  expect_equal(get_timestamp(NA), NA_real_)
  expect_equal(get_timestamp(NULL), NA_real_)
  expect_equal(get_timestamp(1), NA_real_)
  expect_equal(get_timestamp(list()), NA_real_)
  expect_equal(get_timestamp(try(stop("error"), silent = TRUE)), NA_real_)
  expect_equal(get_timestamp(swSoils_Layers(sw_in)), NA_real_)

  expect_equal(format_timestamp(NA), as.POSIXct(NA))
})
