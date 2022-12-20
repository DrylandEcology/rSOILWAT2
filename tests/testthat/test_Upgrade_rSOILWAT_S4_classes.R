

# Maintenance:
#  Copy "Ex1_input.rds" to "versioned_swInputData/" (with updated name)
#  if significant changes occurred.

test_that("Upgrade old rSOILWAT2 input objects", {
  #--- Locate versioned `swInputData` objects
  dir_test_data <- file.path("..", "test_data", "versioned_swInputData")
  fnames_vdata <- list.files(
    dir_test_data,
    pattern = "Ex1_input_v",
    full.names = TRUE
  )

  expect_gt(length(fnames_vdata), 0L)


  # Upgrade `swInputData`
  for (k in seq_along(fnames_vdata)) {
    xold <- readRDS(fnames_vdata[k])

    if (!suppressWarnings(check_version(xold))) {
      expect_error(validObject(xold))
    }

    expect_true(validObject(sw_upgrade(xold)))
  }
})
