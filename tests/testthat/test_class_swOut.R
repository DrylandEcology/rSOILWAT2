

test_that("Partial output", {
  swin <- rSOILWAT2::sw_exampleData

  swof <- rSOILWAT2::sw_out_flags()
  tmp <- c("sw_temp", "sw_precip", "sw_snow")

  slot(slot(swin, "output"), "use")[] <- FALSE

  activate_swOUT_OutKey(swin) <- swof[names(swof) %in% tmp]
  rSOILWAT2::swOUT_TimeStepsForEveryKey(swin) <- c(
    daily = 0, monthly = 2, yearly = 3
  )

  # Expect no "Warning: stack imbalance in ..."
  x <- expect_no_warning(
    rSOILWAT2::sw_exec(inputData = swin, echo = FALSE, quiet = TRUE),
    message = "stack imbalance"
  )

  expect_s4_class(x, "swOutput")
})

test_that("No output", {
  swin <- rSOILWAT2::sw_exampleData

  slot(slot(swin, "output"), "use")[] <- FALSE

  x <- expect_silent(
    rSOILWAT2::sw_exec(inputData = swin, echo = FALSE, quiet = TRUE)
  )

  expect_s4_class(x, "swOutput")
})
