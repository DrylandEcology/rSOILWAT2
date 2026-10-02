test_that("Manipulate 'swOutput' class", {
  x <- new("swOutput")
  expect_s4_class(x, "swOutput")

  expect_equal(
    length(slotNames(x)) - countNonOutputSlots(),
    rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
  )

  expect_setequal(
    slotNames(x)[-seq_len(countNonOutputSlots())],
    rSW2_glovars[["kSOILWAT2"]][["OutKeys"]]
  )
})

test_that("Check sw_out_flags()", {
  expect_length(
    sw_out_flags(),
    # 4 inactive outkeys
    rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]] - 4L
  )
})

test_that("Check validity of 'swOutput_KEY'", {
  kINT <- rSW2_glovars[["kSOILWAT2"]][["kINT"]]

  #--- Empty object (prototype) is valid
  expect_true(validObject(new("swOutput_KEY")))

  #--- Output keys of a simulation run are valid
  rd <- sw_exec(inputData = rSOILWAT2::sw_exampleData, quiet = TRUE)
  for (k in sw_out_flags()) {
    expect_true(validObject(slot(rd, k)), info = k)
  }

  #--- Replace data of an active output time step
  x <- slot(rd, sw_out_flags()[[1L]])
  ts <- x@TimeStep[x@TimeStep != kINT[["eSW_NoTime"]]][[1L]]
  xp <- swOutput_KEY_Period(x, ts + 1L)
  swOutput_KEY_Period(x, ts + 1L) <- xp
  expect_identical(swOutput_KEY_Period(x, ts + 1L), xp)

  #--- Invalid TimeStep values
  expect_true(
    validObject(
      new("swOutput_KEY", TimeStep = c(0L, kINT[["eSW_NoTime"]]))
    )
  )
  expect_error(new("swOutput_KEY", TimeStep = kINT[["SW_OUTNPERIODS"]]))
  expect_error(new("swOutput_KEY", TimeStep = -1L))
  expect_error(new("swOutput_KEY", TimeStep = NA_integer_))
  expect_error(new("swOutput_KEY", TimeStep = c(1L, 1L)))
  expect_error(
    new("swOutput_KEY", TimeStep = rep(0L, kINT[["SW_OUTNPERIODS"]] + 1L))
  )
})
