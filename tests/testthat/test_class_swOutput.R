
test_that("Manipulate 'swOutput' class", {
  x <- new("swOutput")
  expect_s4_class(x, "swOutput")

  expect_equal(
    length(slotNames(x)) - 6L,
    rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
  )

  expect_setequal(
    slotNames(x)[-seq_len(6L)],
    rSW2_glovars[["kSOILWAT2"]][["OutKeys"]]
  )
})

test_that("Check sw_out_flags()", {
  expect_length(
    sw_out_flags(),
    rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]] - 5L
  )
})
