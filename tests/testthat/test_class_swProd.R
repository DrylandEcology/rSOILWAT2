


#---TESTS
test_that("Manipulate 'swProd2' class", {
  ids_VegType <- 1L + rSW2_glovars[["kSOILWAT2"]][["VegTypes2"]]
  names_VegTypes <- tolower(
    gsub(
      "SW_",
      "",
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes2"]]),
      fixed = TRUE
    )
  )

  names_VegTypes2 <- sapply(
    names_VegTypes,
    function(x) {
      if (endsWith(x, "s")) {
        if (endsWith(x, "ss")) x else substr(x, 1, nchar(x) - 1)
      } else {
        x
      }
    }
  )


  x <- new("swProd2")
  expect_s4_class(x, "swProd2")

  # Tests for the 'swProd2' slot of signature 'swInputData'
  xinput <- xinput2 <- swInputData()
  expect_s4_class(get_swProd(xinput), "swProd2")

  x1 <- get_swProd(xinput)
  x2 <- swProd2()
  expect_equal(x1, x2)
  set_swProd(xinput2) <- x1
  expect_equal(xinput, xinput2)


  # Slot 'MonthlyVeg'
  xinv <- get_swProd(xinput)

  for (k in ids_VegType) {
    #--- extraction methods
    # integer-index version
    expect_equal(
      swProd_MonProd_veg(xinput, k),
      swProd_MonProd_veg(xinv, k)
    )

    # character-index version
    expect_equal(
      swProd_MonProd_veg(xinput, names_VegTypes[k]),
      swProd_MonProd_veg(xinv, names_VegTypes[k])
    )
    expect_equal(
      swProd_MonProd_veg(xinv, k),
      swProd_MonProd_veg(xinv, names_VegTypes[k])
    )

    #--- replacement methods failures
    data_fail <- matrix(NA, 10, 2)

    expect_error(swProd_MonProd_veg(xinput, k) <- data_fail)
    expect_error(swProd_MonProd_veg(xinv, k) <- data_fail)

    expect_error(swProd_MonProd_veg(xinput, names_VegTypes[k]) <- data_fail)
    expect_error(swProd_MonProd_veg(xinv, names_VegTypes[k]) <- data_fail)

    #--- replacement methods
    data_good <- swProd_MonProd_veg(xinv, k)
    xinput_ref <- xinput
    xinv_ref <- xinv

    swProd_MonProd_veg(xinput, k) <- data_good
    expect_equal(xinput, xinput_ref)
    swProd_MonProd_veg(xinv, k) <- data_good
    expect_equal(xinv, xinv_ref)

    swProd_MonProd_veg(xinput, names_VegTypes[k]) <- data_good
    expect_equal(xinput, xinput_ref)
    swProd_MonProd_veg(xinv, names_VegTypes[k]) <- data_good
    expect_equal(xinv, xinv_ref)
  }
})
