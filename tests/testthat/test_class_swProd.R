


#---TESTS
test_that("Manipulate 'swProd' class", {
  ids_VegType <- rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]
  names_VegTypes <- tolower(
    gsub(
      "SW_",
      "",
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]),
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


  x <- new("swProd")
  expect_s4_class(x, "swProd")

  # Tests for the 'swProd' slot of signature 'swInputData'
  xinput <- xinput2 <- new("swInputData")
  expect_s4_class(get_swProd(xinput), "swProd")

  x1 <- get_swProd(xinput)
  x2 <- new("swProd")
  expect_equal(x1, x2)
  set_swProd(xinput2) <- x1
  expect_equal(xinput, xinput2)


  # Slot 'MonthlyVeg'
  xinv <- get_swProd(xinput)

  for (k in ids_VegType) {
    #--- extraction methods
    # integer-index version
    expect_equal(swProd_MonProd_veg(xinput, 1 + k),
      swProd_MonProd_veg(xinv, 1 + k))

    # character-index version
    expect_equal(swProd_MonProd_veg(xinput, names_VegTypes[1 + k]),
      swProd_MonProd_veg(xinv, names_VegTypes[1 + k]))
    expect_equal(swProd_MonProd_veg(xinv, 1 + k),
      swProd_MonProd_veg(xinv, names_VegTypes[1 + k]))

    # veg-type named version
    f <- utils::getFromNamespace(paste0("swProd_MonProd_",
      names_VegTypes2[1 + k]), ns = "rSOILWAT2")
    expect_equal(f(xinput), f(xinv))
    expect_equal(swProd_MonProd_veg(xinv, 1 + k), f(xinv))

    #--- replacement methods failures
    data_fail <- matrix(NA, 10, 2)

    expect_error(swProd_MonProd_veg(xinput, 1 + k) <- data_fail)
    expect_error(swProd_MonProd_veg(xinv, 1 + k) <- data_fail)

    expect_error(swProd_MonProd_veg(xinput, names_VegTypes[1 + k]) <- data_fail)
    expect_error(swProd_MonProd_veg(xinv, names_VegTypes[1 + k]) <- data_fail)

    fr <- utils::getFromNamespace(paste0("swProd_MonProd_",
      names_VegTypes2[1 + k], "<-"), ns = "rSOILWAT2")
    expect_error(fr(xinput, data_fail))
    expect_error(fr(xinv, data_fail))

    #--- replacement methods
    data_good <- swProd_MonProd_veg(xinv, 1 + k)
    xinput_ref <- xinput
    xinv_ref <- xinv

    swProd_MonProd_veg(xinput, 1 + k) <- data_good
    expect_equal(xinput, xinput_ref)
    swProd_MonProd_veg(xinv, 1 + k) <- data_good
    expect_equal(xinv, xinv_ref)

    swProd_MonProd_veg(xinput, names_VegTypes[1 + k]) <- data_good
    expect_equal(xinput, xinput_ref)
    swProd_MonProd_veg(xinv, names_VegTypes[1 + k]) <- data_good
    expect_equal(xinv, xinv_ref)

    expect_equal(fr(xinput, data_good), xinput_ref)
    expect_equal(fr(xinv, data_good), xinv_ref)
  }
})
