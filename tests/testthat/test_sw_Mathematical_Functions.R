context("SOILWAT2's Mathematical Functions")

#---TESTS
test_that("replace_NAs_with_val", {
  # length 1
  x0 <- 1
  expect_identical(replace_NAs_with_val(x0, val_replace = NULL), x0)
  expect_identical(replace_NAs_with_val(x0, val_replace = NA), x0)
  expect_identical(replace_NAs_with_val(x0, val_replace = 2), x0)

  x0 <- NA
  temp <- NA
  expect_identical(replace_NAs_with_val(x0, val_replace = temp), temp)
  temp <- 2
  expect_identical(replace_NAs_with_val(x0, val_replace = temp), temp)

  # length > 1
  n <- 3
  x0 <- rep(NA, n)
  temp <- rep(NA, n)
  expect_identical(replace_NAs_with_val(x0, val_replace = temp[1]), temp)
  temp <- rep(2, n)
  expect_identical(replace_NAs_with_val(x0, val_replace = temp[1]), temp)
})


test_that("cut0Inf", {
  # length 1
  x0 <- 1
  temp <- NA_real_
  expect_identical(cut0Inf(x0, val = temp), x0)
  temp <- -2
  expect_identical(cut0Inf(x0, val = temp), x0)

  x0 <- -1
  temp <- NA_real_
  expect_identical(cut0Inf(x0, val = temp), temp)
  temp <- -2
  expect_identical(cut0Inf(x0, val = temp), temp)

  # length > 1
  n <- 3
  x0 <- rep(-1, n)
  temp <- rep(NA_real_, n)
  expect_identical(cut0Inf(x0, val = temp[1]), temp)
  temp <- rep(2, n)
  expect_identical(cut0Inf(x0, val = temp[1]), temp)

  x0 <- c(1, NA, -1, NA, 1)
  temp <- c(1, NA, 0, NA, 1)
  expect_identical(cut0Inf(x0, val = temp[3]), temp)
})


test_that("finite01", {
  # length 1
  expect_identical(finite01(NA), 0)
  expect_identical(finite01(-1), 0)
  expect_identical(finite01(0 - rSW2_glovars[["tol"]]), 0)
  expect_identical(finite01(temp <- 0 + rSW2_glovars[["tol"]]), temp)
  expect_identical(finite01(temp <- 1 - rSW2_glovars[["tol"]]), temp)
  expect_identical(finite01(1 + rSW2_glovars[["tol"]]), 1)
  expect_identical(finite01(10), 1)
  expect_identical(finite01(0.5), 0.5)

  # length > 1
  expect_identical(
    finite01(c(10, 1, NA, -1, NA, 1, 0, 0.5)),
    c(1, 1, 0, 0, 0, 1, 0, 0.5))

  expect_identical(
    finite01(c(10, 1, NA, -1, NA, 1, 0, 0.5), val_high_replace = NA),
    c(NA, 1, 0, 0, 0, 1, 0, 0.5))

  expect_identical(
    finite01(c(10, 1, NA, -1, NA, 1, 0, 0.5), val_low_replace = -3, val_high_replace = 2),
    c(2, 1, -3, -3, -3, 1, 0, 0.5))
})

test_that("squash_into_low_high", {
  x0 <- c(10, 1, NA, -1, NA, 1, 0, 0.5)

  expect_identical(squash_into_low_high(x0),
    c(1, 1, NA, 0, NA, 1, 0, 0.5))
  expect_identical(squash_into_low_high(x0, val_low = NULL),
    c(1, 1, NA, -1, NA, 1, 0, 0.5))
  expect_identical(squash_into_low_high(x0, val_high = NULL),
    c(10, 1, NA, 0, NA, 1, 0, 0.5))
  expect_identical(squash_into_low_high(x0, val_low = NULL, val_high = NULL),
    x0)

  expect_identical(
    squash_into_low_high(x0,
      val_low = 0, val_low_replace = -3, val_high = 1, val_high_replace = 2),
    c(2, 1, NA, -3, NA, 1, 0, 0.5))
})
