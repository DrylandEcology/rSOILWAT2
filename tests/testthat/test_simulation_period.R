#--- Simulation period: first and last year, last day of the last year ------
test_that("Simulation ends on requested last day of last year", {
  sw_in <- rSOILWAT2::sw_exampleData

  # Last day of daily output
  last_day <- function(years, edoey) {
    swYears_EndYear(sw_in) <- years[[2L]]
    swYears_StartYear(sw_in) <- years[[1L]]
    swYears_EDOEY(sw_in) <- edoey
    sw_out <- rSOILWAT2::sw_exec(inputData = sw_in)
    x <- slot(slot(sw_out, "TEMP"), "Day")
    c(nrow = nrow(x), x[nrow(x), c("Year", "Day")])
  }

  tests <- list(
    # Multiple years
    list(
      years = c(2009L, 2010L),
      edoey = 200L,
      expected = c(565L, 2010L, 200L)
    ),
    list(
      years = c(2009L, 2010L),
      edoey = 365L,
      expected = c(730L, 2010L, 365L)
    ),
    list(
      years = c(2009L, 2010L),
      edoey = 366L,
      expected = c(730L, 2010L, 365L)
    ),
    list(
      years = c(2007L, 2008L),
      edoey = 365L,
      expected = c(731L, 2008L, 366L)
    ),
    list(
      years = c(2007L, 2008L),
      edoey = 366L,
      expected = c(731L, 2008L, 366L)
    ),
    # Single year
    list(
      years = c(2009L, 2009L),
      edoey = 200L,
      expected = c(200L, 2009L, 200L)
    ),
    list(
      years = c(2008L, 2008L),
      edoey = 200L,
      expected = c(200L, 2008L, 200L)
    ),
    list(
      years = c(2008L, 2008L),
      edoey = 365L,
      expected = c(366L, 2008L, 366L)
    ),
    list(years = c(2008L, 2008L), edoey = 366L, expected = c(366L, 2008L, 366L))
  )

  for (k in seq_along(tests)) {
    expect_equal(
      unname(last_day(tests[[k]][["years"]], tests[[k]][["edoey"]])),
      tests[[k]][["expected"]],
      info = paste(
        "years",
        toString(tests[[k]][["years"]]),
        "EDOEY",
        tests[[k]][["edoey"]]
      )
    )
  }
})
