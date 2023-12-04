
# see sw_meteo_obtain
vars_mm <- c(
  "metadata",
  "rawdata",
  "weatherDF",
  "vals_missing",
  "desc_rsds",
  "use_cloudCoverMonthly",
  "use_windSpeedMonthly",
  "use_humidityMonthly",
  "dailyInputFlags"
)

expect_obtained_meteo <- function(x) {
  expect_false(inherits(!!x, "try-error"))

  expect_named(!!x, expected = vars_mm, ignore.order = TRUE)

  wd <- dbW_dataframe_to_weatherData(x[["weatherDF"]])
  expect_true(dbW_check_weatherData(wd))
  expect_identical(nrow(x[["weatherDF"]]), nrow(x[["vals_missing"]]))
}


test_that("Weather data extraction", {
  skip_on_ci()
  skip_if_offline()

  #--- Daymet weather for "Mccracken Mesa" location
  if (requireNamespace("daymetr")) {
    mm_dm <- suppressMessages(
      try(
        rSOILWAT2::sw_meteo_obtain_DayMet(
          x = c(longitude = -109.3378, latitude = 37.44671),
          start_year = 2015,
          end_year = 2023
        )
      )
    )

    expect_obtained_meteo(mm_dm)
  }


  #--- SCAN station "Mccracken Mesa"
  if (requireNamespace("soilDB")) {
    mm_scan <- suppressMessages(
      try(
        rSOILWAT2::sw_meteo_obtain_SCAN(
          x = 2140, # SCAN station code
          start_year = 2015,
          end_year = 2023
        )
      )
    )

    expect_obtained_meteo(mm_scan)
  }
})
