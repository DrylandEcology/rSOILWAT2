testthat::local_edition(3L)

#------ Check that test inputs conform to specifications ------
# see data-raw/prepare_testInput_objects.R

dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


test_that("Check example data", {
  swmiss <- rSW2_glovars[["kSOILWAT2"]][["kNUM"]][["SW_MISSING"]]

  for (it in tests) {
    sw_input <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
    sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))
    sw_weather_df <- dbW_weatherData_to_dataframe(sw_weather)

    yrs_sim <- seq(swYears_StartYear(sw_input), swYears_EndYear(sw_input))
    yrs_wth <- get_years_from_weatherData(sw_weather)


    #--- Check that weather is (not) missing ------
    if (it != "Ex2") {
      # not missing: no NAs, no SW_MISSING, all years
      expect_identical(sum(is.na(sw_weather_df)), 0L)
      expect_identical(sum(sw_weather_df == swmiss), 0L)
      expect_true(all(yrs_sim %in% yrs_wth))

    } else {
      # missing: NAs, SW_MISSING, or not all years
      expect_true(
        sum(is.na(sw_weather_df)) > 0 ||
          sum(sw_weather_df == swmiss) > 0 ||
          !all(yrs_sim %in% yrs_wth)
      )
    }


    #--- Check that weather generator is turned off/on ------
    if (it != "Ex2") {
      expect_false(swWeather_UseMarkov(sw_input))
    } else {
      expect_true(swWeather_UseMarkov(sw_input))
    }


    #--- Check that soil temperature turned on ------
    expect_true(swSite_SoilTemperatureFlag(sw_input))


    #--- Check that CO2-effects are turned on ------
    expect_true(as.logical(swCarbon_Use_Bio(sw_input)))
    expect_true(as.logical(swCarbon_Use_WUE(sw_input)))


    #--- Check that surface is flat/tilted ------
    if (it != "Ex5") {
      # flat: slope == 0 || aspect: NA, 999
      expect_true(
        isTRUE(all.equal(
          swSite_IntrinsicSiteParams(sw_input)[["Slope"]],
          0
        )) ||
          swSite_IntrinsicSiteParams(sw_input)[["Aspect"]] %in% c(NA, swmiss)
      )
    } else {
      # tilted: slope > 0 && aspect: not NA, not 999
      expect_gt(swSite_IntrinsicSiteParams(sw_input)[["Slope"]], 0L)
      expect_false(
        swSite_IntrinsicSiteParams(sw_input)[["Aspect"]] %in% c(NA, swmiss)
      )
    }

  }
})
