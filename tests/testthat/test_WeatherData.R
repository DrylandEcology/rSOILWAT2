
test_that("Weather data check", {
  expect_false(dbW_check_weatherData(NA))
  expect_false(dbW_check_weatherData(NULL))
  expect_false(dbW_check_weatherData(1))
  expect_false(dbW_check_weatherData(list()))
  expect_false(dbW_check_weatherData(list(1)))
  expect_false(dbW_check_weatherData(swWeatherData()))
  expect_false(dbW_check_weatherData(weatherHistory()))
  expect_false(dbW_check_weatherData(
    swWeatherData(rSOILWAT2::weatherData[[1]]))
  )

  expect_true(dbW_check_weatherData(rSOILWAT2::weatherData))
  expect_true(dbW_check_weatherData(weatherHistory(rSOILWAT2::weatherData)))
  expect_true(dbW_check_weatherData(
    list(swWeatherData(rSOILWAT2::weatherData[[1]]))
  ))
  expect_true(dbW_check_weatherData(
    get_WeatherHistory(rSOILWAT2::sw_exampleData)
  ))
})
