
test_that("Class constructors", {

  ref <- rSOILWAT2::sw_exampleData
  expect_s4_class(ref, "swInputData")
  sns <- slotNames(ref)
  sns_dc <- setdiff(sns, c("timestamp", "version"))

  #--- Check that constructor helper for "swInputData" reproduces inputs
  expect_s4_class(swInputData(), "swInputData")
  x <- swInputData(ref)
  expect_s4_class(x, "swInputData")

  for (ks in sns_dc) {
    expect_equal(slot(x, ks), slot(ref, ks))
  }


  #--- Check that constructor helper for "swFiles" reproduces inputs
  expect_s4_class(swFiles(), "swFiles")
  x <- swFiles(get_swFiles(ref))
  expect_s4_class(x, "swFiles")
  expect_equal(x, get_swFiles(ref))

  #--- Check that constructor helper for "swYears" reproduces inputs
  expect_s4_class(swYears(), "swYears")
  x <- swYears(get_swYears(ref))
  expect_s4_class(x, "swYears")
  expect_equal(x, get_swYears(ref))

  #--- Check that constructor helper for "swWeather" reproduces inputs
  expect_s4_class(swWeather(), "swWeather")
  x <- swWeather(get_swWeather(ref))
  expect_s4_class(x, "swWeather")
  expect_equal(x, get_swWeather(ref))

  #--- Check that constructor helper for "swWeatherData" reproduces inputs
  expect_s4_class(swWeatherData(), "swWeatherData")
  x <- swWeatherData(get_swWeatherData(ref, 1980))
  expect_s4_class(x, "swWeatherData")
  expect_equal(x, get_swWeatherData(ref, 1980))

  #--- Check that constructor helper for "weatherHistory" reproduces inputs
  expect_type(weatherHistory(), "list")
  expect_s4_class(weatherHistory()[[1]], "swWeatherData")
  x <- weatherHistory(get_WeatherHistory(ref))
  expect_type(x, "list")
  expect_s4_class(x[[1]], "swWeatherData")
  expect_equal(x, get_WeatherHistory(ref))

  #--- Check that constructor helper for "swCloud" reproduces inputs
  expect_s4_class(swCloud(), "swCloud")
  x <- swCloud(get_swCloud(ref))
  expect_s4_class(x, "swCloud")
  expect_equal(x, get_swCloud(ref))

  #--- Check that constructor helper for "swMarkov" reproduces inputs
  expect_s4_class(swMarkov(), "swMarkov")
  x <- swMarkov(get_swMarkov(ref))
  expect_s4_class(x, "swMarkov")
  expect_equal(x, get_swMarkov(ref))

  #--- Check that constructor helper for "swProd" reproduces inputs
  expect_s4_class(swProd(), "swProd")
  x <- swProd(get_swProd(ref))
  expect_s4_class(x, "swProd")
  expect_equal(x, get_swProd(ref))

  #--- Check that constructor helper for "swSite" reproduces inputs
  expect_s4_class(swSite(), "swSite")
  x <- swSite(get_swSite(ref))
  expect_s4_class(x, "swSite")
  expect_equal(x, get_swSite(ref))

  #--- Check that constructor helper for "swSoils" reproduces inputs
  expect_s4_class(swSoils(), "swSoils")
  x <- swSoils(get_swSoils(ref))
  expect_s4_class(x, "swSoils")
  expect_equal(x, get_swSoils(ref))

  #--- Check that constructor helper for "swEstab" reproduces inputs
  expect_s4_class(swEstab(), "swEstab")
  x <- swEstab(get_swEstab(ref))
  expect_s4_class(x, "swEstab")
  expect_equal(x, get_swEstab(ref))

  #--- Check that constructor helper for "swCarbon" reproduces inputs
  expect_s4_class(swCarbon(), "swCarbon")
  x <- swCarbon(get_swCarbon(ref))
  expect_s4_class(x, "swCarbon")
  expect_equal(x, get_swCarbon(ref))

  #--- Check that constructor helper for "swOUT" reproduces inputs
  expect_s4_class(swOUT(), "swOUT")
  x <- swOUT(get_swOUT(ref))
  expect_s4_class(x, "swOUT")
  expect_equal(x, get_swOUT(ref))

  #--- Check that constructor helper for "swSWC" reproduces inputs
  expect_s4_class(swSWC(), "swSWC")
  x <- swSWC(get_swSWC(ref))
  expect_s4_class(x, "swSWC")
  expect_equal(x, get_swSWC(ref))

  #--- Check that constructor helper for "swLog" reproduces inputs
  expect_s4_class(swLog(), "swLog")
  x <- swLog(ref@log)
  expect_s4_class(x, "swLog")
  expect_equal(x, ref@log)
})
