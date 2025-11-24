
test_that("Class constructors", {

  ref <- rSOILWAT2::sw_exampleData
  expect_s4_class(ref, "swInputData")

  sns <- slotNames(ref)
  sns_dc <- setdiff(sns, c("timestamp", "version"))

  tmp <- vapply(
    sns_dc,
    function(x) class(slot(ref, x)),
    FUN.VALUE = NA_character_
  )
  list_classes <- setdiff(tmp, c("list", "swLog", "swProd"))


  #--- Check that constructor helper for "swInputData" reproduces (valid) inputs
  expect_s4_class(swInputData(), "swInputData")
  x <- swInputData(ref)
  expect_s4_class(x, "swInputData")

  for (ks in sns_dc) {
    expect_true(validObject(slot(x, ks)))
    expect_equal(slot(x, ks), slot(ref, ks))
  }


  #--- Loop over classes ------
  for (cn in list_classes) {
    fun_constructor <- match.fun(cn)
    fun_extractor <- match.fun(paste0("get_", cn))

    #--- Check that constructor helper for each class reproduces inputs
    expect_s4_class(fun_constructor(), cn)
    x <- fun_constructor(fun_extractor(ref))
    expect_s4_class(x, cn)
    expect_equal(x, fun_extractor(ref))

    #--- Check that new class objects are valid
    expect_true(validObject(new(cn)))
    expect_true(validObject(fun_constructor()))
  }


  #--- Check special case: "swWeatherData"
  expect_s4_class(swWeatherData(), "swWeatherData")
  x <- swWeatherData(get_swWeatherData(ref, 1980))
  expect_s4_class(x, "swWeatherData")
  expect_equal(x, get_swWeatherData(ref, 1980))


  #--- Check special case: "weatherHistory"
  expect_type(weatherHistory(), "list")
  expect_s4_class(weatherHistory()[[1]], "swWeatherData")
  x <- weatherHistory(get_WeatherHistory(ref))
  expect_type(x, "list")
  expect_s4_class(x[[1]], "swWeatherData")
  expect_equal(x, get_WeatherHistory(ref))


  #--- Check special case: "swLog"
  expect_s4_class(swLog(), "swLog")
  x <- swLog(ref@log)
  expect_s4_class(x, "swLog")
  expect_equal(x, ref@log)
})
