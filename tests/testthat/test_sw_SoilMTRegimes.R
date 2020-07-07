context("Calculate soil moisture/texture regimes")

#---INPUTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)


STR_tests <- list(
  Ex1 = "Cryic",
  Ex2 = NULL,
  Ex3 = "Cryic",
  Ex4 = "Cryic",
  Ex5 = "Cryic"
)

SMR_tests <- list(
  Ex1 = c("Ustic", "Typic-Tempustic"),
  Ex2 = NULL,
  Ex3 = c("Ustic", "Typic-Tempustic"),
  Ex4 = c("Ustic", "Typic-Tempustic"),
  Ex5 = c("Ustic", "Typic-Tempustic")
)


test_that("Test data availability", {
  expect_gt(length(tests), 0)
  expect_equal(length(tests), length(STR_tests))
  expect_equal(length(tests), length(SMR_tests))
})


#---TESTS
test_that("SMTR", {
  for (it in tests) {
    info1 <- paste("test-data", it)

    #--- INPUTS
    sw_in <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))

    # TODO(drs) turn off don't test weather generated runs
    if (!swWeather_UseMarkov(sw_in)) {
      sw_weather <- readRDS(
        file = file.path(dir_test_data, paste0(it, "_weather.rds"))
      )

      #--- Run SOILWAT2
      sw_out <- sw_exec(inputData = sw_in, weatherList = sw_weather)


      #--- Calculate soil moisture/texture regimes
      SMTR <- calc_SMTRs(sim_in = sw_in, sim_out = sw_out)

      expect_true(SMTR[["regimes_done"]])

      expect_true(all(colnames(SMTR[["STR"]]) %in% STR_names()))

      expect_true(
        all(colnames(SMTR[["SMR"]]) %in% c(SMR_names(), SMRq_names()))
      )

      expect_equivalent(
        SMTR[["STR"]][, STR_tests[[it]]],
        rep(1, length(STR_tests[[it]])),
        info = info1
      )

      expect_equivalent(
        SMTR[["SMR"]][, SMR_tests[[it]]],
        rep(1, length(SMR_tests[[it]])),
        info = info1
      )
    }
  }
})
