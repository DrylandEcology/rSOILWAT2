context("Calculate soil moisture/texture regimes")

#---INPUTS
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_"), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", expect_gt(length(tests), 0))

STR_tests <- c(Ex1 = "Cryic", Ex2 = "Gelic", Ex3 = "Cryic", Ex4 = "Cryic")
SMR_tests <- c(Ex1 = "Perudic", Ex2 = "Perudic", Ex3 = "Perudic",
  Ex4 = "Perudic")

#---TESTS

test_that("SMTR", {
  for (it in tests) {
    #--- INPUTS
    sw_in <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
    sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

    #--- Run SOILWAT2
    sw_out <- sw_exec(inputData = sw_in, weatherList = sw_weather)


    #--- Calculate soil moisture/texture regimes
    SMTR <- calc_SMTRs(sim_in = sw_in, sim_out = sw_out)

    expect_true(SMTR[["regimes_done"]])

    expect_true(all(colnames(SMTR[["STR"]]) %in% STR_names()))
    expect_true(all(colnames(SMTR[["SMR"]]) %in% c(SMR_names(), SMRq_names())))

    expect_equivalent(SMTR[["STR"]][, STR_tests[it]], 1)
    expect_equivalent(SMTR[["SMR"]][, SMR_tests[it]], 1)
  }
})
