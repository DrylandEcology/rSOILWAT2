
dir_test_data <- file.path("..", "test_data")
temp <- list.files(dir_test_data, pattern = "Ex")
temp <- sapply(strsplit(temp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(temp)

test_that("Test data availability", {
  expect_gt(length(tests), 0)
})


test_that("Plant establishment: input files", {
  expect_gte(length(tests), 6L)
  it <- tests[[6L]]

  swin <- readRDS(file.path(dir_test_data, paste0(it, "_input.rds")))
  sw_weather <- readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))

  # Execute the simulation run
  swout <- rSOILWAT2::sw_exec(
    inputData = swin,
    weatherList = sw_weather,
    quiet = TRUE
  )
  res_estab <- swout@ESTABL@Year

  # Expect species columns
  expect_gt(nrow(res_estab), 1L)

  # Expect some non-zero output for each species
  expect_true(all(colSums(res_estab[, -1L, drop = FALSE]) > 0))
})


test_that("Plant establishment: user specified", {
  swin <- rSOILWAT2::sw_exampleData

  # Turn on output for establishment module
  activate_swOUT_OutKey(swin) <- "ESTABL"

  # turn on sumtype for every output (for simplicity)
  # actual sumtype doesn't matter for establishment
  slot(slot(swin, "output"), "sumtype")[] <- 1L
  # ideally, this should be (but it uses `:::` operator):
  # nolint start: commented_code_linter.
  # tmp <- rSOILWAT2:::rSW2_glovars[["kSOILWAT2"]][["OutKeys"]] %in% "ESTABL"
  # slot(slot(swin, "output"), "sumtype")[tmp] <- 1L
  # nolint end: commented_code_linter.

  # Turn on establishment module
  swin@estab@useEstab <- TRUE

  # Provide establishment parameter values
  test_names <- c("sp1", "sp2", "sp3")
  n <- length(test_names)

  swin@estab <- new(
    "swEstab",
    useEstab = TRUE,
    count = n,
    fileName = test_names,
    Name = test_names,
    vegType = rep(3L, n),
    estab_lyrs = c(2L, 3L, 1L),
    barsGERM = c(10, 10, 5L),
    barsESTAB = c(15, 15, 10L),
    min_pregerm_days = c(60L, 200L, 0L),
    max_pregerm_days = c(180L, 365L, 365L),
    min_wetdays_for_germ = c(2L, 6L, 1L),
    max_drydays_postgerm = c(40L, 45L, 5L),
    min_wetdays_for_estab = c(5L, 6L, 10L),
    min_days_germ2estab = c(15L, 15L, 0L),
    max_days_germ2estab = c(75L, 90L, 100L),
    min_temp_germ = c(5, 10, 0),
    max_temp_germ = c(20, 30, 35),
    min_temp_estab = c(0, 3, 0),
    max_temp_estab = c(20, 30, 15)
  )

  # Execute the simulation run
  swout <- rSOILWAT2::sw_exec(inputData = swin, quiet = TRUE)
  res_estab <- swout@ESTABL@Year

  # Expect species columns
  expect_gt(nrow(res_estab), 1L)

  # Expect species names as columns
  expect_equal(colnames(res_estab)[-1L], expected = test_names)

  # Expect some non-zero output for each species
  expect_true(all(colSums(res_estab[, -1L, drop = FALSE]) > 0))
})
