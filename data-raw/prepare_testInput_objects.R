#!/usr/bin/env Rscript

dir_in <- file.path("inst", "extdata")
dir_out <- file.path("tests", "testthat")

tests <- 1:3
examples <- paste0("example", tests)

for (it in seq_along(tests)) {
  dir_temp_out <- file.path(dir_in, examples[it], "Output")
  dir.create(dir_temp_out, showWarnings = FALSE)

  #---INPUTS
  sw_input <- rSOILWAT2::sw_inputDataFromFiles(file.path(dir_in, examples[it]),
    files.in = "files_v31.in")

  sw_weather <- methods::slot(sw_input, "weatherHistory")
  methods::slot(sw_input, "weatherHistory") <- methods::slot(sw_input, "weatherHistory")[1]

  #---OUTPUTS
  saveRDS(sw_weather, file = file.path(dir_out, paste0("Ex", it, "_weather.rds")))
  saveRDS(sw_input, file = file.path(dir_out, paste0("Ex", it, "_input.rds")))

  #--- CLEAN-UP
  unlink(dir_temp_out, recursive = TRUE)
}
