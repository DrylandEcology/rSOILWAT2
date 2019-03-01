context("Spell checks")

#--- Inputs
pkg_path <- pkg_temp_dir()



#--- Spell check
test_that("Package spell checks", {
  # Check locally and on travis
  skip_on_cran()
  skip_on_appveyor()
  skip_if_not_installed("spelling", minimum_version = "1.1.0")

  # TODO: turn spell-checking on for vignettes
  temp <- spelling::spell_check_package(pkg_path, vignettes = FALSE)
  misspelled <- temp[["word"]]

  expect_identical(length(misspelled), 0L, info = print(misspelled))
})
