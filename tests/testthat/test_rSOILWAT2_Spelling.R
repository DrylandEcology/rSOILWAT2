context("Spell checks")

#--- Inputs
pkg_path <- pkg_temp_dir()



#--- Spell check
test_that("Package spell checks", {
  # Check locally and on travis
  skip_on_cran()
  skip_on_appveyor()

  # Check which package is available for spell-checking
  do_spelling <- requireNamespace("spelling", quietly = TRUE) &&
    utils::packageVersion("spelling") >= "1.1.0"
  has_old_devtools <- if (do_spelling) FALSE else {
      requireNamespace("devtools", quietly = TRUE) &&
      utils::packageVersion("devtools") < "2.0.0"
  }

  # Spell check with `hunspell` as backend:
  if (do_spelling) {
    # TODO: turn spell-checking on for vignettes
    temp <- spelling::spell_check_package(pkg_path, vignettes = FALSE)
    misspelled <- temp[["word"]]

  } else if (has_old_devtools) {
    # old devtools version with `ignore` argument
    #   ignores text in roxygen2 content directives, e.g., \url{}, \var{},
    #   but checks text in roxygen2 formatting directives, e.g., \sQuote{}

    # ignore additional words (keep as short as possible)
    spell_ignores <- scan(file.path(pkg_path, "inst", "WORDLIST"),
      what = "character", comment.char = "#", quiet = TRUE)

    misspelled <- devtools::spell_check(pkg = pkg_path, ignore = spell_ignores)

  } else {
    skip("No spell-checker found.")
  }

  expect_identical(length(misspelled), 0L, info = print(misspelled))
})
