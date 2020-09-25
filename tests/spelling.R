if (requireNamespace("spelling", quietly = TRUE)) {

  # as of `spelling` v2.1: works only during `R CMD check`/`devtools::check()`
  # because it searches for package code at
  # either `../00_pkg_src/` or `../.Rcheck/`
  spelling::spell_check_test(
    vignettes = TRUE,
    error = TRUE,
    skip_on_cran = TRUE
  )
}
