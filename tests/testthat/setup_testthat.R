# `setup_*.R`: executed before tests, but not during `devtools::load_all()`
# (see `?test_check`)


# Environmental variable `RSOILWAT_ALLTESTS` determines whether or not
# long-running expectations/unit-tests are skipped;
# default is "true", i.e., run all expectations/unit-tests.
# To set to false, e.g.,
#   * \code{Sys.setenv(RSOILWAT_ALLTESTS = "false"); devtools::test()}
#   * \code{RSOILWAT_ALLTESTS="false" R CMD check *tar.gz}
if (!nzchar(Sys.getenv("RSOILWAT_ALLTESTS"))) {
  Sys.setenv(RSOILWAT_ALLTESTS = "true")
}


# Environmental variable `RSOILWAT_INTEGRATIONTESTS` determines whether or not
# integration-tests are skipped;
# default is "false", i.e., skipp all integration-tests.
# To set to true, e.g.,
#   * \code{Sys.setenv(RSOILWAT_INTEGRATIONTESTS = "true"); devtools::test()}
#   * \code{RSOILWAT_INTEGRATIONTESTS="true" R CMD check *tar.gz}
if (!nzchar(Sys.getenv("RSOILWAT_INTEGRATIONTESTS"))) {
  Sys.setenv(RSOILWAT_INTEGRATIONTESTS = "false")
}



# Make sure that environmental variable \code{NOT_CRAN} is set to either
# \var{\dQuote{true}} or \var{\dQuote{false}}:
# \var{\dQuote{NOT_CRAN="true"}} should be set if run with:
#   * \code{devtools::test()} unless \code{NOT_CRAN} previously set
#   * \code{devtools::check(cran = FALSE)}
#   * \code{R CMD check *tar.gz}
# \var{\dQuote{NOT_CRAN="false"}} should be set if run with:
#   * \code{Sys.setenv(NOT_CRAN = "false"); devtools::test()}
#   * \code{devtools::check(cran = TRUE)}
#   * \code{R CMD check *tar.gz --as-cran}

# \code{testthat::skip_on_cran()} requires the value of \var{\dQuote{true}}
# for it to not skip. However, only \pkg{devtools} sets \code{NOT_CRAN}.
# For instance, `R CMD check *tar.gz` without `--as-cran`) does not set
# \code{NOT_CRAN} and thus, \code{testthat::skip_on_cran()} skips even
# though unintended.

if (!nzchar(Sys.getenv("NOT_CRAN"))) {
  # `NOT_CRAN` is not set

  # Hack to guess whether `R CMD check *tar.gz` is run with/without `--as-cran`;
  # see section `if (as_cran) {Sys.setenv(...); ...}` of function
  # \code{tools:::.check_packages}:
  # NOTE: this likely changes from R version to R version!

  # currently, up-to-date for R version 3.5.0
  ascran_env_vals <- list(
    `_R_CHECK_CODE_USAGE_VIA_NAMESPACES_` = "TRUE",
    `_R_CHECK_CODE_USAGE_WITH_ONLY_BASE_ATTACHED_` = "TRUE",
    `_R_CHECK_COMPILATION_FLAGS_` = "TRUE",
    `_R_CHECK_DOT_FIRSTLIB_` = "TRUE",
    `_R_CHECK_INSTALL_DEPENDS_` = "TRUE",
    `_R_CHECK_NATIVE_ROUTINE_REGISTRATION_` = "TRUE",
    `_R_CHECK_NO_RECOMMENDED_` = "TRUE",
    `_R_CHECK_NO_STOP_ON_TEST_ERROR_` = "TRUE",
    `_R_CHECK_PACKAGE_DATASETS_SUPPRESS_NOTES_` = "TRUE",
    `_R_CHECK_PACKAGES_USED_CRAN_INCOMING_NOTES_` = "TRUE",
    `_R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_` = "TRUE",
    `_R_CHECK_PRAGMAS_` = "TRUE",
    `_R_CHECK_R_DEPENDS_` = "warn",
    `_R_CHECK_S3_METHODS_NOT_REGISTERED_` = "TRUE",
    `_R_CHECK_TIMINGS_` = "10"
  )

  temp <- Sys.getenv(names(ascran_env_vals), names = TRUE)
  temp <- toupper(temp) == toupper(ascran_env_vals)
  guess_ascran <- mean(temp)

  if (guess_ascran > 0.9) {
    Sys.setenv(NOT_CRAN = "false")
  } else {
    Sys.setenv(NOT_CRAN = "true")
  }
}


# Print environmental variables that are important for running our tests:
temp <- c("RSOILWAT_ALLTESTS", "RSOILWAT_INTEGRATIONTESTS",
  "NOT_CRAN", "TRAVIS", "APPVEYOR", "TESTTHAT")
temp <- Sys.getenv(temp, names = TRUE)

message(
  "Testing environment is run with:",
  paste(
    paste("\n\t*", names(temp), "=", shQuote(temp)),
    collapse = ""
  )
)
