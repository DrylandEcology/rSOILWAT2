#--- Code style

#nolint start

# Problem: `R CMD check` doesn't allow hidden files
# including the lintr settings; thus, we exclude `.lintr` via `.Rbuildignore`.
# Consequently, we wouldn't be able to lint with our settings
# during package checks with `R CMD check`/`devtools::check()`.

# ==> Configure linters here in code instead of via file `.lintr`


# The linting code can be run via any the following options
# (assuming the current working directory is at the root of the source package)
#   - `Sys.setenv(NOT_CRAN = "true"); source("tests/lint_code.R")`
#   - `devtools::check(env_vars = c(NOT_CRAN = "true"))`
#   - `R CMD build . && NOT_CRAN="true" R CMD check *.tar.gz`

#nolint end

if (
  requireNamespace(
    "lintr",
    versionCheck = list(op = ">=", version = "2.0")
  ) &&
  # skip_on_cran
  isTRUE(tolower(Sys.getenv("NOT_CRAN")) %in% c(1, "yes", "true")) &&
  # skip_on_appveyor
  !isTRUE(tolower(Sys.getenv("APPVEYOR")) %in% c(1, "yes", "true")) &&
  # skip_on_covr
  !isTRUE(tolower(Sys.getenv("R_COVR")) %in% c(1, "yes", "true"))
) {

  # Locate package source directory
  is_package_source_path <- function(path) {
    dir.exists(path) && file.exists(file.path(path, "DESCRIPTION"))
  }

  # During interactive session and sourcing the file
  pkg_path <- "."

  # During unit testing, the current path is set to `tests/testthat/`
  if (!is_package_source_path(pkg_path)) {
    pkg_path <- file.path("..", "..")

    if (!is_package_source_path(pkg_path)) {
      # During package checks, the current path is a temporary build directory
      # Code chunk based on `spelling::spell_check_test`
      if (!is_package_source_path(pkg_path)) {
        pkg_path <- list.files(file.path("..", "00_pkg_src"), full.names = TRUE)

        if (!length(pkg_path)) {
          check_dir <- dirname(getwd())
          if (grepl("\\.Rcheck$", check_dir)) {
            source_dir <- sub("\\.Rcheck$", "", check_dir)
            if (file.exists(source_dir)) {
              pkg_path <- source_dir
            }
          }
        }
      }
    }
  }


  if (is_package_source_path(pkg_path)) {

    #--- List files that shouldn't be linted
    files_not_tolint <- file.path(
      "R",
      c("RcppExports.R", "rSOILWAT2_deprecated.R")
    )

    #--- List of linters to apply to package code
    linters_config <- lintr::with_defaults(
      #------ DEFAULT LINTERS
      assignment_linter = lintr::assignment_linter,
      closed_curly_linter =
        lintr::closed_curly_linter(allow_single_line = TRUE),
      commas_linter = lintr::commas_linter,
      commented_code_linter = lintr::commented_code_linter,
      equals_na_linter = lintr::equals_na_linter,
      function_left_parentheses_linter =
        lintr::function_left_parentheses_linter,
      infix_spaces_linter = lintr::infix_spaces_linter,
      line_length_linter = lintr::line_length_linter(length = 80L),
      no_tab_linter = lintr::no_tab_linter,
      object_length_linter = lintr::object_length_linter,
      object_usage_linter = lintr::object_usage_linter,
      open_curly_linter = lintr::open_curly_linter(allow_single_line = TRUE),
      paren_brace_linter = lintr::paren_brace_linter,
      pipe_continuation_linter = lintr::pipe_continuation_linter,
      seq_linter = lintr::seq_linter,
      single_quotes_linter = lintr::single_quotes_linter,
      spaces_inside_linter = lintr::spaces_inside_linter,
      spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
      trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
      trailing_whitespace_linter = lintr::trailing_whitespace_linter,
      #--- Turn off default linters for now:
      object_name_linter = NULL,
      cyclocomp_linter = NULL,
      #------ NON-DEFAULT LINTERS
      #--- Not activated non-default linters:
      #lintr::extraction_operator_linter,
      #lintr::implicit_integer_linter,
      #lintr::todo_comment_linter,
      # see https://github.com/jimhester/lintr/issues/468
      #lintr::nonportable_path_linter(lax = TRUE),
      #--- Activated non-default linters:
      absolute_path_linter = lintr::absolute_path_linter(lax = TRUE),
      infix_spaces_linter = lintr::infix_spaces_linter,
      T_and_F_symbol_linter = lintr::T_and_F_symbol_linter,
      semicolon_terminator_linter = lintr::semicolon_terminator_linter(
        semicolon = c("compound", "trailing")
      ),
      undesirable_function_linter = lintr::undesirable_function_linter,
      undesirable_operator_linter = lintr::undesirable_operator_linter,
      unneeded_concatenation_linter = lintr::unneeded_concatenation_linter
    )


    #--- Lint package code
    # `lintr::expect_lint_free` and `lintr::lint_package` lint R code
    # only in "R", "tests", "inst"
    paths <- file.path(
      pkg_path,
      c("data-raw", "demo", "R", "tests", "inst")
    )

    # Exclude vignette code from this step here which may be
    # located at "inst/doc" (see below)
    vignette_files <- list.files(
      path = file.path(pkg_path, "inst", "doc"),
      pattern = "\\.[Rr]$",
      recursive = FALSE,
      full.names = FALSE
    )

    # Prepare exclusions
    # i.e., a named list of file paths relative to `path` argument and with
    # `Inf` as values (see code of `lintr::lint_dir`; as of `lintr` v2.0.1)
    files_to_exclude <- c(
      if (length(files_not_tolint) > 0) {
        file.path(pkg_path, files_not_tolint)
      },
      if (length(vignette_files) > 0) {
        file.path(pkg_path, "inst", "doc", vignette_files)
      }
    )

    excluded_files <- as.list(rep(Inf, length(files_to_exclude)))
    names(excluded_files) <- files_to_exclude

    lints1 <- lintr::lint_dir(
      path = paths[dir.exists(paths)],
      exclusions = excluded_files,
      linters = linters_config,
      parse_settings = FALSE,
      relative_path = FALSE # TRUE assumes that argument path is of length 1
    )


    #--- Lint code from vignettes
    # (extracted by building the vignette(s) from vignette.Rmd -
    # an automatic process which adds an extra trailing blank line)
    linters_config[["trailing_blank_lines_linter"]] <- NULL

    # Locate vignette code
    # During interactive development located at pkg_path/doc
    path_vignette_code <- file.path(pkg_path, "doc")

    if (!dir.exists(path_vignette_code)) {
      # During build/check located at pkg_path/inst/doc/
      path_vignette_code <- file.path(pkg_path, "inst", "doc")
    }


    if (dir.exists(path_vignette_code)) {
      lints2 <- lintr::lint_dir(
        path = path_vignette_code,
        linters = linters_config,
        parse_settings = FALSE,
        relative_path = FALSE # TRUE assumes that argument path is of length 1
      )

    } else {
      lints2 <- NULL
    }

    has_lints <- c(length(lints1) > 0, length(lints2) > 0)
    if (any(has_lints)) {
      if (has_lints[1]) print(lints1)
      if (has_lints[2]) print(lints2)
      stop("Not lint free.")
    }

  } else {
    warning(
      "No linting: failed to find package source at ",
      shQuote(normalizePath(pkg_path, mustWork = FALSE))
    )
  }
}
