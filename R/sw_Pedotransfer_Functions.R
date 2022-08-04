#' Pedotransfer functions to convert between soil moisture (volumetric water
#' content, \var{VWC}) and soil water potential (\var{SWP})
#'
#' @param sand A numeric value or vector. Sand content of the soil layer(s) as
#'   fractional value in \code{[0,1]}.
#' @param clay A numeric value or vector. Clay content of the soil layer(s) as
#'   fractional value in \code{[0,1]}.
#'
#' @references Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn.
#' 1984. A statistical exploration of the relationships of soil moisture
#' characteristics to the physical properties of soils. Water Resources Research
#' 20:682-690.
#'
#' @name pedotransfer
NULL

#' @rdname pedotransfer
#' @section Note: either \code{swp} or \code{sand}/\code{clay} needs be a
#'   single value
pdf_to_vwc <- function(swp, sand, clay, thetas, psis, b, MPa_toBar = -10,
  bar_conversion = 1024) {

  thetas * (psis / (swp * MPa_toBar * bar_conversion)) ^ (1 / b) / 100
}

#' @rdname pedotransfer
#' @section Note: either \code{vwc} or \code{sand}/\code{clay} needs be a
#'   single value
pdf_to_swp <- function(vwc, sand, clay, thetas, psis, b, bar_toMPa = -0.1,
  bar_conversion = 1024) {

  psis / ((vwc * 100 / thetas) ^ b * bar_conversion) * bar_toMPa
}

pedotransfer <- function(x, sand, clay, pdf) {
  stopifnot(
    length(sand) > 0,
    length(sand) == length(clay)
  )
  sand <- rSW2utils::finite01(sand, NA, NA)
  clay <- rSW2utils::finite01(clay, NA, NA)

  if (any(complete.cases(sand, clay))) {
    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    if (any(b <= 0, na.rm = TRUE))
      stop("Pedotransfer for soil texture with b <= 0 is not possible.")

    np_x <- NROW(x) * NCOL(x)

    if (NROW(x) == 1 || NCOL(x) == 1) {
      # cases 1-4
      if (np_x == 1 || length(sand) == 1) {
        # cases 1-3
        res <- pdf(x, sand, clay, thetas, psis, b)

      } else {
        # case 4; Note: case 3 could also be calculated with the code for
        # case 4, but is much slower, unless x is a data.frame with one column
        temp <- lapply(x, function(v) pdf(v, sand, clay, thetas, psis, b))
        res <- matrix(unlist(temp), nrow = np_x, byrow = TRUE)
      }

    } else {
      # cases 5-6
      dx <- dim(x)

      if (length(sand) == 1) {
        # case 5
        res <- vapply(seq_len(dx[2]), function(d) {
            pdf(x[, d], sand, clay, thetas, psis, b)
          }, rep(1, dx[1]), USE.NAMES = FALSE)

      } else {
        # case 6
        stopifnot(dx[2] == length(sand))
        res <- vapply(seq_len(dx[2]), function(d) {
            pdf(x[, d], sand[d], clay[d], thetas[d], psis[d], b[d])
          }, rep(1, dx[1]), USE.NAMES = FALSE)
      }
    }

  } else {
    res <- x
    res[] <- NA
  }

  # if SWP then in units of MPa [-Inf, 0]; if VWC then in units of m3/m3 [0, 1]
  res
}

#' Calculate volumetric water content from soil water potential and soil texture
#' @rdname pedotransfer
#' @param swp A numeric value, vector, or 2-dimensional object
#'   (matrix or data.frame). The soil water potential (of the soil matrix) in
#'   units of \var{MPa}, i.e., the soil without the volume of rock and gravel.
#'
#' @return Volumetric water content in units of m^3 (of water) / m^3 (of soil)
#'  \code{[0, 1]}. There are six use cases:\enumerate{
#'    \item 1) \itemize{
#'      \item Input: \code{SWP} [single value]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{VWC} [single value]}
#'    \item 2) \itemize{
#'      \item Input: \code{SWP} [single value]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{VWC} [vector of length d]}
#'    \item 3) \itemize{
#'      \item Input: \code{SWP} [vector of length l]; \code{sand} and
#'        \code{clay} infraction [single values]
#'      \item Output: \code{VWC} [vector of length l]}
#'    \item 4) \itemize{
#'      \item Input: \code{SWP} [vector of length l]; \code{sand} and
#'        \code{clay} [vectors of length d]
#'      \item Output: \code{VWC} [l x d matrix] where \code{SWP} is
#'        repeated for each column}
#'    \item 5) \itemize{
#'      \item Input: \code{SWP} [l x d matrix]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{VWC} [l x d matrix]}
#'    \item 6) \itemize{
#'      \item Input: \code{SWP} [l x d matrix]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{VWC} [l x d matrix], \code{sand} and \code{clay}
#'        vectors are repeated for each row}
#'  }
#' @export
SWPtoVWC <- function(swp, sand, clay, ...) {
  .Deprecated("swrc_swp_to_vwc")

  swrc_swp_to_vwc(
    swp_MPa = swp,
    sand = sand,
    clay = clay,
    ...
  )
}

SWPtoVWC_old <- function(swp, sand, clay) {
  pedotransfer(swp, sand, clay, pdf = pdf_to_vwc)
}


#' Calculate soil water potential from volumetric water content and soil texture
#' @rdname pedotransfer
#' @param vwc A numeric value, vector, or 2-dimensional object
#'   (matrix or data.frame). The matric soil moisture, i.e., reduced by the
#'   volume of rock and gravel.
#'
#' @return Soil water potential in units of \var{MPa} \code{[-Inf, 0]}.
#'   There are six use cases: \enumerate{
#'    \item 1) \itemize{
#'      \item Input: \code{VWC} [single value]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{SWP} [single value]}
#'    \item 2) \itemize{
#'      \item Input: \code{VWC} [single value]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{SWP} [vector of length d]}
#'    \item 3) \itemize{
#'      \item Input: \code{VWC} [vector of length l]; \code{sand} and
#'        \code{clay} in fraction [single values]
#'      \item Output: \code{SWP} [vector of length l]}
#'    \item 4) \itemize{
#'      \item Input: \code{VWC} [vector of length l]; \code{sand} and
#'        \code{clay} [vectors of length d]
#'      \item Output: \code{SWP} [l x d matrix] where \code{VWC} is repeated for
#'        each column}
#'    \item 5) \itemize{
#'      \item Input: \code{VWC} [l x d matrix]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{SWP} [l x d matrix]}
#'    \item 6) \itemize{
#'      \item Input: \code{VWC} [l x d matrix]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{SWP} [l x d matrix], \code{sand} and \code{clay}
#'        vectors are repeated for each row}
#'  }
#' @export
VWCtoSWP <- function(vwc, sand, clay, ...) {
  .Deprecated("swrc_vwc_to_swp")

  swrc_vwc_to_swp(
    vwcBulk = vwc,
    sand = sand,
    clay = clay,
    ...
  )
}

VWCtoSWP_old <- function(vwc, sand, clay) {
  pedotransfer(vwc, sand, clay, pdf = pdf_to_swp)
}




#------ SWRC parameters & pedotransfer functions ------

# MAINTENANCE:
# Notes for implementing a new SWRC "XXX" and corresponding PDF "YYY"
#   1) SOILWAT2: see notes in SOILWAT2/SW_Site.h, i.e.,
#      --> updated `N_SWRCs`, `N_PDFs`, `swrc2str[]`, `pdf2str[]`
#      --> updated `check_SWRC_vs_PDF()`, `SWRC_PDF_estimate_parameters()`,
#          `SWRC_check_parameters()`, `SWRC_SWCtoSWP()`, `SWRC_SWPtoSWC()`
#      --> new `SWRC_check_parameters_for_XXX()`, `SWRC_PDF_YYY_for_XXX()`,
#          `SWRC_SWCtoSWP_XXX()`, `SWRC_SWPtoSWC_XXX()`
#
#   2) rSOILWAT2:
#     * if "YYY" is implemented in R, then:
#       * new `pdf_YYY_for_XXX()`
#       * new `pdf_YYY_availability(verbose = interactive(), ...)`
#       * update `pdfs_implemented_in_rSW2()`
#       * update `rSW2_SWRC_PDF_estimate_parameters()`
#       * update `check_pdf_availability()`
#     * update examples and unit tests to utilize new XXX/YYY functions


#' Functionality for Soil Water Retention Curves (`SWRC`)
#'
#' @description
#' `SWRCs` convert between soil water content and soil water potential
#' using a set of parameters, see [swrc_swp_to_vwc()] and [swrc_vwc_to_swp()].
#'
#' The `SWRC` parameters may be estimated from soil properties with suitable
#' pedotransfer functions `PDFs`, see [pdf_estimate()].
#'
#' The `SWRC` parameters can be checked for consistency with [check_swrcp()].
#'
#'
#' @param sand A numeric value or vector.
#'   Sand content of the matric soil component
#'   (< 2 mm fraction; units of `[g/g]`) of each soil layer.
#' @param clay A numeric value or vector.
#'   Clay content of the matric soil component
#'   (< 2 mm fraction; units of `[g/g]`) of each soil layer.
#' @param fcoarse A numeric value or vector.
#'   Coarse fragments, e.g., gravel, (> 2 mm; units of `[m3/m3]`)
#'   relative to the whole soil of each soil layer.
#'   `fcoarse` is required, for instance, to translate between
#'   values relative to the matric soil component (< 2 mm fraction) and
#'   relative to the whole soil (matric soil plus coarse fragments).
#' @param bdensity A numeric value or vector.
#'   Density of the whole soil
#'   (matric soil plus coarse fragments; units `[g/cm3]`).
#' @param layer_width A numeric value or vector.
#'   Depth interval, width, of each soil layer (units of `cm`).
#'   `layer_width` is required to translate between
#'   soil water content of a soil layer and volumetric water content.
#' @param swrc_name An character string or vector.
#'   The selected `SWRC` name
#'   (one of [swrc_names()], with default `"Campbell1974"`).
#' @param pdf_name An character string or vector.
#'   The selected `PDF` name
#'   (one of [pdf_names()], with default `"Cosby1984AndOthers"`).
#' @param swrcp A numeric vector or matrix.
#'   The parameters of a selected `SWRC`;
#'   each row represents one `SWRC`, e.g., one per soil layer.
#' @param swrc A named list.
#'   Contains all necessary elements of a `SWRC`,
#'   i.e., `name` (short for `swrc_name`) and `swrcp`,
#'   or all necessary elements to estimate parameters of a `SWRC` given
#'   soil parameters, i.e., `swrc_name` and `pdf_name`.
#' @param verbose A logical value. If `TRUE`, then display
#'   `SOILWAT2` internal warnings and other messages.
#'
#' @section Details:
#' [swrc_names()] lists implemented `SWRCs`;
#' [pdf_names()] lists implemented `PDFs`.
#'
#' @inherit pdf_Rosetta_for_vanGenuchten1980 references
#' @inherit pdf_neuroFX2021_for_FXW references
#' @references
#'   Cosby, B. J., G. M. Hornberger, R. B. Clapp, & T. R. Ginn. 1984.
#'   A statistical exploration of the relationships of soil moisture
#'   characteristics to the physical properties of soils.
#'   Water Resources Research, 20:682-690, \doi{10.1029/WR020i006p00682}
#'
#' @seealso
#'   [swrc_names()],
#'   [pdf_names()],
#'   [check_pdf_availability()],
#'   [pdf_estimate()],
#'   [check_swrcp()],
#'   [swrc_swp_to_vwc()],
#'   [swrc_vwc_to_swp()]
#'
#' @name SWRCs
#' @md
NULL


#' List Soil Water Retention Curves `SWRCs`
#'
#' @return An integer vector with names of implemented `SWRCs`
#'
#' @details Notes:
#' The integer values may change with new versions of `SOILWAT2.`
#'
#' @seealso [`SWRCs`], [pdf_names()], [check_pdf_availability()]
#'
#' @md
#' @export
swrc_names <- function() {
  rSW2_glovars[["kSOILWAT2"]][["SWRC_types"]]
}

#' List Pedotransfer Functions `PDFs`
#'
#' @return An integer vector with names of implemented `PDFs`
#'
#' @details Notes:
#' The integer values may change with new versions of `SOILWAT2.`
#'
#' @seealso [`SWRCs`], [swrc_names()], [check_pdf_availability()]
#'
#' @md
#' @export
pdf_names <- function() {
  rSW2_glovars[["kSOILWAT2"]][["PDF_types"]]
}


#' Standardize a `SWRC` name
#' @noRd
std_swrc <- function(swrc_name) {
  if (missing(swrc_name) || is.null(swrc_name) || all(is.na(swrc_name))) {
    "Campbell1974"
  } else {
    as.character(swrc_name)
  }
}

#' Standardize a `PDF` name
#' @noRd
std_pdf <- function(pdf_name) {
  if (missing(pdf_name) || is.null(pdf_name) || all(is.na(pdf_name))) {
    "Cosby1984AndOthers"
  } else {
    as.character(pdf_name)
  }
}


#' Translate a `SWRC` name to its internal integer code
#' @noRd
encode_name2swrc <- function(swrc_name) {
  as.integer(unname(swrc_names()[std_swrc(swrc_name)]))
}

#' Translate a `PDF` name to its internal integer code
#' @noRd
encode_name2pdf <- function(pdf_name) {
  as.integer(unname(pdf_names()[std_pdf(pdf_name)]))
}


#' Matching pairs of implemented `SWRCs` and `PDFs`
#'
#' @inheritParams SWRCs
#'
#' @return A `data.frame` with two columns `SWRC` and `PDF` where each
#'   row contains a matching pair of `SWRC` and `PDF` that are implemented.
#'
#' @section Details:
#'   The argument `swrc_name` is optional. If missing, then all implemented
#'   `SWRCs` are listed.
#'   \var{"NoPDF"} is not included in the list.
#'
#' @examples
#' # Data frame of SWRC-PDF combinations
#' df_swrc_pdfs <- rSOILWAT2::list_matched_swrcs_pdfs()
#'
#' # List of SWRC-PDF combinations
#' list_swrcs_pdfs <- unname(as.list(as.data.frame(t(df_swrc_pdfs))))
#'
#' # Available SWRC-PDF combinations
#' has_pdf <- check_pdf_availability(df_swrc_pdfs[, "PDF"])
#' df_swrc_pdfs[has_pdf, , drop = FALSE]
#' list_swrcs_pdfs[has_pdf]
#'
#' @export
list_matched_swrcs_pdfs <- function(swrc_name) {
  swrc_name <- if (
    missing(swrc_name) || is.null(swrc_name) || all(is.na(swrc_name))
  ) {
    names(swrc_names())
  } else {
    as.character(swrc_name)
  }

  res <- expand.grid(
    SWRC = swrc_name,
    PDF = names(pdf_names()[-1]),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  ids <- mapply(
    function(s, p) .Call(C_rSW2_check_SWRC_vs_PDF, s, p),
    res[, "SWRC"],
    res[, "PDF"]
  )

  res <- res[ids, , drop = FALSE]
  rownames(res) <- NULL
  res
}


#' Estimate `SWRC` parameters from soil texture with a pedotransfer function
#'
#' @inheritParams SWRCs
#' @param ... Additional parameters passed to selected `PDF` function.
#'
#' @section Notes:
#' [swrc_names()] lists implemented `SWRCs`;
#' [pdf_names()] lists implemented `PDFs`; and
#' [check_pdf_availability()] checks availability of `PDFs`.
#'
#' @section Notes:
#' The soil parameters `sand`, `clay`, `fcoarse`, and `bdensity` must be of
#' the same length, i.e., represent one soil (length 1) or
#' multiple soil (layers) (length > 1); however, `bdensity` may be `NULL`.
#' The arguments selecting `SWRC` (`swrc_name`) and `PDF` (`pdf_name`)
#' are recycled for multiple soil layers.
#'
#' @inherit SWRCs references
#'
#' @return `swrcp`, i.e,.
#' a numeric matrix where rows represent soil (layers) and
#' columns represent a fixed number of `SWRC` parameters.
#' The interpretation is dependent on the selected `SWRC`, see
#' `SOILWAT2` input file `swrc_param.in`
# nolint start: line_length_linter.
#' (
#' `system.file("extdata", "example1", "Input", "swrc_params.in", package = "rSOILWAT2")`
#' ).
# nolint end
#'
#' @examples
#' pdf_estimate(sand = c(0.5, 0.3), clay = c(0.2, 0.1), fcoarse = c(0, 0))
#'
#' soils <- swSoils_Layers(rSOILWAT2::sw_exampleData)
#'
#' # Use PDF "Cosby1984" to estimate parameters of SWRC "Campbell1974"
#' pdf_estimate(
#'   sand = soils[, "sand_frac"],
#'   clay = soils[, "clay_frac"],
#'   fcoarse = soils[, "gravel_content"],
#'   swrc_name = "Campbell1974",
#'   pdf_name = "Cosby1984"
#' )
#'
#' # Use PDF "Rosetta3" to estimate parameters of SWRC "vanGenuchten1980"
#' if (check_pdf_availability("Rosetta3")) {
#'   pdf_estimate(
#'     sand = soils[, "sand_frac"],
#'     clay = soils[, "clay_frac"],
#'     fcoarse = soils[, "gravel_content"],
#'     bdensity = soils[, "bulkDensity_g/cm^3"],
#'     swrc_name = "vanGenuchten1980",
#'     pdf_name = "Rosetta3"
#'   )
#' }
#'
#' # Use PDF "neuroFX2021" to estimate parameters of SWRC `FXW`
#' \dontrun{
#' # Set neuroFX2021 file path, see details in `pdf_neuroFX2021_for_FXW()`
#' options(RSW2_FILENEUROFX2021 = "path/to/sscbd.RData")
#' }
#'
#' if (check_pdf_availability("neuroFX2021")) {
#'   pdf_estimate(
#'     sand = soils[, "sand_frac"],
#'     clay = soils[, "clay_frac"],
#'     fcoarse = soils[, "gravel_content"],
#'     bdensity = soils[, "bulkDensity_g/cm^3"],
#'     swrc_name = "FXW",
#'     pdf_name = "neuroFX2021"
#'   )
#' }
#'
#' @md
#' @export
pdf_estimate <- function(
  sand,
  clay,
  fcoarse,
  bdensity = NULL,
  swrc_name,
  pdf_name,
  ...
) {

  #--- Check for consistency between SWRC and PDF
  swrc_name <- std_swrc(swrc_name)[1]
  pdf_name <- std_pdf(pdf_name)[1]

  if (!.Call(C_rSW2_check_SWRC_vs_PDF, swrc_name, pdf_name)) {
    stop(
      "Selected PDF ",
      shQuote(pdf_name),
      " is incompatible with selected SWRC ",
      shQuote(swrc_name)
    )
  }


  #--- Determine whether we use a C- or R-implemented PDF
  swrcp <- if (pdf_name %in% pdfs_implemented_in_rSW2()) {
    rSW2_SWRC_PDF_estimate_parameters(
      pdf_name = pdf_name,
      sand = sand,
      clay = clay,
      fcoarse = fcoarse,
      bdensity = bdensity,
      ...
    )

  } else {
    .Call(
      C_rSW2_SWRC_PDF_estimate_parameters,
      pdf_type = rep_len(encode_name2pdf(pdf_name), length(sand)),
      sand = sand,
      clay = clay,
      fcoarse = fcoarse,
      bdensity = bdensity
    )
  }


  #--- Check validity of estimated SWRCp
  if (!all(check_swrcp(swrc_name, swrcp))) {
    warning("Some estimated parameters failed checks.")
  }

  swrcp
}


#' List PDFs implemented only in `rSOILWAT2` instead of `SOILWAT2`
#' @md
pdfs_implemented_in_rSW2 <- function() {
  c(
    # `Rosetta3` estimates parameters of `vanGenuchten1980` SWRC
    "Rosetta3",
    # `neuroFX2021` estimates parameters of `FXW` SWRC
    "neuroFX2021"
  )
}

#' Check availability of `PDFs`
#'
#' PDFs implemented in `SOILWAT2` are always available;
#' PDFs implemented in `rSOILWAT2` may have additional requirements, e.g.,
#' live internet connection or access to specific data files.
#'
#' @param pdfs A character vector. `PDF` names to be checked;
#'   defaults to `pdf_names()`.
#' @param verbose A logical value.
#'
#' @return A named logical vector with current availability of `PDFs`;
#' `pdfs` that are not implemented return `NA`.
#'
#' @examples
#' check_pdf_availability()
#' check_pdf_availability("neuroFX2021")
#' check_pdf_availability("nonexistent_PDF")
#'
#' @export
#' @md
check_pdf_availability <- function(
  pdfs = names(pdf_names()),
  verbose = interactive()
) {
  res <- rep(NA, length(pdfs))
  names(res) <- pdfs

  rpdfs <- pdfs_implemented_in_rSW2()

  # PDFs implemented in SOILWAT2 are always available
  tmp <- pdfs %in% names(pdf_names()) & !(pdfs %in% rpdfs)
  res[tmp] <- TRUE

  # Check requested PDFs implemented in R
  has_rpdfs <- vapply(
    pdfs[pdfs %in% rpdfs],
    function(pdf) {
      switch(
        EXPR = pdf,
        Rosetta3 = pdf_Rosetta3_availability(verbose = verbose),
        neuroFX2021 = pdf_neuroFX2021_availability(verbose = verbose),
        NA
      )
    },
    FUN.VALUE = NA,
    USE.NAMES = TRUE
  )

  res[names(has_rpdfs)] <- has_rpdfs

  res
}


#' Estimate parameters of selected soil water retention curve (`SWRC`)
#' using selected pedotransfer function (`PDF`) that are implemented in `R`
#'
#' @inheritParams pdf_estimate
#' @param fail A logical value. If `TRUE` (default) and
#'   requested `PDF` is not implemented in `R`, then throw an error;
#'   otherwise, return silently.
#'
#' @return `swrcp`, i.e,.
#' a numeric matrix where rows represent soil (layers) and
#' columns represent a fixed number of `SWRC` parameters.
#' The interpretation is dependent on the selected `SWRC`.
#' However, return value is `NULL`
#' only if `fail` is `FALSE` and requested `PDF` is not implemented in `R`.
#'
#' @inherit SWRCs references
#'
#' @section Details:
#' [pdf_estimate()] is the function that should be directly called; this here
#' is an internal helper function.
#'
#' @section Notes:
#' See `SWRC_PDF_estimate_parameters()` in `SOILWAT2` for `PDFs`
#' implemented in C.
#'
#' @md
rSW2_SWRC_PDF_estimate_parameters <- function( # nolint: object_length_linter.
  pdf_name,
  sand,
  clay,
  fcoarse,
  bdensity = NULL,
  fail = TRUE,
  ...
) {
  pdf_name <- std_pdf(pdf_name)[1]
  has_pdf <- pdf_name %in% pdfs_implemented_in_rSW2()

  list_soilargs <- list(
    sand = sand,
    clay = clay,
    bdensity = bdensity
  )

  if (has_pdf && pdf_name %in% "Rosetta3") {
    dots <- list(...)
    dots[["version"]] <- if ("version" %in% names(dots)) {
      as.character(dots[["version"]])
    } else {
      "3"
    }

    do.call(
      pdf_Rosetta_for_vanGenuchten1980,
      args = c(list_soilargs, dots)
    )

  } else if (has_pdf && pdf_name %in% "neuroFX2021") {
    do.call(
      pdf_neuroFX2021_for_FXW,
      args = c(list_soilargs, list(...))
    )[["mean"]]

  } else {
    if (isTRUE(fail)) {
      stop("PDF ", shQuote(pdf_name), " is not implemented in rSOILWAT2.")
    }
  }
}


#' Estimate van Genuchten 1980 `SWRC` parameters using `Rosetta` live `API`
#'
#' @inheritParams SWRCs
#' @param version A character string that selects a `Rosetta` version.
#'
#' @return `swrcp`, i.e,.
#' a numeric matrix where rows represent soil (layers) and
#' columns represent a fixed number of `SWRC` parameters: \itemize{
#'   \item `swrcp[0]` (`theta_r`): residual volumetric water content
#'         of the matric component (units of `[cm / cm]`)
#'   \item `swrcp[1]` (`theta_s`): saturated volumetric water content
#'         of the matric component (units of `[cm / cm]`)
#'   \item `swrcp[2]` (`alpha`): related to the inverse of
#'         air entry suction (units of `[cm-1]`)
#'   \item `swrcp[3]` (`n`): measure of the pore-size distribution `[-]`
#'   \item `swrcp[4]` (`K_sat`): saturated hydraulic conductivity `[cm / day]`
#' }
#'
#' @references
#'   Mualem, Y. 1976. A new model for predicting the hydraulic conductivity of
#'   unsaturated porous media.
#'   Water Resources Research, 12:513-522, \doi{10.1029/WR012i003p00513}
#' @references
#'   van Genuchten, M. T. 1980. A Closed-form Equation for Predicting the
#'   Hydraulic Conductivity of Unsaturated Soils.
#'   Soil Science Society of America Journal, 44:892-898,
#'   \doi{10.2136/sssaj1980.03615995004400050002x}
#' @references
#'   Zhang, Y., & Schaap, M. G. 2017. Weighted recalibration of the
#'   Rosetta pedotransfer model with improved estimates of
#'   hydraulic parameter distributions and summary statistics (Rosetta3).
#'   Journal of Hydrology, 547:39-53, \doi{10.1016/j.jhydrol.2017.01.004}
#'
#' @section Details:
#' [pdf_estimate()] is the function that should be directly called; this here
#' is an internal helper function.
#'
#' @section Notes:
#' This function calls `soilDB::ROSETTA()` and
#' a live internet connection is required to access `Rosetta`.
#'
#' @seealso `soilDB::ROSETTA()`
#'
#' @md
pdf_Rosetta_for_vanGenuchten1980 <- function( # nolint: object_length_linter.
  sand,
  clay,
  bdensity = NULL,
  version = c("3", "1", "2"),
  verbose = interactive(),
  ...
) {
  stopifnot(pdf_Rosetta3_availability(verbose = verbose))

  version <- match.arg(version)

  if (verbose) {
    message("Connecting live to ROSETTA API...")
  }

  tmp_txt <- 100 * data.frame(
    sand = sand,
    silt = 1 - (sand + clay),
    clay = clay
  )
  var_txt <- c("sand", "silt", "clay")

  tmp <- if (is.null(bdensity)) {
    soilDB::ROSETTA(tmp_txt, vars = var_txt, v = version)
  } else {
    soilDB::ROSETTA(
      cbind(tmp_txt, bdensity = bdensity),
      vars = c(var_txt, "bdensity"),
      v = version
    )
  }

  unname(data.matrix(data.frame(
    tmp[, c("theta_r", "theta_s")],
    10 ^ tmp[, "alpha"],
    10 ^ tmp[, "npar"],
    10 ^ tmp[, "ksat"],
    0
  )))
}

# Checks availability of `Rosetta3` `PDF`
#
# Note: `check_pdf_availability()` requires function name
# to match pattern "pdf_XXX_availability" where XXX = name of PDF
pdf_Rosetta3_availability <- function(verbose = interactive(), ...) {
  tmp <- c(
    requireNamespace("soilDB"),
    requireNamespace("curl") && curl::has_internet()
  )

  res <- all(tmp)

  if (!res && verbose) {
    if (!tmp[1]) {
      message(
        "`pdf_Rosetta3_availability()`: ",
        "R package 'soilDB' is not available."
      )
    }
    if (!tmp[2]) {
      message(
        "`pdf_Rosetta3_availability()`: ",
        "R package 'curl' is not available or there is no live internet."
      )
    }
  }

  res
}


#' Estimate `FXW` `SWRC` parameters using `neuroFX2021`
#'
#' @inheritParams SWRCs
#' @param file_neuroFX2021 A character string that contains the file name with
#'   full path of the `neuroFX2021` R object provided by Rudiyanto et al. 2021;
#'   The path to the appropriate file can be set per R session
#'   via option `"RSW2_FILENEUROFX2021"`, see additional details.
#'
#' @return `swrcp`, i.e,.
#' a numeric matrix where rows represent soil (layers) and
#' columns represent a fixed number of `SWRC` parameters: \itemize{
#'   \item `swrcp[0]` (`theta_s`): saturated volumetric water content
#'         of the matric component (units of `[cm / cm]`)
#'   \item `swrcp[1]` (`alpha`): shape parameter (units of `[cm-1]`)
#'   \item `swrcp[2]` (`n`): shape parameter `[-]`
#'   \item `swrcp[3]` (`m`): shape parameter `[-]`
#'   \item `swrcp[4]` (`K_sat`): saturated hydraulic conductivity `[cm / day]`
#'   \item `swrcp[5]` (`L`): tortuosity/connectivity parameter `[-]`
#' }
#'
#' @references
#'   Rudiyanto, Minasny, B., Chaney, N. W., Maggi, F., Goh Eng Giap, S.,
#'   Shah, R. M., Fiantis, D., & Setiawan, B. I. 2021.
#'   Pedotransfer functions for estimating soil hydraulic properties from
#'   saturation to dryness.
#'   Geoderma, 403:115194, \doi{10.1016/j.geoderma.2021.115194}
#' @references
#'   Fredlund, D. G., & Xing, A. 1994.
#'   Equations for the soil-water characteristic curve.
#'   Canadian Geotechnical Journal, 31: 512–532, \doi{10.1139/t94-061}
#' @references
#'   Wang, Y., Jin, M., & Deng, Z. 2018.
#'   Alternative model for predicting soil hydraulic conductivity over
#'   the complete moisture range.
#'   Water Resources Research, 54:6860–6876, \doi{10.1029/2018WR023037}
#'
#' @section Details:
#' [pdf_estimate()] is the function that should be directly called; this here
#' is an internal helper function.
#'
#' @section Details:
#' This function requires that users download
#' the fitted `neuroFX2021` neural networks published by Rudiyanto et al. 2021
#' in Supplementary Material 1 (resulting in a local file named `xxx_mmc1.zip`).
#' This needs to be unzipped and the resulting `tar` file unpacked;
#' this produces a folder `R code for neuroFX2021`.
#' This folder contains two R data files : `ssc.RData` and `sscbd.RData`.
#' The argument `file_neuroFX2021` is the file name (with path) to `sscbd.RData`
#' if soil density data are available and to `ssc.RData` otherwise
#' (see Rudiyanto et al. 2021).
#' The path to the appropriate file can be set per R session
#' via option `"RSW2_FILENEUROFX2021"`
#' (and avoid passing it directly as argument to the function);
#' this can be useful, for example, if `pdf_estimate()` is used for `FXW`.
#'
#' @md
pdf_neuroFX2021_for_FXW <- function(
  sand,
  clay,
  bdensity = NULL,
  file_neuroFX2021 = getOption("RSW2_FILENEUROFX2021", NULL),
  ...
) {
  stopifnot(pdf_neuroFX2021_availability(file_neuroFX2021))

  # Load `neuroFX2021`
  nfx <- new.env()
  load(file_neuroFX2021, envir = nfx)

  # Check whether type of neuroFX2021 is
  #    SSC (sand, silt, clay) or
  #    SSCBD (sand, silt, clay, bulk density)
  is_sscbd <- dim(nfx[["tW1"]])[2] == 4

  # Prepare soil data
  tmp_txt <- data.frame(
    sand = sand,
    silt = 1 - (sand + clay),
    clay = clay
  )

  if (!is.null(bdensity)) {
    if (is_sscbd) {
      tmp_txt[, "bd"] <- bdensity
    } else {
      warning(
        "`pdf_neuroFX2021_for_FXW()`: ",
        "`bdensity` ignored because ",
        "'neuroFX2021' object is for SSC (sand, silt, clay)."
      )
    }
  } else {
    if (is_sscbd) {
      stop(
        "`pdf_neuroFX2021_for_FXW()`: ",
        "'neuroFX2021' object is for SSCBD (sand, silt, clay, bulk density) ",
        "but `bdensity` contains no values."
      )
    }
  }

  # Evaluate neuroFX2021
  res <- iterate_neuroFX(tmp_txt, nfx, niter = dim(nfx[["tW1"]])[1])

  # Aggregate across iterations
  tmp_res <- lapply(c("mean", "sd"), function(f) apply(res, 2:3, f))

  # Backtransformation
  for (k in seq_along(tmp_res)) {
    # backtransform log(alpha) -> alpha
    tmp_res[[k]][, 2] <- exp(tmp_res[[k]][, 2])
    # backtransform log(n - 1) -> n
    tmp_res[[k]][, 3] <- 1 + exp(tmp_res[[k]][, 3])
    # backtransform log10(Ks) -> Ks
    tmp_res[[k]][, 5] <- 10 ^ tmp_res[[k]][, 5]
  }

  list(
    mean = tmp_res[[1]],
    sd = tmp_res[[2]]
  )
}

# Checks availability of `neuroFX2021` `PDF`
#
# Note: `check_pdf_availability()` requires function name
# to match pattern "pdf_XXX_availability" where XXX = name of PDF
pdf_neuroFX2021_availability <- function(
  file_neuroFX2021 = getOption("RSW2_FILENEUROFX2021", NULL),
  verbose = interactive(),
  ...
) {
  res <- !is.null(file_neuroFX2021) && file.exists(file_neuroFX2021)

  if (!res && verbose) {
    message(
      "`pdf_neuroFX2021_availability()`: ",
      "data file 'file_neuroFX2021' does not exist; ",
      "see documentation for `pdf_neuroFX2021_for_FXW()` and consider setting ",
      "`options(RSW2_FILENEUROFX2021 = \"path/to/sscbd.RData\")`"
    )
  }

  res
}


# Evaluate neural net: code based on Rudiyanto et al. 2021
eval_nnet <- function(X, W1, W2) {
  N <- nrow(X)
  # from input layer to hidden layer
  xt <- rbind(t(X), matrix(1, nrow = 1, ncol = N))
  h <- W1 %*% xt
  # activation function
  y1 <- tanh(h)
  # from hidden layer to output layer
  t(W2 %*% rbind(y1, matrix(1, nrow = 1, ncol = N)))
}

# Iterate over neuroFX2021: code based on Rudiyanto et al. 2021
iterate_neuroFX <- function(x, nfx, niter = 50) {
  res <- array(dim = c(niter, nrow(x), 6))

  # loop though n iteration
  for (k in seq_len(niter)) {
    # Predict theta_sat, log(alpha), log(n-1), m
    res[k, , 1:4] <- eval_nnet(
      x,
      W1 = nfx[["tW1"]][k, , ],
      W2 = nfx[["tW2"]][k, , ]
    )
    # Predict log10(K_sat), L
    res[k, , 5:6] <- eval_nnet(
      x,
      W1 = nfx[["kW1"]][k, , ],
      W2 = nfx[["kW2"]][k, , ]
    )
  }

  res
}


#' Check Soil Water Retention Curve parameters
#'
#' @inheritParams SWRCs
#'
#' @section Notes:
#' The argument selecting `SWRC` (`swrc_name`) is recycled
#' for multiple parameter sets, i.e., rows of `swrcp`.
#'
#' @section Details:
#' [swrc_names()] lists implemented `SWRCs`.
#'
#' @seealso [pdf_estimate()]
#'
#' @examples
#' swrc_name <- "Campbell1974"
#' pdf_name <- "Cosby1984AndOthers"
#' swrcp <- pdf_estimate(
#'   sand = c(0.5, 0.3),
#'   clay = c(0.2, 0.1),
#'   fcoarse = c(0, 0),
#'   swrc_name = swrc_name,
#'   pdf_name = pdf_name
#' )
#'
#' check_swrcp(swrc_name, swrcp)
#' check_swrcp(swrc_name, swrcp[1, ])
#'
#' swrcp2 <- swrcp
#' swrcp2[1, 1] <- -10
#' check_swrcp(swrc_name, swrcp2)
#'
#' @export
#' @md
check_swrcp <- function(swrc_name, swrcp) {
  # lengths of arguments are checked by `C_rSW2_SWRC_check_parameters()`
  .Call(
    C_rSW2_SWRC_check_parameters,
    swrc_type = rep_len(
      encode_name2swrc(swrc_name)[1],
      if (is.matrix(swrcp)) nrow(swrcp) else 1
    ),
    swrcp = swrcp
  )
}



#------ Soil Water Retention Curves ------

#' Conversion between bulk soil water content and soil water potential
#'
#' @inheritParams SWRCs
#' @param direction A character string. Indicates the direction of
#'   soil water conversion.
#' @param x A numeric value, vector, or matrix.
#'   The soil water values to be converted,
#'   either soil water potential (units `[MPa]`) of the soil matric component or
#'   bulk volumetric water content (units `[cm/cm]`).
#' @param outer_if_equalsize A logical value.
#'   Relevant only if `x` of length `l` and soils of length `d` are equal.
#'   If `TRUE`, then the returned object has a size of `l x d` = `l x l`
#'   where the `d` sets of soil values are repeated for each value of `x`.
#'   If `FALSE` (default), then the returned object has a size of `l` = `d`
#'   where the the `SWRC` conversion is applied to the
#'   first element of `x` and soils, the second elements, and so on.
#'
#' @return The dimensions of the output are a function of `x` and the
#'   number of soil values (e.g., rows or length of `swrc[["swrcp"]]`).
#'   The returned object has:
#'   \itemize{
#'     \item length `l` if both `x` and soils are of length `l`.
#'     \item length `l` if `x` has length `l` and there is one soil.
#'     \item length `d` if `x` is one value and soils are of length `d`.
#'     \item size `l x d` if `x` has length `l` and soils are of length `d`
#'           (if `l` and `d` are not equal or `outer_if_equalsize` is `TRUE`;
#'           cf. the first case);
#'           the `d` sets of soil values are repeated for each value of `x`.
#'     \item size `l x d` if `x` has size `l x d` and there is one soil.
#'           the soil is repeated for each value of `x`.
#'     \item size `l x d` if `x` has size `l x d` and soils are of length `d`
#'           the `d` sets of soil values are repeated for each row of `x`.
#'   }
#'
#'
#' @inherit SWRCs references
#'
#' @section Details:
#' [swrc_names()] lists implemented `SWRCs`;
#' [pdf_names()] lists implemented `PDFs`; and
#' [check_pdf_availability()] checks availability of `PDFs`.
#'
#' @section Details:
#' For backward compatibility, `fcoarse` and `layer_width` may be missing.
#' If they are missing, then the soils are assumed to contain
#' `0%` coarse fragments and be represented by `1 cm` wide soil layers.
#'
#' @section Details:
#' Arguments `sand`, `clay`, and `bdensity` are only required
#' if `SWRC` parameter values need to be estimated on the fly,
#' i.e., if `swrc` does not contain the element `swrcp`
#' (with suitable `SWRC` parameter values).
#' This is handled by [pdf_estimate()] and additionally requires
#' the element `pdf_name` for argument `swrc`.
#'
#' @section Details:
#' If `swrc` contains element `swrcp` with one set of `SWRC` parameters,
#' i.e., one row, then the parameter set is repeated for each value of `x`.
#'
#' @section Details:
#' If `vwc` inputs represent the matric component
#' (instead of expected bulk values), then set `fcoarse` to 0.
#' This works, however, only if `swrcp` are provided or `fcoarse` is not
#' utilized by the requested `pdf`.
#'
#'
#' @seealso
#'   [pdf_estimate()],
#'   [check_swrcp()],
#'   [check_pdf_availability()]
#'
#' @examples
#' fsand <- c(0.5, 0.3)
#' fclay <- c(0.2, 0.1)
#' fcrs1 <- c(0, 0)
#' fcrs2 <- c(0.4, 0.1)
#'
#' swrc1 <- list(
#'   name = "Campbell1974",
#'   swrcp = pdf_estimate(
#'     sand = fsand,
#'     clay = fclay,
#'     fcoarse = fcrs1,
#'     swrc_name = "Campbell1974",
#'     pdf_name = "Cosby1984"
#'   )
#' )
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, swrc = swrc1)
#' swrc_swp_to_vwc(c(-1.5, NA), fcoarse = fcrs1, swrc = swrc1)
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, sand = fsand, clay = fclay)
#' swrc_vwc_to_swp(c(0.10, 0.15, 0.20), fcoarse = fcrs1, swrc = swrc1)
#' swrc_vwc_to_swp(c(0.10, NA, 0.20), fcoarse = fcrs1, swrc = swrc1)
#'
#' swrc2 <- list(
#'   name = "Campbell1974",
#'   swrcp = pdf_estimate(
#'     sand = fsand,
#'     clay = fclay,
#'     fcoarse = fcrs2,
#'     swrc_name = "Campbell1974",
#'     pdf_name = "Cosby1984"
#'   )
#' )
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, swrc = swrc2)
#' (1 - fcrs2) * swrc_swp_to_vwc(-1.5, swrc = swrc2)
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, sand = fsand, clay = fclay)
#' swrc_vwc_to_swp(c(0.10, 0.15, 0.20), fcoarse = fcrs2, swrc = swrc2)
#'
#'
#' # Available water holding capacity "AWC"
#' soils <- swSoils_Layers(rSOILWAT2::sw_exampleData)
#' p <- pdf_estimate(
#'   sand = soils[, "sand_frac"],
#'   clay = soils[, "clay_frac"],
#'   fcoarse = soils[, "gravel_content"]
#' )
#' tmp <- swrc_swp_to_vwc(
#'   c(-1.5, -0.033),
#'   fcoarse = soils[, "gravel_content"],
#'   swrc = list(name = "Campbell1974", swrcp = p)
#' )
#' awc <- diff(c(0, soils[, "depth_cm"])) * as.vector(diff(tmp))
#'
#'
#' # Shape of SWRCs
#' theta <- seq(0.05, 0.55, by = 0.001)
#' soils <- data.frame(
#'   sand_frac = c(sand = 0.92, silty_loam = 0.17, silty_clay = 0.06),
#'   clay_frac = c(0.03, 0.13, 0.58),
#'   bd = c(1.614, 1.464, 1.437)
#' )
#' phi <- list(
#'   Campbell1974 = swrc_vwc_to_swp(
#'     theta,
#'     sand = soils[, "sand_frac"],
#'     clay = soils[, "clay_frac"],
#'     swrc = list(swrc_name = "Campbell1974", pdf_name = "Cosby1984")
#'   )
#' )
#'
#' if (check_pdf_availability("Rosetta3")) {
#'   phi[["vanGenuchten1980"]] <- swrc_vwc_to_swp(
#'     theta,
#'     sand = soils[, "sand_frac"],
#'     clay = soils[, "clay_frac"],
#'     bdensity = soils[, "bd"],
#'     swrc = list(swrc_name = "vanGenuchten1980", pdf_name = "Rosetta3")
#'   )
#' }
#'
#' # Use PDF "neuroFX2021" to estimate parameters of SWRC `FXW`
#' \dontrun{
#' # Set neuroFX2021 file path, see details in `pdf_neuroFX2021_for_FXW()`
#' options(RSW2_FILENEUROFX2021 = "path/to/sscbd.RData")
#' }
#'
#' if (check_pdf_availability("neuroFX2021")) {
#'   phi[["FXW"]] <- swrc_vwc_to_swp(
#'     theta,
#'     sand = soils[, "sand_frac"],
#'     clay = soils[, "clay_frac"],
#'     bdensity = soils[, "bd"],
#'     swrc = list(swrc_name = "FXW", pdf_name = "neuroFX2021")
#'   )
#' }
#'
#' if (requireNamespace("graphics")) {
#'   par_prev <- graphics::par(mfcol = c(length(phi), 1))
#'
#'   for (k in seq_along(phi)) {
#'     graphics::matplot(
#'       theta, -phi[[k]],
#'       type = "l",
#'       log = "y",
#'       xlim = c(0, max(theta)),
#'       xlab = "theta [m/m]",
#'       ylim = c(1e-4, 1e6),
#'       ylab = "-phi [MPa]",
#'       main = paste0("Soil Water Retention Curve (", names(phi)[k], ")")
#'     )
#'     graphics::abline(h = -c(-1.5, -0.033), col = "gray", lty = 3)
#'     graphics::legend("topright", rownames(soils), col = 1:3, lty = 1:3)
#'   }
#'
#'   graphics::par(par_prev)
#' }
#'
#'
#' @export
#' @md
swrc_conversion <- function(
  direction = c("swp_to_vwc", "vwc_to_swp"),
  x,
  fcoarse,
  layer_width,
  swrc,
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
) {
  #--- Check inputs
  direction <- match.arg(direction)

  # `name` can be used as short form of `swrc_name`
  if (!("swrc_name" %in% names(swrc)) && "name" %in% names(swrc)) {
    swrc[["swrc_name"]] <- swrc[["name"]]
  }

  stopifnot("swrc_name" %in% names(swrc))
  swrc[["swrc_name"]] <- std_swrc(swrc[["swrc_name"]])[1]
  swrc[["swrc_type"]] <- encode_name2swrc(swrc[["swrc_name"]])


  # Do we need to estimate swrcp?
  swrc[["swrcp"]] <- if (
    "swrcp" %in% names(swrc) && !is.null(swrc[["swrcp"]])
  ) {
    if (is.null(dim(swrc[["swrcp"]]))) {
      matrix(swrc[["swrcp"]], nrow = 1)
    } else {
      as.matrix(swrc[["swrcp"]])
    }
  }

  # Do we have sufficient information to estimate swrcp?
  if (is.null(swrc[["swrcp"]])) {
    if (
      !all(c("swrc_name", "pdf_name") %in% names(swrc)) ||
        is.null(sand) || is.null(clay)
    ) {
      stop("Insufficient information to estimate SWRC parameters.")
    } else {
      swrc[["pdf_type"]] <- encode_name2swrc(swrc[["pdf_name"]])[1]
    }
  }

  # If we have one set of `swrcp` but many `x` -> repeat `swrcp` for each `x`
  if (
    !is.null(swrc[["swrcp"]]) &&
    nrow(swrc[["swrcp"]]) == 1 &&
    length(x) > 0
  ) {
    swrc[["swrcp"]] <- swrc[["swrcp"]][rep(1, length(x)), , drop = FALSE]
  }

  # Do we have sufficient soil parameters?
  if (missing(fcoarse) && missing(layer_width)) {
    ntmp <- if (!is.null(swrc[["swrcp"]])) {
      nrow(swrc[["swrcp"]])
    } else {
      if (!is.null(sand)) {
        length(sand)
      } else if (!is.null(clay)) {
        length(clay)
      }
    }

    if (!is.null(ntmp)) {
      fcoarse <- rep(0, ntmp)
      layer_width <- rep(1, ntmp)
    } else {
      stop("Insufficient soil parameters to use SWRC.")
    }

  } else if (missing(fcoarse)) {
    fcoarse <- rep(0, length(layer_width))
  } else if (missing(layer_width)) {
    layer_width <- rep(1, length(fcoarse))
  }

  # Put together available soil parameters and check for consistency
  soils <- list(
    fcoarse = fcoarse,
    layer_width = layer_width
  )

  if (is.null(swrc[["swrcp"]])) {
    soils <- c(
      soils,
      list(sand = sand, clay = clay),
      if (!is.null(bdensity)) list(bdensity = bdensity)
    )
  }

  nsoils <- unique(lengths(soils))

  if (length(nsoils) > 1) {
    stop("Soil variables have different lengths.")
  }

  if (!is.null(swrc[["swrcp"]]) && nrow(swrc[["swrcp"]]) != nsoils) {
    stop("Dimensions of `swrcp` and lenght of soil variables disagree.")
  }


  #--- Determine dimensions of data and result
  nrx <- NROW(x)
  ncx <- NCOL(x)
  nx <- nrx * ncx
  nx1d <- nrx == 1 || ncx == 1

  res <- array(dim = c(nrx, ncx))


  #--- Prepare inputs and make SWRC conversion
  if (
    nx1d && (nx == 1 || nsoils == 1 || (nx == nsoils && !outer_if_equalsize))
  ) {

    # 1a. x [len = 1] + soils [len = 1] --> res [len = 1, dim = 1 x 1]
    # nothing to prepare

    if (nx == 1 && nsoils > 1) {
      # 2. x [len = 1] + soils [len = d] --> res [len = d, dim = 1 x d]
      x <- rep_len(x, nsoils)

    } else if (nx > 1 && nx1d && nsoils == 1) {
      # 3. x [len = l] + soils [len = 1] --> res [len = l, dim = l x 1]
      soils <- lapply(soils, rep_len, length.out = nx)

    } else if (nx == nsoils && !outer_if_equalsize) {
      # 1b. x [len = l] + soils [len = l] --> res [len = l, dim = l x 1]
      x <- as.vector(unlist(x))
    }

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        bdensity = soils[["bdensity"]],
        swrc_name = swrc[["swrc_name"]],
        pdf_name = swrc[["pdf_name"]]
      )
    }

    res <- swrc_conversion_1d(direction, x, soils, swrc, verbose)

  } else if (nx1d && nx > 1 && nsoils > 1) {
    # 4. x [len = l] + soils [len = d] -> res [dim = l x d]
    # (x repeated for each soil)

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        bdensity = soils[["bdensity"]],
        swrc_name = swrc[["swrc_name"]],
        pdf_name = swrc[["pdf_name"]]
      )
    }

    tmp <- lapply(
      x,
      function(v) {
        swrc_conversion_1d(direction, rep_len(v, nsoils), soils, swrc, verbose)
      }
    )
    res <- matrix(unlist(tmp), nrow = nx, ncol = nsoils, byrow = TRUE)

  } else if (nx > 1 && !nx1d && nsoils == 1) {
    # 5. x [dim = l x d] + soils [len = 1] --> res [dim = l x d]
    soils <- lapply(soils, rep_len, length.out = nrx)

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        bdensity = soils[["bdensity"]],
        swrc_name = swrc[["swrc_name"]],
        pdf_name = swrc[["pdf_name"]]
      )
    }

    res <- vapply(
      seq_len(ncx),
      function(k) swrc_conversion_1d(direction, x[, k], soils, swrc, verbose),
      FUN.VALUE = rep(1, nrx),
      USE.NAMES = FALSE
    )


  } else if (nx > 1 && !nx1d && nsoils == ncx) {
    # 6. x [dim = l x d] + soils [len = d] --> res [dim = l x d]
    # (soils repeated for row of x value)

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        bdensity = soils[["bdensity"]],
        swrc_name = swrc[["swrc_name"]],
        pdf_name = swrc[["pdf_name"]]
      )
    }

    swrc[["swrc_type"]] <- rep_len(swrc[["swrc_type"]], nsoils)

    res <- vapply(
      seq_len(ncx),
      function(k) {
        ids <- rep.int(k, nrx)
        swrc_conversion_1d(
          direction,
          x = x[, k],
          soils = lapply(soils, function(sp) sp[ids]),
          swrc = list(
            swrc_type = swrc[["swrc_type"]][ids],
            swrcp = swrc[["swrcp"]][ids, , drop = FALSE]
          ),
          verbose = verbose
        )
      },
      FUN.VALUE = rep(1, nrx),
      USE.NAMES = FALSE
    )

  } else {
    stop("Unsuitable inputs.")
  }

  res
}


#' Helper function of \code{swrc_conversion} to access underlying C code
#' @noRd
swrc_conversion_1d <- function(direction, x, soils, swrc, verbose) {

  prev_verbosity <- sw_verbosity(verbose = as.logical(verbose))
  on.exit(sw_verbosity(prev_verbosity))

  # lengths of arguments are checked by `C_rSW2_SWRC()`
  nx <- length(x)

  switch(
    EXPR = direction,
    # C_rSW2_SWRC(direction = 1) returns [cm] convert to [cm/cm]
    swp_to_vwc = 1 / soils[["layer_width"]] * .Call(
      C_rSW2_SWRC,
      # x = SWP [MPa] convert to [-bar]
      x = - 10 * x,
      direction = 1L,
      swrc_type = rep_len(swrc[["swrc_type"]], nx),
      swrcp = swrc[["swrcp"]],
      fcoarse = soils[["fcoarse"]],
      width = soils[["layer_width"]]
    ),
    # C_rSW2_SWRC(direction = 2) returns [-bar] convert to [MPa]
    vwc_to_swp = - 0.1 * .Call(
      C_rSW2_SWRC,
      # x = VWC (bulk) [cm/cm] convert to SWC [cm]
      x = x * soils[["layer_width"]],
      direction = 2L,
      swrc_type = rep_len(swrc[["swrc_type"]], nx),
      swrcp = swrc[["swrcp"]],
      fcoarse = soils[["fcoarse"]],
      width = soils[["layer_width"]]
    )
  )
}



#' @describeIn swrc_conversion Convenience wrapper
#'   to convert from `SWP` to bulk `VWC` with selected `SWRC`
#'
#' @param swp_MPa A numeric object. The soil water potential values
#'   (units `[MPa]`) of the soil matric component to be converted to
#'   bulk volumetric water content
#'   (i.e., relative to the whole soil; units `[cm/cm]`).
#'
#' @export
swrc_swp_to_vwc <- function(
  swp_MPa,
  fcoarse,
  layer_width,
  swrc = list(swrc_name = NULL, pdf_name = NULL, swrcp = NULL),
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
) {
  swrc_conversion(
    direction = "swp_to_vwc",
    x = swp_MPa,
    sand = sand,
    clay = clay,
    fcoarse = fcoarse,
    bdensity = bdensity,
    layer_width = layer_width,
    swrc = swrc,
    outer_if_equalsize = outer_if_equalsize,
    verbose = verbose
  )
}



#' @describeIn swrc_conversion Convenience wrapper
#'   to convert from bulk `VWC` to matric `SWP` with selected `SWRC`
#'
#' @param vwcBulk A numeric object. The volumetric water content values
#'   (relative to the whole soil; units `[cm/cm]`)
#'   to be converted to soil water potential (units `[MPa]`)
#'   of the soil matric component.
#'
#' @export
swrc_vwc_to_swp <- function(
  vwcBulk,
  fcoarse,
  layer_width,
  swrc = list(swrc_name = NULL, pdf_name = NULL, swrcp = NULL),
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
) {
  swrc_conversion(
    direction = "vwc_to_swp",
    x = vwcBulk,
    sand = sand,
    clay = clay,
    fcoarse = fcoarse,
    bdensity = bdensity,
    layer_width = layer_width,
    swrc = swrc,
    outer_if_equalsize = outer_if_equalsize,
    verbose = verbose
  )
}
