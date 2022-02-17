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
  stopifnot(length(sand) && length(sand) == length(clay))
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
#' @param layer_width A numeric value or vector.
#'   Depth interval, width, of each soil layer (units of `cm`).
#'   `layer_width` is required to translate between
#'   soil water content of a soil layer and volumetric water content.
#' @param swrc_type An integer value or vector.
#'   The identification number of selected `SWRC`.
#' @param pdf_type An integer value or vector.
#'   The identification number of selected `PDF`.
#' @param swrcp A numeric vector or matrix.
#'   The parameters of a selected `SWRC`;
#'   each row represents one `SWRC`, e.g., one per soil layer.
#' @param swrc A named list.
#'   Contains all necessary elements of a `SWRC`,
#'   i.e., `type` (short for `swrc_type`) and `swrcp`,
#'   or all necessary elements to estimate parameters of a `SWRC` given
#'   soil parameters, i.e., `swrc_type` and `pdf_type`.
#'
#' @section Details:
#' Implemented SWRCs (`swrc_type`):
#'   1. Campbell 1974
#'
#' @references Campbell, G. S. 1974.
#' A simple method for determining unsaturated conductivity
#' from moisture retention data.
#' Soil Science, 117(6):311-314. \doi{10.1097/00010694-197406000-00001}
#'
#' @seealso
#'   [pdf_estimate()],
#'   [check_swrcp()],
#'   [swrc_swp_to_vwc()],
#'   [swrc_vwc_to_swp()]
#'
#' @name SWRCs
#' @md
NULL


#' Estimate `SWRC` parameters from soil texture with a pedotransfer function
#'
#' @inheritParams SWRCs
#'
#' @section Details:
#' Implemented PDFs (`pdf_type`):
#'   1. Cosby et al. 1984 PDF estimates parameters of Campbell 1974 SWRC
#'
#' @section Details:
#' See [SWRCs] for suitable values of `swrc_type`.
#'
#' @section Notes:
#' The soil parameters `sand`, `clay`, and `fcoarse` must be of
#' the same length, i.e., represent one soil (length 1) or
#' multiple soil (layers) (length > 1).
#' The arguments selecting `SWRC` (`swrc_type`) and `PDF` (`pdf_type`)
#' are recycled for multiple soil layers.
#'
#' @inherit SWRCs references
#'
#' @references Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn.
#' 1984. A statistical exploration of the relationships of soil moisture
#' characteristics to the physical properties of soils.
#' Water Resources Research, 20:682-690, \doi{10.1029/WR020i006p00682}
#'
#' @return `swrcp`, i.e,.
#' a numeric matrix where rows represent soil (layers) and
#' columns represent a fixed number of `SWRC` parameters.
#' The interpretation is dependent on the selected `SWRC`.
#'
#' @examples
#' pdf_estimate(sand = c(0.5, 0.3), clay = c(0.2, 0.1), fcoarse = c(0, 0))
#'
#' soils <- swSoils_Layers(rSOILWAT2::sw_exampleData)
#' pdf_estimate(
#'   sand = soils[, "sand_frac"],
#'   clay = soils[, "clay_frac"],
#'   fcoarse = soils[, "gravel_content"]
#' )
#'
#' @md
#' @export
pdf_estimate <- function(sand, clay, fcoarse, swrc_type = 1L, pdf_type = 1L) {

  # lengths of arguments are checked by `C_rSW2_SWRC_PDF_estimate_parameters()`
  nlyrs <- length(sand)

  .Call(
    C_rSW2_SWRC_PDF_estimate_parameters,
    swrc_type = rep_len(swrc_type, nlyrs),
    pdf_type = rep_len(pdf_type, nlyrs),
    sand = sand,
    clay = clay,
    fcoarse = fcoarse
  )
}


#' Check Soil Water Retention Curve parameters
#'
#' @inheritParams SWRCs
#'
#' @section Notes:
#' The argument selecting `SWRC` (`swrc_type`) is recycled
#' for multiple parameter sets, i.e., rows of `swrcp`.
#'
#' @section Details:
#' See [SWRCs] for suitable values of `swrc_type`.
#'
#' @seealso [pdf_estimate()]
#'
#' @examples
#' swrc_type <- 1
#' swrcp <- pdf_estimate(
#'   sand = c(0.5, 0.3),
#'   clay = c(0.2, 0.1),
#'   fcoarse = c(0, 0),
#'   swrc_type = swrc_type
#' )
#'
#' check_swrcp(swrc_type, swrcp)
#' check_swrcp(rep(swrc_type, 2), swrcp)
#' check_swrcp(swrc_type, swrcp[1, ])
#'
#' swrcp2 <- swrcp
#' swrcp2[1, 1] <- -10
#' check_swrcp(rep(swrc_type, 2), swrcp2)
#'
#' @export
#' @md
check_swrcp <- function(swrc_type = 1L, swrcp) {
  # lengths of arguments are checked by `C_rSW2_SWRC_check_parameters()`
  .Call(
    C_rSW2_SWRC_check_parameters,
    swrc_type = rep_len(swrc_type, if (is.matrix(swrcp)) nrow(swrcp) else 1),
    swrcp = swrcp
  )
}



#------ Soil Water Retention Curves ------

#' Conversion between soil water content and soil water potential
#'
#' @inheritParams SWRCs
#' @param direction A character string. Indicates the direction of
#'   soil water conversion.
#' @param x A numeric value, vector, or matrix.
#'   The soil water values to be converted,
#'   either soil water potential (units `[MPa]`) or
#'   volumetric water content (units `[cm/cm]`).
#'
#' @return The dimensions of the output are a function of `x` and the
#'   number of soil values (e.g., rows or length of `swrc[["swrcp"]]`).
#'   The returned object has:
#'   \itemize{
#'     \item length `l` if both `x` and soils are of length `l`.
#'     \item length `l` if `x` has length `l` and there is one soil.
#'     \item length `d` if `x` is one value and soils are of length `d`.
#'     \item size `l x d` if `x` has length `l` and soils are of length `d`
#'           (if `l` and `d` are not equal);
#'           the `d` sets of soil values are repeated for each value of `x`.
#'     \item size `l x d` if `x` has size `l x d` and there is one soil.
#'           the soil is repeated for each value of `x`.
#'     \item size `l x d` if `x` has size `l x d` and soils are of length `d`
#'           the `d` sets of soil values are repeated for each row of `x`.
#'   }
#'
#'
#' @section Details:
#' See [SWRCs] for suitable values of `swrc_type`.
#'
#' @section Details:
#' For backward compatibility, `fcoarse` and `layer_width` may be missing.
#' If they are missing, then the soils are assumed to contain
#' `0%` coarse fragments and be represented by `1 cm` wide soil layers.
#'
#' @section Details:
#' Arguments `sand` and `clay` are only required if `SWRC` parameter values
#' need to be estimated on the fly, i.e., if `swrc` does not contain
#' the element `swrcp` (with suitable `SWRC` parameter values).
#' This is handled by [pdf_estimate()] and additionally requires
#' the element `pdf_type` for argument `swrc`.
#'
#' @seealso
#'   [pdf_estimate()],
#'   [check_swrcp()]
#'
#' @examples
#' fsand <- c(0.5, 0.3)
#' fclay <- c(0.2, 0.1)
#' fcrs1 <- c(0, 0)
#' fcrs2 <- c(0.4, 0.1)
#'
#' p1 <- pdf_estimate(sand = fsand, clay = fclay, fcoarse = fcrs1)
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, swrc = list(type = 1, swrcp = p1))
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, sand = fsand, clay = fclay)
#' swrc_vwc_to_swp(
#'   c(0.10, 0.15, 0.20),
#'   fcoarse = fcrs1,
#'   swrc = list(type = 1, swrcp = p1)
#' )
#'
#' p2 <- pdf_estimate(sand = fsand, clay = fclay, fcoarse = fcrs2)
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, swrc = list(type = 1, swrcp = p2))
#' (1 - fcrs2) * swrc_swp_to_vwc(-1.5, swrc = list(type = 1, swrcp = p2))
#' swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, sand = fsand, clay = fclay)
#' swrc_vwc_to_swp(
#'   c(0.10, 0.15, 0.20),
#'   fcoarse = fcrs2,
#'   swrc = list(type = 1, swrcp = p2)
#' )
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
  clay = NULL
) {
  #--- Check inputs
  direction <- match.arg(direction)

  # `type` can be used as short form of `swrc_type`
  if (!("swrc_type" %in% names(swrc)) && "type" %in% names(swrc)) {
    swrc[["swrc_type"]] <- swrc[["type"]]
  }

  stopifnot("swrc_type" %in% names(swrc))

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
      !all(c("swrc_type", "pdf_type") %in% names(swrc)) ||
        is.null(sand) || is.null(clay)
    ) {
      stop("Insufficient information to estimate SWRC parameters.")
    }
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
    soils <- c(soils, list(sand = sand, clay = clay))
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


  if (nx1d && (nx == 1 || nsoils == 1 || nx == nsoils)) {

    # 1a. x [len = 1] + soils [len = 1] --> res [len = 1, dim = 1 x 1]
    # nothing to prepare

    if (nx == 1 && nsoils > 1) {
      # 2. x [len = 1] + soils [len = d] --> res [len = d, dim = 1 x d]
      x <- rep_len(x, nsoils)

    } else if (nx > 1 && nx1d && nsoils == 1) {
      # 3. x [len = l] + soils [len = 1] --> res [len = l, dim = l x 1]
      soils <- lapply(soils, rep_len, length.out = nx)

    } else if (nx == nsoils) {
      # 1b. x [len = l] + soils [len = l] --> res [len = l, dim = l x 1]
      x <- as.vector(unlist(x))
    }

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        swrc_type = swrc[["swrc_type"]],
        pdf_type = swrc[["pdf_type"]]
      )
    }

    res <- swrc_conversion_1d(direction, x, soils, swrc = swrc)

  } else if (nx1d && nx > 1 && nsoils > 1) {
    # 4. x [len = l] + soils [len = d] -> res [dim = l x d]
    # (x repeated for each soil)

    if (is.null(swrc[["swrcp"]])) {
      swrc[["swrcp"]] <- pdf_estimate(
        sand = soils[["sand"]],
        clay = soils[["clay"]],
        fcoarse = soils[["fcoarse"]],
        swrc_type = swrc[["swrc_type"]],
        pdf_type = swrc[["pdf_type"]]
      )
    }

    tmp <- lapply(
      x,
      function(v) {
        swrc_conversion_1d(direction, x = rep_len(v, nsoils), soils, swrc)
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
        swrc_type = swrc[["swrc_type"]],
        pdf_type = swrc[["pdf_type"]]
      )
    }

    res <- vapply(
      seq_len(ncx),
      function(k) swrc_conversion_1d(direction, x = x[, k], soils, swrc),
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
        swrc_type = swrc[["swrc_type"]],
        pdf_type = swrc[["pdf_type"]]
      )
    }

    swrc[["swrc_type"]] <- rep_len(swrc[["swrc_type"]], nsoils)

    res <- vapply(
      seq_len(ncx),
      function(k) {
        swrc_conversion_1d(
          direction,
          x = x[, k],
          soils = lapply(soils, function(sp) rep_len(sp[k], length.out = nrx)),
          swrc = list(
            swrc_type = rep_len(swrc[["swrc_type"]][k], nrx),
            swrcp = swrc[["swrcp"]][rep(k, nrx), ]
          )
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
swrc_conversion_1d <- function(direction, x, soils, swrc) {

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
#'   to convert from `SWP` to `VWC` with selected `SWRC`
#'
#' @param swp_MPa A numeric object. The soil water potential values
#'   (units `[MPa]`) to be converted to
#'   volumetric water content (relative to the whole soil; units `[cm/cm]`).
#'
#' @export
swrc_swp_to_vwc <- function(
  swp_MPa,
  fcoarse,
  layer_width,
  swrc = list(swrc_type = 1L, pdf_type = 1L, swrcp = NULL),
  sand = NULL,
  clay = NULL
) {
  swrc_conversion(
    direction = "swp_to_vwc",
    x = swp_MPa,
    sand = sand,
    clay = clay,
    fcoarse = fcoarse,
    layer_width = layer_width,
    swrc = swrc
  )
}



#' @describeIn swrc_conversion Convenience wrapper
#'   to convert from `VWC` to `SWP` with selected `SWRC`
#'
#' @param vwcBulk A numeric object. The volumetric water content values
#'   (relative to the whole soil; units `[cm/cm]`)
#'   to be converted to soil water potential values (units `[MPa]`).
#'
#' @export
swrc_vwc_to_swp <- function(
  vwcBulk,
  fcoarse,
  layer_width,
  swrc = list(swrc_type = 1L, pdf_type = 1L, swrcp = NULL),
  sand = NULL,
  clay = NULL
) {
  swrc_conversion(
    direction = "vwc_to_swp",
    x = vwcBulk,
    sand = sand,
    clay = clay,
    fcoarse = fcoarse,
    layer_width = layer_width,
    swrc = swrc
  )
}
