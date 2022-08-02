###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018)
###############################################################################

###############################################################SOILS###########
#' Class \code{"swSoils"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swSoils}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swSoils")
#' x <- new("swSoils")
#' x <- swSoils()
#'
#' @name swSoils-class
#' @export
setClass(
  "swSoils",
  slots = c(
    Layers = "matrix",
    SWRCp = "matrix"
  ),
  prototype = list(
    Layers = array(
      NA_real_,
      dim = c(0L, 12L),
      dimnames = list(
        NULL,
        c(
          "depth_cm", "bulkDensity_g/cm^3", "gravel_content",
          "EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
          "transpTree_frac", "transpForb_frac", "sand_frac", "clay_frac",
          "impermeability_frac", "soilTemp_c"
        )
      )
    ),
    SWRCp = array(
      NA_real_,
      dim = c(0L, 6L),
      dimnames = list(
        NULL,
        paste0("Param", seq_len(6L))
      )
    )
  )
)


setValidity(
  "swSoils",
  function(object) {
    val <- TRUE
    tmpL <- dim(object@Layers)
    tmpp <- dim(object@SWRCp)
    dtol1 <- 1. + tmpL[1] * rSW2_glovars[["tol"]]

    #--- Check "Layers"
    if (tmpL[2] != 12L) {
      msg <- paste(
        "@Layers must have exactly 12 columns corresponding to",
        "depth_cm, bulkDensity_g/cm^3, gravel_content, EvapBareSoil_frac,",
        "transpGrass_frac,transpShrub_frac, transpTree_frac, transpForb_frac,",
        "sand_frac, clay_frac, impermeability_frac, soilTemp_c"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    depths <- object@Layers[, 1L]

    if (
      !(
        all(is.na(depths)) ||
          all(depths > 0.) &&
          !anyNA(
            rSW2utils::check_monotonic_increase(
              depths,
              MARGIN = 2L,
              strictly = TRUE
            )
          )
      )
    ) {
      msg <- paste(
        "@Layers[, 'depth_cm'] must be positive, strictly increasing depths",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }


    tmp <- object@Layers[, 3L:11L]
    if (!(all(is.na(tmp)) || all(tmp >= 0., tmp <= dtol1))) {
      msg <- paste(
        "@Layers values of gravel, evco, trcos, sand, clay, and",
        "impermeability must be between 0 and 1",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    tmp <- colSums(object@Layers[, 4L:8L, drop = FALSE])
    if (!(all(is.na(tmp)) || all(tmp <= dtol1, na.rm = TRUE))) {
      msg <- paste(
        "@Layers values of profile sums of evco and trcos must be",
        "between 0 and 1",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    #--- Check "SWRCp"
    # `SW_SIT_init_run()` will call function to check validity of SWRCp values
    if (tmpp[1L] != tmpL[1L]) {
      msg <- paste(
        "@SWRCp must have exactly the same number of soil layers (rows)",
        "as @Layers."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (
      tmpp[2L] != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]]
    ) {
      msg <- paste(
        "@SWRCp must have exactly",
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]],
        "columns."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swSoils-class
#' @export
swSoils <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "soils")
  sns <- slotNames("swSoils")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSoils")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slot `Layers` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("Layers" %in% dns)) {
    def@Layers <- def@Layers[1, , drop = FALSE]
    def@Layers[] <- NA_real_
    ntmp <- 1
  } else {
    # Guarantee names
    dimnames(dots[["Layers"]]) <- list(NULL, colnames(def@Layers))
    ntmp <- nrow(dots[["Layers"]])
  }

  # We don't set values for slot `SWRCp` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("SWRCp" %in% dns)) {
    def@SWRCp <- def@SWRCp[rep.int(1, ntmp), , drop = FALSE]
    def@SWRCp[] <- NA_real_
  } else {
    # Guarantee names
    dimnames(dots[["SWRCp"]]) <- list(NULL, colnames(def@SWRCp))
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSoils", tmp))
}



#' @rdname sw_upgrade
#' @export
setMethod(
  "sw_upgrade",
  signature = "swSoils",
  definition = function(object, verbose = FALSE) {
    #--- Make sure we have SWRC parameters
    tmp <- try(object@SWRCp, silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swSoils`.")
      }
      object <- suppressWarnings(swSoils(object))
    }

    object
  }
)


#' @rdname swSoils-class
#' @export
setMethod("get_swSoils", "swSoils", function(object) object)

#' @rdname swSoils-class
#' @export
setMethod("swSoils_Layers", "swSoils", function(object) object@Layers)

#' @rdname swSoils-class
#' @export
setMethod("swSoils_SWRCp", "swSoils", function(object) object@SWRCp)

#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swSoils", value = "swSoils"),
  function(object, value) {
    colnames(value@Layers) <- colnames(object@Layers)
    colnames(value@SWRCp) <- colnames(object@SWRCp)
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swSoils", value = "list"),
  function(object, value) {
    idl <- if (utils::hasName(value, "Layers")) "Layers" else 1
    idp <- if (utils::hasName(value, "SWRCp")) "SWRCp" else 2
    colnames(value[[idl]]) <- colnames(object@Layers)
    colnames(value[[idp]]) <- colnames(object@SWRCp)
    object@Layers <- data.matrix(value[[idl]])
    object@SWRCp <- data.matrix(value[[idp]])
    validObject(object)
    object
  }
)

#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "swSoils_Layers",
  signature = "swSoils",
  function(object, value) {
    colnames(value) <- colnames(object@Layers)
    object@Layers <- data.matrix(value)
    # Note: validity check fails if number of soil layers disagrees with
    # number of of soil layers of SWRC parameters
    # --> see method for "swInputData" that can automatically resizes SWRCp
    validObject(object)
    object
  }
)


#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "swSoils_SWRCp",
  signature = "swSoils",
  function(object, value) {
    colnames(value) <- colnames(object@SWRCp)
    object@SWRCp <- data.matrix(value)
    validObject(object)
    object
  }
)




#' @rdname swSoils-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swSoils", file = "character"),
  function(object, file) {
    stop("This function no longer works correctly; and SWRCp are not read.")
    infiletext <- readLines(con = file)
    infiletext <- infiletext[infiletext != ""] #get rid of extra spaces
    infiletext <- infiletext[17:length(infiletext)] #get rid of comments
    object@Layers <- matrix(data = NA, nrow = length(infiletext), ncol = 12)
    colnames(object@Layers) <- c("depth_cm", "bulkDensity_g/cm^3",
      "gravel_content", "EvapBareSoil_frac", "transpGrass_frac",
      "transpShrub_frac", "transpTree_frac", "transpForb_frac",
      "sand_frac", "clay_frac", "impermeability_frac", "soilTemp_c")
    for (i in seq_along(infiletext)) {
      object@Layers[i, ] <- readNumerics(infiletext[i], 12)
    }

    object
  }
)
# nolint end
