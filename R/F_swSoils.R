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
#' @param .Object An object of class \code{\linkS4class{swSoils}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
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
#'
#' @name swSoils-class
#' @export
setClass("swSoils", slots = c(Layers = "matrix"))

swSoilLayers_validity <- function(object) {
  val <- TRUE
  temp <- dim(object@Layers)
  dtol1 <- 1 + temp[1] * rSW2_glovars[["tol"]]

  if (temp[1] == 0) {
    msg <- "@Layers must have at least one row/soil layer."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (temp[2] != 12) {
    msg <- paste(
      "@Layers must have exactly 12 columns corresponding to",
      "depth_cm, bulkDensity_g/cm^3, gravel_content, EvapBareSoil_frac,",
      "transpGrass_frac,transpShrub_frac, transpTree_frac, transpForb_frac,",
      "sand_frac, clay_frac, impermeability_frac, soilTemp_c"
    )
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (!all(is.na(object@Layers[, 1])) && (any(object@Layers[, 1] <= 0) ||
    any(diff(object@Layers[, 1]) < rSW2_glovars[["tol"]]))) {
    msg <- "@Layers['depth_cm', ] must be positive increasing depths."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (!all(is.na(object@Layers[, 3:11])) && (any(object@Layers[, 3:11] < 0) ||
    any(object@Layers[, 3:11] > dtol1))) {
    msg <- paste("@Layers values of gravel, evco, trcos, sand, clay, and",
      "impermeability must be between 0 and 1.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  temp <- colSums(object@Layers[, 4:8, drop = FALSE])
  if (any(temp > dtol1, na.rm = TRUE)) {
    msg <- paste("@Layers values of profile sums of evco and trcos must be",
      "between 0 and 1.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swSoils", swSoilLayers_validity)

#' @rdname swSoils-class
#' @export
setMethod("initialize", signature = "swSoils", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "soils")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slot `Layers` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("Layers" %in% dns)) {
    def@Layers <- def@Layers[1, , drop = FALSE]
    def@Layers[] <- NA_real_
  } else {
    # Guarantee dimnames
    dimnames(dots[["Layers"]]) <- dimnames(def@Layers)
  }

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  if (FALSE) {
    # not needed because no relevant inheritance
    .Object <- callNextMethod(.Object, ...)
  }

  validObject(.Object)
  .Object
})


#' @rdname swSoils-class
#' @export
setMethod("get_swSoils", "swSoils", function(object) object)

#' @rdname swSoils-class
#' @export
setMethod("swSoils_Layers", "swSoils", function(object) object@Layers)

#' @rdname swSoils-class
#' @export
setReplaceMethod("set_swSoils",
  signature = c(object = "swSoils", value = "swSoils"),
  function(object, value) {
    colnames(value@Layers) <- colnames(object@Layers)
    object <- value
    validObject(object)
    object
})

#' @rdname swSoils-class
#' @export
setReplaceMethod("swSoils_Layers",
  signature = c(object = "swSoils", value = "matrix"),
  function(object, value) {
    colnames(value) <- colnames(object@Layers)
    object@Layers <- value
    validObject(object)
    object
})


#' @rdname swSoils-class
#' @export
# nolint start
setMethod("swReadLines",
  signature = c(object = "swSoils", file = "character"),
  function(object, file) {
    print("TODO: method 'swReadLines' is not up-to-date; hard-coded indices are incorrect")
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
})
# nolint end
