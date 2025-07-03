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


########################CLOUD DATA################################
# TODO: consider individual slots for each row of the 5 x 12 matrix

#' Class \code{"swCloud"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swCloud}}.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso
#' \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}}
#' \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}}
#' \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}}
#' \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swSpinup}}
#' \code{\linkS4class{swEstab}}
#' \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swCarbon}}
#' \code{\linkS4class{swSWC}}
#' \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swCloud")
#' x <- new("swCloud")
#' x <- swCloud()
#'
#' @name swCloud-class
#' @export
setClass(
  "swCloud",
  slots = c(Cloud = "matrix"),
  prototype = list(
    Cloud = array(
      NA_real_,
      dim = c(5, 12),
      dimnames = list(
        c(
          "SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT", "SnowDensity_kg/m^3",
          "RainEvents_per_day"
        ),
        c(
          "January", "February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December"
        )
      )
    )
  )
)


setValidity(
  "swCloud",
  function(object) {
    val <- TRUE
    temp <- dim(object@Cloud)

    if (temp[1] != 5) {
      msg <- paste(
        "@Cloud must have exactly 5 rows corresponding to",
        "SkyCoverPCT, WindSpeed_m/s, HumidityPCT,",
        "SnowDensity_kg/m^3, and RainEvents_per_day"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (temp[2] != 12) {
      msg <- "@Cloud must have exactly 12 columns corresponding months."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!all(is.na(object@Cloud[1, ])) && (any(object@Cloud[1, ] < 0) ||
        any(object@Cloud[1, ] > 100))) {
      msg <- "@Cloud['SkyCoverPCT', ] must be values between 0 and 100%."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!all(is.na(object@Cloud[2, ])) && (any(object@Cloud[2, ] < 0))) {
      msg <- "@Cloud['WindSpeed_m/s', ] must be values >= 0."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!all(is.na(object@Cloud[3, ])) && (any(object@Cloud[3, ] < 0) ||
        any(object@Cloud[3, ] > 100))) {
      msg <- "@Cloud['HumidityPCT', ] must be values between 0 and 100%."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!all(is.na(object@Cloud[4, ])) && any(object@Cloud[4, ] < 0)) {
      msg <- "@Cloud['SnowDensity_kg/m^3', ] must be values >= 0."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!all(is.na(object@Cloud[5, ])) && any(object@Cloud[5, ] < 1)) {
      msg <- "@Cloud['RainEvents_per_day', ] must be values >= 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swCloud-class
#' @export
swCloud <- function(...) {
  # Copy from SOILWAT2 "testing", but dot arguments take precedence
  def <- slot(rSOILWAT2::sw_exampleData, "cloud")
  sns <- slotNames("swCloud")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swCloud")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slot `Cloud` (except SnowDensity and RainEvents)
  # if not passed via ...; this is to prevent simulation runs with
  # accidentally incorrect values
  if ("Cloud" %in% dns) {
    # Guarantee names
    dimnames(dots[["Cloud"]]) <- dimnames(def@Cloud)
  } else {
    ids <- 4:5
    def@Cloud[- ids, ] <- NA_real_
  }

  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swCloud", tmp))
}


#' @rdname swCloud-class
#' @export
setMethod("get_swCloud", "swCloud", function(object) object)
#' @rdname swCloud-class
#' @export
setMethod("swCloud_SkyCover", "swCloud", function(object) object@Cloud[1, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_WindSpeed", "swCloud", function(object) object@Cloud[2, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_Humidity", "swCloud", function(object) object@Cloud[3, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_SnowDensity", "swCloud", function(object) object@Cloud[4, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_RainEvents", "swCloud", function(object) object@Cloud[5, ])

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "set_swCloud",
  signature = "swCloud",
  function(object, value) {
    dimnames(value@Cloud) <- dimnames(object@Cloud)
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "swCloud_SkyCover",
  signature = "swCloud",
  function(object, value) {
    object@Cloud[1, ] <- value
    validObject(object)
    object
  }
)

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "swCloud_WindSpeed",
  signature = "swCloud",
  function(object, value) {
    object@Cloud[2, ] <- value
    validObject(object)
    object
  }
)

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "swCloud_Humidity",
  signature = "swCloud",
  function(object, value) {
    object@Cloud[3, ] <- value
    validObject(object)
    object
  }
)

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "swCloud_SnowDensity",
  signature = "swCloud",
  function(object, value) {
    object@Cloud[4, ] <- value
    validObject(object)
    object
  }
)

#' @rdname swCloud-class
#' @export
setReplaceMethod(
  "swCloud_RainEvents",
  signature = "swCloud",
  function(object, value) {
    object@Cloud[5, ] <- value
    validObject(object)
    object
  }
)
