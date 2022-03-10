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

##############################################################################

#' Class \code{"swWeatherData"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swWeatherData}}.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @slot year An integer value. The calendar year of the weather \code{data}
#'   object.
#' @slot data A 365 x 4 or 366 x 4 matrix representing daily weather data for
#'   one calendar \code{year} with columns \var{DOY}, \var{Tmax_C},
#'   \var{Tmin_C}, and \var{PPT_cm}.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swWeatherData")
#' x <- new("swWeatherData")
#' x <- swWeatherData()
#'
#' @name swWeatherData-class
#' @export
setClass(
  "swWeatherData",
  slots = c(data = "matrix", year = "integer"),
  prototype = list(
    # NOTE: 999 should be rSW2_glovars[["kSOILWAT2"]][["kNUM"]][["SW_MISSING"]]
    data = array(
      data = c(1:366, rep(999, 366 * 3)),
      dim = c(366, 4),
      dimnames = list(NULL, c("DOY", "Tmax_C", "Tmin_C", "PPT_cm"))
    ),
    year = NA_integer_
  )
)

setValidity(
  "swWeatherData",
  function(object) {
    val <- TRUE

    if (!(length(object@year) == 1 && isTRUE(is.finite(object@year)) &&
        isTRUE(object@year >= 0))) {
      msg <- "@year must be exactly one positive finite value."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    tmp <- dim(object@data)
    if (tmp[2] != 4) {
      msg <- paste(
        "@data must have exactly 4 columns corresponding to",
        "DOY, Tmax_C, Tmin_C, PPT_cm"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (!(tmp[1] %in% c(365, 366))) {
      msg <- "@data must 365 or 366 rows corresponding to day of year."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swWeatherData-class
#' @export
swWeatherData <- function(...) {
  # We don't use default values for slots `year` and `data`; this is to prevent
  # simulation runs with accidentally incorrect values
  def <- new("swWeatherData")
  sns <- slotNames(def)
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swWeatherData")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # Guarantee names
  if ("data" %in% dns) {
    dimnames(dots[["data"]]) <- dimnames(slot(def, "data"))
  }

  if ("year" %in% dns) {
    dots[["year"]] <- as.integer(dots[["year"]])
  }

  do.call("new", args = c("swWeatherData", dots[dns %in% sns]))
}


#' @rdname swWeatherData-class
#' @export
weatherHistory <- function(weatherList = NULL) {
  if (isTRUE(dbW_check_weatherData(weatherList))) {
    weatherList
  } else {
    list(swWeatherData())
  }
}


#' @rdname swWeatherData-class
#' @export
setMethod(
  "swReadLines",
  signature = c(object = "swWeatherData", file = "character"),
  function(object, file) {
    object@year <- as.integer(
      strsplit(
        x = basename(file),
        split = ".",
      fixed = TRUE
      )[[1]][2]
    )
    data <- utils::read.table(
      file,
      header = FALSE,
      comment.char = "#",
      blank.lines.skip = TRUE,
      sep = "\t"
    )
    colnames(data) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    object@data <- as.matrix(data)

    object
})
