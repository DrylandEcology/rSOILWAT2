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


# TODO: Add comment
#
# Author: Ryan J. Murphy (2013)
###############################################################################


#' Class \code{"swYears"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swYears}}.
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
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swYears")
#' x <- new("swYears")
#' x <- swYears()
#'
#' @name swYears-class
#' @export
setClass(
  "swYears",
  slots = c(
    StartYear = "integer",
    EndYear = "integer",
    FDOFY = "integer",
    EDOEY = "integer",
    isNorth = "logical"
  ),
  prototype = list(
    StartYear = NA_integer_,
    EndYear = NA_integer_,
    FDOFY = NA_integer_,
    EDOEY = NA_integer_,
    isNorth = NA
  )
)

#' @rdname swYears-class
#' @export
swYears <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "years")
  sns <- slotNames("swYears")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swYears")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slots `StartYear` and `EndYear` if not passed
  # via ...; this is to prevent simulation runs with accidentally incorrect
  # values
  if (!("StartYear" %in% dns)) def@StartYear <- NA_integer_
  if (!("EndYear" %in% dns)) def@EndYear <- NA_integer_

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swYears", tmp))
}


setValidity(
  "swYears",
  function(object) {
    val <- TRUE

    if (
      length(object@StartYear) != 1 ||
      (!anyNA(object@StartYear) && isTRUE(object@StartYear < 0))
    ) {
      msg <- "There must be exactly one NA or non-negative @StartYear value."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@EndYear) != 1 ||
        (!anyNA(object@EndYear) && isTRUE(object@EndYear < 0)) ||
        (!anyNA(object@EndYear) && !anyNA(object@StartYear) &&
            isTRUE(object@EndYear < object@StartYear))
    ) {
      msg <- paste(
        "There must be exactly NA or one non-negative @EndYear value ",
        "that is not smaller than @StartYear."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@FDOFY) != 1 ||
      !is.finite(object@FDOFY) ||
      object@FDOFY < 0 ||
      object@FDOFY > 365
    ) {
      msg <- paste(
        "There must be exactly one non-negative finite @FDOFY value",
        "that is smaller than day 366."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@EDOEY) != 1 ||
      !is.finite(object@EDOEY) ||
      object@EDOEY < 0 ||
      object@EDOEY > 366 ||
      object@EDOEY < object@FDOFY
    ) {
      msg <- paste(
        "There must be exactly one non-negative finite @EDOEY value",
        "that is not larger than day 366 and larger than @FDOFY."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@isNorth) != 1 || is.na(object@isNorth)) {
      msg <- "There must be exactly one non-NA logical @isNorth value."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)





#' @rdname swYears-class
#' @export
setMethod("swYears_StartYear", "swYears", function(object) object@StartYear)
#' @rdname swYears-class
#' @export
setMethod("swYears_EndYear", "swYears", function(object) object@EndYear)
#' @rdname swYears-class
#' @export
setMethod("swYears_FDOFY", "swYears", function(object) object@FDOFY)
#' @rdname swYears-class
#' @export
setMethod("swYears_EDOEY", "swYears", function(object) object@EDOEY)
#' @rdname swYears-class
#' @export
setMethod("swYears_isNorth", "swYears", function(object) object@isNorth)

#' @rdname swYears-class
#' @export
setReplaceMethod(
  "swYears_StartYear",
  signature = "swYears",
  function(object, value) {
    object@StartYear <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swYears-class
#' @export
setReplaceMethod(
  "swYears_EndYear",
  signature = "swYears",
  function(object, value) {
    object@EndYear <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swYears-class
#' @export
setReplaceMethod(
  "swYears_FDOFY",
  signature = "swYears",
  function(object, value) {
    object@FDOFY <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swYears-class
#' @export
setReplaceMethod(
  "swYears_EDOEY",
  signature = "swYears",
  function(object, value) {
    object@EDOEY <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swYears-class
#' @export
setReplaceMethod(
  "swYears_isNorth",
  signature = "swYears",
  function(object, value) {
    object@isNorth <- as.logical(value)
    validObject(object)
    object
  }
)


#' @rdname swYears-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swYears", file = "character"),
  function(object, file) {
    stop("swReadLines is defunct")
    infiletext <- readLines(con = file)
    object@StartYear <- readInteger(infiletext[4])
    object@EndYear <- readInteger(infiletext[5])
    object@FDOFY <- readInteger(infiletext[6])
    object@EDOEY <- readInteger(infiletext[7])
    temp <- unlist(strsplit(x = infiletext[8], split = "\t"))
    temp <- unlist(strsplit(x = temp, split = " "))
    temp <- temp[temp != ""][1]
    object@isNorth <- isTRUE(temp == "N")

    object
})
# nolint end
