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


############Spinup############
#' Class \code{"swSpinup"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swSpinup}}.
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
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swSpinup")
#' x <- new("swSpinup")
#' x <- swSpinup()
#'
#' @name swSpinup-class
#' @export
setClass(
  "swSpinup",
  slots = c(
    SpinupMode = "integer",
    SpinupScope = "integer",
    SpinupDuration = "integer",
    SpinupSeed = "integer",
    SpinupActive = "logical"
  ),
  prototype = list(
    SpinupMode = NA_integer_,
    SpinupScope = NA_integer_,
    SpinupDuration = NA_integer_,
    SpinupSeed = NA_integer_,
    SpinupActive = FALSE
  )
)


#' @rdname swSpinup-class
#' @export
swSpinup <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "spinup")
  sns <- slotNames("swSpinup")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSpinup")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSpinup", tmp))
}


setValidity(
  "swSpinup",
  function(object) {
    val <- TRUE

    if (
      length(object@SpinupMode) != 1L ||
      (object@SpinupMode != 1L &&
      object@SpinupMode != 2L)
    ) {
      msg <- paste("There must be exactly NA or one @SpinupMode ",
      "that is equal to either 1 or 2.")
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@SpinupScope) != 1L ||
        (!anyNA(object@SpinupScope) && isTRUE(object@SpinupScope < 0L)) ||
        (!anyNA(object@SpinupScope) && !anyNA(object@StartYear) &&
            isTRUE(object@SpinupScope <= object@StartYear))
    ) {
      msg <- paste(
        "The @SpinupScope must not exceed the value given for @StartYear"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@SpinupDuration) != 1L ||
      object@SpinupDuration < 0L
    ) {
      msg <- paste(
        "There must be exactly NA or one non-negative @SpinupDuration."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@SpinupSeed) != 1L ||
      !is.finite(object@SpinupSeed) ||
      object@SpinupSeed < 0L ||
      object@SpinupSeed > 366L ||
      object@SpinupSeed < object@FDOFY
    ) {
      msg <- paste(
        "There must be exactly one non-negative finite @SpinupSeed value"
        )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)



#' @rdname swSpinup-class
#' @export
setMethod("swSpinup_SpinupMode", "swSpinup",
  function(object) object@SpinupMode)
#' @rdname swSpinup-class
#' @export
setMethod("swSpinup_SpinupScope", "swSpinup",
  function(object) object@SpinupScope)
#' @rdname swSpinup-class
#' @export
setMethod("swSpinup_SpinupDuration", "swSpinup",
  function(object) object@SpinupDuration)
#' @rdname swSpinup-class
#' @export
setMethod("swSpinup_SpinupSeed", "swSpinup",
  function(object) object@SpinupSeed)


#' @rdname swSpinup-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupMode",
  signature = "swSpinup",
  function(object, value) {
    object@SpinupMode <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSpinup-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupScope",
  signature = "swSpinup",
  function(object, value) {
    object@SpinupScope <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSpinup-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupDuration",
  signature = "swSpinup",
  function(object, value) {
    object@SpinupDuration <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSpinup-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupSeed",
  signature = "swSpinup",
  function(object, value) {
    object@SpinupSeed <- value
    validObject(object)
    object
  }
)




#' @rdname swSpinup-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swSpinup", file = "character"),
  function(object, file) {
    stop("swReadLines is defunct")
    infiletext <- readLines(con = file)
    object@SpinupMode <- readInteger(infiletext[8])
    object@SpinupScope <- readInteger(infiletext[9])
    object@SpinupDuration <- readInteger(infiletext[10])
    object@SpinupSeed <- readInteger(infiletext[11])
    temp <- unlist(strsplit(x = infiletext[8], split = "\t"))
    temp <- unlist(strsplit(x = temp, split = " "))
    temp <- temp[temp != ""][1]

    object
})
# nolint end
