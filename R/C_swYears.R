###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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

#' @export
setClass("swYears", slots = c(StartYear = "integer", EndYear = "integer",
  FDOFY = "integer", EDOEY = "integer", isNorth = "logical"))

setMethod("initialize", signature = "swYears", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "years")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slots `StartYear` and `EndYear` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("StartYear" %in% dns)) def@StartYear <- NA_integer_
  if (!("EndYear" %in% dns)) def@EndYear <- NA_integer_

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

swYears_validity <- function(object) {
  val <- TRUE

  if (length(object@StartYear) != 1 || (!is.na(object@StartYear) && object@StartYear < 0)) {
    msg <- "There must be exactly one non-negative @StartYear value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@EndYear) != 1 || (!is.na(object@EndYear) && object@EndYear < 0) ||
    (!is.na(object@EndYear) && !is.na(object@StartYear) && object@EndYear < object@StartYear)) {
    msg <- paste("There must be exactly one non-negative @EndYear value that is",
      "not smaller than @StartYear.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@FDOFY) != 1 || !is.finite(object@FDOFY) || object@FDOFY < 0 ||
    object@FDOFY > 365) {
    msg <- paste("There must be exactly one non-negative finite @FDOFY value that is",
      "smaller than day 366.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@EDOEY) != 1 || !is.finite(object@EDOEY) || object@EDOEY < 0 ||
    object@EDOEY > 366 || object@EDOEY < object@FDOFY) {
    msg <- paste("There must be exactly one non-negative finite @EDOEY value that is",
      "not larger than day 366 and larger than @FDOFY.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@isNorth) != 1 || is.na(object@isNorth)) {
    msg <- paste("There must be exactly one non-NA logical @isNorth value.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swYears", swYears_validity)




setMethod("swYears_StartYear", "swYears", function(object) object@StartYear)
setMethod("swYears_EndYear", "swYears", function(object) object@EndYear)
setMethod("swYears_FDOFY", "swYears", function(object) object@FDOFY)
setMethod("swYears_EDOEY", "swYears", function(object) object@EDOEY)
setMethod("swYears_isNorth", "swYears", function(object) object@isNorth)

setReplaceMethod("swYears_StartYear", signature = "swYears", function(object, value) {
  object@StartYear <- value
  validObject(object)
  object
})
setReplaceMethod("swYears_EndYear", signature = "swYears", function(object, value) {
  object@EndYear <- value
  validObject(object)
  object
})
setReplaceMethod("swYears_FDOFY", signature = "swYears", function(object, value) {
  object@FDOFY <- value
  validObject(object)
  object
})
setReplaceMethod("swYears_EDOEY", signature = "swYears", function(object, value) {
  object@EDOEY <- value
  validObject(object)
  object
})
setReplaceMethod("swYears_isNorth", signature = "swYears", function(object, value) {
  object@isNorth <- value
  validObject(object)
  object
})


setMethod("swReadLines", signature = c(object="swYears",file="character"), function(object,file) {
			infiletext <- readLines(con = file)
			object@StartYear = readInteger(infiletext[4])
			object@EndYear = readInteger(infiletext[5])
			object@FDOFY = readInteger(infiletext[6])
			object@EDOEY = readInteger(infiletext[7])
			temp <- unlist(strsplit(x=infiletext[8],split="\t"))
			temp <- unlist(strsplit(x=temp,split=" "))
			temp <- temp[temp != ""][1]
			if(temp == "N") object@isNorth = TRUE
			if(temp == "S") object@isNorth = FALSE
			return(object)
		})
