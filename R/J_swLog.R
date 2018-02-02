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


#' Class \code{"swLog"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param .Object An object of class \code{\linkS4class{swLog}}.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swLog")
#' x <- new("swLog")
#'
#' @name swLog-class
#' @export
setClass("swLog", slot = c(LogData = "character", MaxLines = "integer",
  UsedLines = "integer"))


#' @rdname swLog-class
#' @export
setMethod("initialize", signature = "swLog", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "log")

  # We don't set values for any slots; this is to prevent simulation runs with
  # accidentally incorrect values
  .Object@MaxLines <- 150L
  .Object@LogData <- character(.Object@MaxLines)
  .Object@UsedLines <- 1L

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})
