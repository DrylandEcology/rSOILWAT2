###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2016}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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
setClass("swLog", slot = c(LogData = "character", MaxLines = "integer",
  UsedLines = "integer"))

setMethod(f="swClear",
		signature="swLog",
		definition=function(object) {
			object@LogData=character(150)
			object@MaxLines=as.integer(150)
			object@UsedLines=integer(1)
			return(object)
		})

setMethod("initialize", signature = "swLog", function(.Object, ...) {
  def <- slot(inputData, "log")

  # We don't set values for any slots; this is to prevent simulation runs with
  # accidentally incorrect values
  .Object@MaxLines <- 150L
  .Object@LogData <- character(.Object@MaxLines)
  .Object@UsedLines <- 1L

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})
