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
setClass("swYears", slots = c(StartYear = "integer", EndYear = "integer",
  FDOFY = "integer", EDOEY = "integer", isNorth = "logical"))

swYears_validity<-function(object){
	if(length(object@StartYear)!=1 | is.na(object@StartYear) | object@StartYear<0)
		return("@StartYear:Needstobeintegerlength1.Non-negativeandnotNA")
	if(length(object@EndYear)!=1 | is.na(object@EndYear) | object@EndYear<0 | object@EndYear<object@StartYear)
		return("@EndYear:Needstobeintegerlength1.Non-negativeandnotNA")
	if(length(object@FDOFY)!=1 | is.na(object@FDOFY) | object@FDOFY<0 | object@FDOFY>365)
		return("@FDOFY:Needstobeintegerlength1.Non-negativeandnotNA.Lessthan365")
	if(length(object@EDOEY)!=1 | is.na(object@EDOEY) | object@EDOEY<0 | object@EDOEY>366 | object@EDOEY<object@FDOFY)
		return("@EDOEY:Needstobeintegerlength1.Non-negativeandnotNA.Lessthanorequalto366.GreaterthanFirstDayofFirstYear")
	if(length(object@isNorth)!=1)
		return("@isNorthneedstobelength1.")
	TRUE
}
setValidity("swYears", swYears_validity)

setMethod("initialize", signature = "swYears", function(.Object, ...) {
  def <- slot(inputData, "years")

  # We don't set values for slots `StartYear` and `EndYear`; this is to prevent simulation runs with
  # accidentally incorrect values
  .Object@StartYear <- NA_integer_
  .Object@EndYear <- NA_integer_

  .Object@FDOFY <- def@FDOFY
  .Object@EDOEY <- def@EDOEY
  .Object@isNorth <- def@isNorth

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})




setMethod("swYears_StartYear", "swYears", function(object) {return(object@StartYear)})
setMethod("swYears_EndYear", "swYears", function(object) {return(object@EndYear)})
setMethod("swYears_FDOFY", "swYears", function(object) {return(object@FDOFY)})
setMethod("swYears_EDOEY", "swYears", function(object) {return(object@EDOEY)})
setMethod("swYears_isNorth", "swYears", function(object) {return(object@isNorth)})
setReplaceMethod(f="swYears_StartYear", signature="swYears", definition=function(object,value) {object@StartYear <- value; return(object)})
setReplaceMethod(f="swYears_EndYear", signature="swYears", definition=function(object,value) {object@EndYear <- value; return(object)})
setReplaceMethod(f="swYears_FDOFY", signature="swYears", definition=function(object,value) {object@FDOFY <- value; return(object)})
setReplaceMethod(f="swYears_EDOEY", signature="swYears", definition=function(object,value) {object@EDOEY <- value; return(object)})
setReplaceMethod(f="swYears_isNorth", signature="swYears", definition=function(object,value) {object@isNorth <- value; return(object)})


setMethod("swReadLines", signature=c(object="swYears",file="character"), definition=function(object,file) {
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
