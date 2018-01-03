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


#Remember this models the C code so index starts at 0 not 1
timePeriods <- c("dy", "wk", "mo", "yr")

#######
#Note I use 0 for keys which are not implemented.
#######
#' @export
setClass("swOUT_key", slots = c(mykey = "integer", myobj = "integer", period = "integer",
  sumtype = "integer", use = "logical", first = "integer", last = "integer",
  first_orig = "integer", last_orig = "integer", outfile = "character"))

setValidity("swOUT_key", function(object) {
  temp <- c(object@mykey, object@myobj, object@period, object@sumtype, object@use,
    object@first, object@last, object@first_orig, object@last_orig, object@outfile)

  if (any(!lapply(temp, function(x) length(x) == 1)))
    return("Missing values...")

  TRUE
})

setMethod("initialize", signature = "swOUT_key", function(.Object, ...) {
  def <- slot(inputData, "output")

  .Object@mykey <- def@mykey
  .Object@myobj <- def@myobj
  .Object@period <- def@period
  .Object@sumtype <- def@sumtype
  .Object@use <- def@use
  .Object@first <- def@first
  .Object@last <- def@last
  .Object@first_orig <- def@first_orig
  .Object@last_orig <- def@last_orig
  .Object@outfile <- def@outfile

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})


###########################OUTSETUP.IN########################################

#' @export
setClass("swOUT", slot = c(outputSeparator = "character", timePeriods = "integer",
  useTimeStep = "logical"), contains = "swOUT_key")

swOUT_validity<-function(object){
	if(length(object@outputSeparator)!=1)
		return("@outputSeparator needs to be of length 1.")
	if(length(object@timePeriods) < 1)
		return("@timePeriods needs to to contain at least 1 value for output")
	if(length(object@useTimeStep)!=1)
		return("@useTimeStep needs to be of length 1.")
}
setValidity("swOUT", swOUT_validity)


setMethod("initialize", signature = "swOUT", function(.Object, ...) {
  def <- slot(inputData, "output")

  .Object@outputSeparator <- def@outputSeparator
  .Object@timePeriods <- def@timePeriods
  .Object@useTimeStep <- def@useTimeStep

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)
  .Object
})


setMethod("get_swOUT", "swOUT", function(object) {return(object)})
setMethod("swOUT_TimeStep","swOUT",function(object) {return(object@timePeriods)})
setMethod("swOUT_OutputSeparator","swOUT",function(object) {return(object@outputSeparator)})
setMethod("swOUT_useTimeStep","swOUT",function(object) {return(object@useTimeStep)})

setReplaceMethod(f="set_swOUT",signature="swOUT",function(object,value) {object <- value; return(object)})
setReplaceMethod(f="swOUT_TimeStep",signature="swOUT",function(object,value) {value<-as.integer(value); object@timePeriods <- value; return(object)})
setReplaceMethod(f="swOUT_OutputSeparator",signature="swOUT",function(object,value) {object@outputSeparator <- value; return(object)})
setReplaceMethod(f="swOUT_useTimeStep",signature="swOUT",function(object,value) {object@useTimeStep <- value; return(object)})


# used by swReadLines
			KEY <- c("WTHR", "TEMP", "PRECIP", "SOILINFILT", "RUNOFF", "ALLH2O", "VWCBULK",
				"VWCMATRIC", "SWCBULK", "SWABULK", "SWAMATRIC", "SWPMATRIC","SURFACEWATER", "TRANSP",
				"EVAPSOIL", "EVAPSURFACE", "INTERCEPTION", "LYRDRAIN", "HYDRED", "ET", "AET", "PET",
				"WETDAY", "SNOWPACK", "DEEPSWC", "SOILTEMP", "ALLVEG", "ESTABL")
OutSum <- c("off", "sum", "avg", "fnl") # only used for 'swReadLines'


setMethod("swReadLines", signature=c(object="swOUT",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			if(temp<-strsplit(infiletext[41],split=" ")[[1]][2] == "t") {
				object@outputSeparator="\t"
			} else if(temp == "s") {
				object@outputSeparator=" "
			} else {
				object@outputSeparator="\t"
			}

			if(infiletext[42]==""){
				object@useTimeStep = FALSE
			} else {
				object@useTimeStep = TRUE
				temp<-strsplit(x=infiletext[42],split=" ")[[1]][-1]
				object@timePeriods = as.integer(sapply(1:length(temp), FUN=function(i) which(temp[i] == timePeriods))-1)
			}

			for(i in 45:length(infiletext)) {
				if(infiletext[i] != "") {
					temp<-strsplit(x=infiletext[i],split="\t")[[1]]
					temp<-unlist(strsplit(x=temp,split=" "))
					temp <- temp[temp != ""][1:6]
					mykey<- as.integer(grep(pattern=temp[1],x=KEY)[1])
					sumtype <- as.integer(grep(pattern=temp[2],x=OutSum))-1
					period <- which(tolower(temp[3]) == timePeriods)-1
					start <- as.integer(temp[4])
					if(grepl(pattern="end",x=temp[5])) {
						end <- as.integer(366)
					} else {
						end <- as.integer(temp[5])
					}
					object@mykey[mykey] = as.integer(mykey-1)
					object@sumtype[mykey] = as.integer(sumtype)
					object@period[mykey] = as.integer(period)
					object@first_orig[mykey] = start
					object@last_orig[mykey] = end
					object@outfile[mykey] = temp[6]
					if(object@sumtype[mykey] != 0) {
						object@use[mykey] = TRUE
					} else {
						object@use[mykey] = FALSE
					}
				}
			}
			return(object)
		})

