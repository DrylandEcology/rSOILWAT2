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
# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2016)
###############################################################################

#######################Monthly Scaling Params#################################

#' @export
setClass("swMonthlyScalingParams", slots = c(MonthlyScalingParams = "matrix"))

setValidity("swMonthlyScalingParams", function(object) {
  if (identical(dim(object@MonthlyScalingParams), c(12L, 7L))) {
		TRUE
	} else {
		paste("@MonthlyScalingParams requires an input of 12 rows and 7 columns instead of", paste(dim(object@MonthlyScalingParams), collapse = " by "))
	}
})

setMethod("initialize", signature = "swMonthlyScalingParams", function(.Object, ...) {
  def <- slot(inputData, "weather")

  .Object@MonthlyScalingParams <- def@MonthlyScalingParams

  validObject(.Object)
  .Object
})



#####################WEATHERSETUP.IN###################################

#' @export
setClass("swWeather", slots = c(UseSnow = "logical", pct_SnowDrift = "numeric",
  pct_SnowRunoff = "numeric", use_Markov = "logical", FirstYear_Historical = "integer",
  DaysRunningAverage = "integer"),
  contains = "swMonthlyScalingParams")

setValidity("swWeather", function(object) {
  msg <- NULL

	if (length(object@UseSnow) != 1)
		msg <- c(msg, "@UseSnow needs to be of length 1.")
	if (length(object@pct_SnowDrift) != 1)
		msg <- c(msg, "@pct_SnowDrift needs to be of length 1.")
	if (length(object@pct_SnowRunoff) != 1)
		msg <- c(msg, "@pct_SnowRunoff needs to be of length 1.")
	if (length(object@use_Markov) != 1)
		msg <- c(msg, "@use_Markov needs to be of length 1.")
	if (length(object@FirstYear_Historical) != 1)
		msg <- c(msg, "@FirstYear_Historical needs to be of length 1.")
	if (length(object@DaysRunningAverage) != 1)
		msg <- c(msg, "@DaysRunningAverage needs to be of length 1.")

  if (is.null(msg)) TRUE else msg
})


setMethod("initialize", signature = "swWeather", function(.Object, ...) {
  def <- slot(inputData, "weather")

  .Object@UseSnow <- def@UseSnow
  .Object@pct_SnowDrift <- def@pct_SnowDrift
  .Object@pct_SnowRunoff <- def@pct_SnowRunoff
  .Object@use_Markov <- def@use_Markov
  .Object@FirstYear_Historical <- def@FirstYear_Historical
  .Object@DaysRunningAverage <- def@DaysRunningAverage

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)

  .Object
})



setMethod("swWeather_DaysRunningAverage", "swWeather", function(object) object@DaysRunningAverage)
setMethod("swWeather_FirstYearHistorical", "swWeather", function(object) object@FirstYear_Historical)
setMethod("swWeather_pct_SnowDrift", "swWeather", function(object) object@pct_SnowDrift)
setMethod("swWeather_pct_SnowRunoff", "swWeather", function(object) object@pct_SnowRunoff)
setMethod("swWeather_UseMarkov", "swWeather", function(object) object@use_Markov)
setMethod("swWeather_UseSnow", "swWeather", function(object) object@UseSnow)
setMethod("swWeather_MonScalingParams", "swWeather", function(object) object@MonthlyScalingParams)

setReplaceMethod("swWeather_DaysRunningAverage", signature = "swWeather",function(object,value) initialize(object, DaysRunningAverage = value))
setReplaceMethod("swWeather_FirstYearHistorical", signature = "swWeather",function(object,value) initialize(object, FirstYear_Historical = value))
setReplaceMethod("swWeather_pct_SnowDrift", signature = "swWeather",function(object,value) initialize(object, pct_SnowDrift = value))
setReplaceMethod("swWeather_pct_SnowRunoff", signature = "swWeather",function(object,value) initialize(object, pct_SnowRunoff = value))
setReplaceMethod("swWeather_UseMarkov", signature = "swWeather",function(object,value) initialize(object, use_Markov = value))
setReplaceMethod("swWeather_UseSnow", signature = "swWeather",function(object,value) initialize(object, UseSnow = value))
setReplaceMethod("swWeather_MonScalingParams", signature = "swWeather", function(object, value) initialize(object, MonthlyScalingParams = value))



setMethod("swReadLines", signature=c(object="swWeather",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)

			object@UseSnow = readLogical(infiletext[4])
			object@pct_SnowDrift = readNumeric(infiletext[5])
			object@pct_SnowRunoff = readNumeric(infiletext[6])
			object@use_Markov = readLogical(infiletext[7])
			object@FirstYear_Historical = readInteger(infiletext[8])
			object@DaysRunningAverage = readInteger(infiletext[9])

			data=matrix(data=c(rep(1,12),rep(NA,12*6)),nrow=12,ncol=7)
			colnames(data)<-c("PPT","MaxT","MinT","SkyCover","Wind","rH","Transmissivity")
			rownames(data)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
			for(i in 21:32) {
				data[i-20,] <- readNumerics(infiletext[i],8)[2:8]
			}
			object@MonthlyScalingParams = data
			return(object)
		})
