###############################################################################
#Rsoilwat and Rsoilwat31
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

print("swWeather")

#######################Monthly Scaling Params#################################

swMonthlyScalingParams <- setClass("swMonthlyScalingParams",
									 slots = list(MonthlyScalingParams = "matrix"),
									 prototype = list(MonthlyScalingParams = 
														matrix(data = c(rep(1, 12), rep(0, 12 * 3), rep(1, 12), rep(0, 12), rep(1, 12)),
																nrow = 12, ncol = 7,
																dimnames = list(month.name, c("PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "Transmissivity"))
															)
									 				)
									)

setValidity("swMonthlyScalingParams", function(object) {
	if (identical(dim(object@MonthlyScalingParams), c(12L, 7L))) {
		TRUE
	} else {
		paste("@MonthlyScalingParams requires an input of 12 rows and 7 columns instead of", paste(dim(object@MonthlyScalingParams), collapse = " by "))
	}
})

		
setMethod("swClear", signature = "swMonthlyScalingParams", function(object) {
	slot(object, "MonthlyScalingParams") <- matrix(data = c(rep(1, 12), rep(0, 12 * 3), rep(1, 12), rep(0, 12), rep(1, 12)),
													nrow = 12, ncol = 7,
													dimnames = list(month.name, c("PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "Transmissivity")))
	
	object
})

#####################WEATHERSETUP.IN###################################

swWeather <- setClass("swWeather",
						slots = list(UseSnow = "logical",
									pct_SnowDrift = "numeric",
									pct_SnowRunoff = "numeric",
									use_Markov = "logical",
									FirstYear_Historical = "integer",
									DaysRunningAverage = "integer"),
						prototype = list(UseSnow = TRUE,
										pct_SnowDrift = 0.0,
										pct_SnowRunoff = 0.0,
										use_Markov = FALSE,
										FirstYear_Historical = 1982L,
										DaysRunningAverage = 5L),
						contains = c("swMonthlyScalingParams")
					)

setValidity("swWeather", function(object){
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


setMethod("swClear", signature = "swWeather", function(object) {
	slot(object, "UseSnow") <- NA
	slot(object, "pct_SnowDrift") <- NA_real_
	slot(object, "pct_SnowRunoff") <- NA_real_
	slot(object, "use_Markov") <- NA
	slot(object, "FirstYear_Historical") <- NA_integer_
	slot(object, "DaysRunningAverage") <- NA_integer_
	
	slot(object, "MonthlyScalingParams") <- swClear(slot(object, "MonthlyScalingParams"))

	object
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

setMethod("swWriteLines", signature=c(object="swWeather", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(28)
			infiletext[1] <- "# Weather setup parameters"
			infiletext[2] <- paste("# Location: ")
			infiletext[3] <- "#"
			
			infiletext[4] <- paste(ifelse(object@UseSnow,"1","0"), "\t# 1=allow snow accumulation,   0=no snow effects.", sep="")
			infiletext[5] <- paste(object@pct_SnowDrift, "\t# % of snow drift per snow event (+ indicates snow addition, - indicates snow taken away from site)", sep="")
			infiletext[6] <- paste(object@pct_SnowRunoff, "\t# % of snowmelt water as runoff/on per event (>0 indicates runoff, <0 indicates runon)", sep="")
			infiletext[7] <- paste(ifelse(object@use_Markov,"1","0"), "\t# 0=use historical data only, 1=use markov process for missing weather.", sep="")
			infiletext[8] <- paste(object@FirstYear_Historical, "\t# first year to begin historical weather.", sep="")
			infiletext[9] <- paste(object@DaysRunningAverage, "\t# number of days to use in the running average of temperature.", sep="")
			
			infiletext[11] <- "# Monthly scaling parameters."
			infiletext[12] <- "# Month 1 = January, Month 2 = February, etc."
			infiletext[13] <- "# PPT = multiplicative for PPT (scale*ppt)."
			infiletext[14] <- "# MaxT = additive for max temp (scale+maxtemp)."
			infiletext[15] <- "# MinT = additive for min temp (scale+mintemp)."
			infiletext[16] <- "# SkyCover = additive for mean monthly sky cover [%]; min(100, max(0, scale + sky cover))"
			infiletext[17] <- "# Wind = multiplicative for mean monthly wind speed; max(0, scale * wind speed)"
			infiletext[18] <- "# rH = additive for mean monthly relative humidity [%]; min(100, max(0, scale + rel. Humidity))"
			infiletext[19] <- "# Transmissivity = multiplicative for mean monthly relative transmissivity; min(1, max(0, scale * transmissivity))"
			infiletext[20] <- "#Mon  PPT  MaxT  MinT	SkyCover	Wind	rH	Transmissivity"
			
			for(i in 21:32) {
				infiletext[i] <- paste(format(i-20),"\t",format(object@MonthlyScalingParams[i-20,1]),"\t",
						format(object@MonthlyScalingParams[i-20,2]),"\t",format(object@MonthlyScalingParams[i-20,3]),"\t",
						format(object@MonthlyScalingParams[i-20,4]),"\t",format(object@MonthlyScalingParams[i-20,5]),"\t",
						format(object@MonthlyScalingParams[i-20,6]),"\t",format(object@MonthlyScalingParams[i-20,7]),"\t",sep="")
			}
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})


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
