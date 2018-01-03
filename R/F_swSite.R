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


###############################################################SITE#####################################################################
#' @export
setClass("swSite", slots = c(SWClimits = "numeric", ModelFlags = "logical",
  ModelCoefficients = "numeric", SnowSimulationParameters = "numeric",
  DrainageCoefficient = "numeric", EvaporationCoefficients = "numeric",
  TranspirationCoefficients = "numeric", IntrinsicSiteParams = "numeric",
  SoilTemperatureFlag = "logical", SoilTemperatureConstants = "numeric",
  TranspirationRegions = "matrix"))

setValidity("swSite", function(object) {
  val <- TRUE

  if (length(object@SWClimits) != 3) {
    msg <- "@SWClimits length != 3."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@ModelFlags) != 2) {
    msg <- "@ModelFlags length != 2."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@ModelCoefficients) != 3) {
    msg <- "@ModelCoefficients length != 3."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  x <- slot(object, "ModelCoefficients")[1]
  if (!is.na(x) && x < 0) {
    msg <- paste("@ModelCoefficients:PETmultiplier =", x, "must be a non-negative number")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  x <- slot(object, "ModelCoefficients")[2]
  if (!is.na(x) && !(x >= 0 && x <= 1)) {
    msg <- paste("@ModelCoefficients:DailyRunoff =", x, "must be a number between 0 and 1",
      "(inclusive)")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  x <- slot(object, "ModelCoefficients")[3]
  if (!is.na(x) && x < 0) {
    msg <- paste("@ModelCoefficients:DailyRunon =", x, "must be a non-negative number")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@SnowSimulationParameters) != 5) {
    msg <- "@SnowSimulationParameters length != 5."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@DrainageCoefficient) != 1) {
    msg <- "@DrainageCoefficient length != 1."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@EvaporationCoefficients) != 4) {
    msg <- "@EvaporationCoefficients length != 4."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@TranspirationCoefficients) != 4) {
    msg <- "@TranspirationCoefficients length != 4."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@IntrinsicSiteParams) != 4) {
    msg <- "@IntrinsicSiteParams length != 4."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@SoilTemperatureFlag) != 1) {
    msg <- "@SoilTemperatureFlag length != 1."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (length(object@SoilTemperatureConstants) != 10) {
    msg <- "@SoilTemperatureConstants length != 10."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (NCOL(object@TranspirationRegions) != 2) {
    msg <- "@TranspirationRegions columns != 2."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
})

setMethod(f = "initialize", signature = "swSite", definition = function(.Object,
  SWClimits = c(-1, 15, 15), ModelFlags = c(FALSE, TRUE),
  ModelCoefficients = c(1, 0.0, 0.0), SnowSimulationParameters = c(.61, 1.54, .1, 0, .27),
  DrainageCoefficient = 0.02, EvaporationCoefficients = c(45, 0.1, 0.25, 0.5),
  TranspirationCoefficients = c(45, 0.1, 0.50, 1.10),
  IntrinsicSiteParams = c(0.681, 1651, 0, -1), SoilTemperatureFlag = FALSE,
  SoilTemperatureConstants = c(300, 15, -4, 600, 0.00070, 0.00030, 0.18, 6.69, 15, 990),
  TranspirationRegions = matrix(data = c(1, 2, 3, 6, 9, 11), nrow = 3, ncol = 2,
  dimnames = list(NULL, c("ndx", "layer"))), ...) {

  .Object <- callNextMethod(.Object, ...)

  slot(.Object, "SWClimits") <- SWClimits
  slot(.Object, "ModelFlags") <- ModelFlags
  slot(.Object, "ModelCoefficients") <- ModelCoefficients
  slot(.Object, "SnowSimulationParameters") <- SnowSimulationParameters
  slot(.Object, "DrainageCoefficient") <- DrainageCoefficient
  slot(.Object, "EvaporationCoefficients") <- EvaporationCoefficients
  slot(.Object, "TranspirationCoefficients") <- TranspirationCoefficients
  slot(.Object, "IntrinsicSiteParams") <- IntrinsicSiteParams
  slot(.Object, "SoilTemperatureFlag") <- SoilTemperatureFlag
  slot(.Object, "SoilTemperatureConstants") <- SoilTemperatureConstants
  slot(.Object, "TranspirationRegions") <- TranspirationRegions

  validObject(.Object)

  # Use standardized names
  colnames(slot(.Object, "TranspirationRegions")) <- c("ndx", "layer")
  names(slot(.Object, "SWClimits")) <- c("swc_min", "swc_init", "swc_wet")
  names(slot(.Object, "ModelFlags")) <- c("Reset", "DeepDrain")
  names(slot(.Object, "ModelCoefficients")) <- c("PETmultiplier", "DailyRunoff", "DailyRunon")
  names(slot(.Object, "SnowSimulationParameters")) <- c("TminAccu2", "TmaxCrit", "lambdaSnow",
    "RmeltMin", "RmeltMax")
  names(slot(.Object, "DrainageCoefficient")) <- c("SlowDrainCoefficientPerYear_cm/dy")
  names(slot(.Object, "TranspirationCoefficients")) <-
    names(slot(.Object, "EvaporationCoefficients")) <-
    c("RateShift", "RateSlope", "InflectionPoint", "Range")
  names(slot(.Object, "IntrinsicSiteParams")) <- c("Latitude", "Altitude", "Slope", "Aspect")
  names(slot(.Object, "SoilTemperatureFlag")) <- c("CalculateSoilTemp")
  names(slot(.Object, "SoilTemperatureConstants")) <- c("BiomassLimiter_g/m^2", "T1constant_a",
    "T1constant_b", "T1constant_c", "cs_constant_SoilThermCondct", "cs_constant",
    "sh_constant_SpecificHeatCapacity", "ConstMeanAirTemp", "deltaX_Param", "MaxDepth")

  .Object
})

setMethod(f = "swClear", signature = "swSite", definition = function(object) {
  initialize(object,
    SWClimits = rep(NA_real_, 3), ModelFlags = rep(NA, 2),
    ModelCoefficients = rep(NA_real_, 3),
    SnowSimulationParameters = rep(NA_real_, 5), DrainageCoefficient = NA_real_,
    EvaporationCoefficients = rep(NA_real_, 4),
    TranspirationCoefficients = rep(NA_real_, 4), IntrinsicSiteParams = rep(NA_real_, 4),
    SoilTemperatureFlag = NA, SoilTemperatureConstants = rep(NA_real_, 10),
    TranspirationRegions = matrix(data = NA_integer_, nrow = 3, ncol = 2))
})



setMethod("swSite_SWClimits", "swSite", function(object) slot(object, "SWClimits"))
setMethod("swSite_ModelFlags", "swSite", function(object) slot(object, "ModelFlags"))
setMethod("swSite_ModelCoefficients", "swSite", function(object) slot(object, "ModelCoefficients"))
setMethod("swSite_SnowSimulationParams", "swSite", function(object) slot(object, "SnowSimulationParameters"))
setMethod("swSite_DrainageCoefficient", "swSite", function(object) slot(object, "DrainageCoefficient"))
setMethod("swSite_EvapCoefficients", "swSite", function(object) slot(object, "EvaporationCoefficients"))
setMethod("swSite_TranspCoefficients", "swSite", function(object) slot(object, "TranspirationCoefficients"))
setMethod("swSite_IntrinsicSiteParams", "swSite", function(object) slot(object, "IntrinsicSiteParams"))
setMethod("swSite_SoilTemperatureFlag", "swSite", function(object) slot(object, "SoilTemperatureFlag"))
setMethod("swSite_SoilTemperatureConsts", "swSite", function(object) slot(object, "SoilTemperatureConstants"))
setMethod("swSite_TranspirationRegions", "swSite", function(object) slot(object, "TranspirationRegions"))

setReplaceMethod(f = "swSite_SWClimits", signature = "swSite", definition = function(object, value) {
  slot(object, "SWClimits") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_ModelFlags", signature = "swSite", definition = function(object, value) {
  slot(object, "ModelFlags") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_ModelCoefficients", signature = "swSite", definition = function(object, value) {
  slot(object, "ModelCoefficients") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_SnowSimulationParams", signature = "swSite", definition = function(object, value) {
  slot(object, "SnowSimulationParameters") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_DrainageCoefficient", signature = "swSite", definition = function(object, value) {
  slot(object, "DrainageCoefficient") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_EvapCoefficients", signature = "swSite", definition = function(object, value) {
  slot(object, "EvaporationCoefficients") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_TranspCoefficients", signature = "swSite", definition = function(object, value) {
  slot(object, "TranspirationCoefficients") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_IntrinsicSiteParams", signature = "swSite", definition = function(object, value) {
  slot(object, "IntrinsicSiteParams") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_SoilTemperatureFlag", signature = "swSite", definition = function(object, value) {
  slot(object, "SoilTemperatureFlag") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_SoilTemperatureConsts", signature = "swSite", definition = function(object, value) {
  slot(object, "SoilTemperatureConstants") <- value
  validObject(object)
  object
})
setReplaceMethod(f = "swSite_TranspirationRegions", signature = "swSite", definition = function(object, value) {
  slot(object, "TranspirationRegions") <- value
  validObject(object)
  object
})



setMethod("swReadLines", signature=c(object="swSite",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			object@SWClimits[1] = readNumeric(infiletext[2])
			object@SWClimits[2] = readNumeric(infiletext[3])
			object@SWClimits[3] = readNumeric(infiletext[4])
			object@ModelFlags[1] = readLogical(infiletext[7])
			object@ModelFlags[2] = readLogical(infiletext[8])
			object@ModelCoefficients[1] = readNumeric(infiletext[10])
			object@ModelCoefficients[2] = readNumeric(infiletext[11])
			object@ModelCoefficients[3] = readNumeric(infiletext[12])
			object@SnowSimulationParameters[1] = readNumeric(infiletext[15])
			object@SnowSimulationParameters[2] = readNumeric(infiletext[16])
			object@SnowSimulationParameters[3] = readNumeric(infiletext[17])
			object@SnowSimulationParameters[4] = readNumeric(infiletext[18])
			object@SnowSimulationParameters[5] = readNumeric(infiletext[19])
			object@DrainageCoefficient = readNumeric(infiletext[22])
			object@EvaporationCoefficients[1] = readNumeric(infiletext[30])
			object@EvaporationCoefficients[2] = readNumeric(infiletext[33])
			object@EvaporationCoefficients[3] = readNumeric(infiletext[35])
			object@EvaporationCoefficients[4] = readNumeric(infiletext[36])
			object@TranspirationCoefficients[1] = readNumeric(infiletext[40])
			object@TranspirationCoefficients[2] = readNumeric(infiletext[41])
			object@TranspirationCoefficients[3] = readNumeric(infiletext[42])
			object@TranspirationCoefficients[4] = readNumeric(infiletext[43])
			object@IntrinsicSiteParams[1] = readNumeric(infiletext[46])
			object@IntrinsicSiteParams[2] = readNumeric(infiletext[47])
			object@IntrinsicSiteParams[3] = readNumeric(infiletext[48])
			object@IntrinsicSiteParams[4] = readNumeric(infiletext[49])
			object@SoilTemperatureConstants[1] = readNumeric(infiletext[53])
			object@SoilTemperatureConstants[2] = readNumeric(infiletext[54])
			object@SoilTemperatureConstants[3] = readNumeric(infiletext[55])
			object@SoilTemperatureConstants[4] = readNumeric(infiletext[56])
			object@SoilTemperatureConstants[5] = readNumeric(infiletext[57])
			object@SoilTemperatureConstants[6] = readNumeric(infiletext[58])
			object@SoilTemperatureConstants[7] = readNumeric(infiletext[59])
			object@SoilTemperatureConstants[8] = readNumeric(infiletext[60])
			object@SoilTemperatureConstants[9] = readNumeric(infiletext[61])
			object@SoilTemperatureConstants[10] = readNumeric(infiletext[62])
			object@SoilTemperatureFlag = readLogical(infiletext[63])

			for(i in 71:length(infiletext)) {
				if(grepl(pattern="#", infiletext[i]))
					infiletext=infiletext[-i]
			}

			data = matrix(NA,nrow=length(71:length(infiletext)),ncol=2)
			colnames(data)<-c("ndx","layer")
			for(i in 71:length(infiletext)) {
				if(!grepl(pattern="#", infiletext[i]))
					data[i-70,] = readNumerics(infiletext[i],2)
			}
			object@TranspirationRegions = data
			return(object)
})
