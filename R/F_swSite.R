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
# Author: Ryan J. Murphy (2013)
###############################################################################

print("swSite")

###############################################################SITE#####################################################################
swSite <- setClass("swSite",representation(SWClimits="numeric",ModelFlags="logical",ModelCoefficients="numeric",SnowSimulationParameters="numeric",DrainageCoefficient="numeric",EvaporationCoefficients="numeric",TranspirationCoefficients="numeric",IntrinsicSiteParams="numeric",SoilTemperatureFlag="logical",SoilTemperatureConstants="numeric",TranspirationRegions="matrix"),
		prototype=prototype(SWClimits=c(-1,15,15),ModelFlags=c(FALSE,TRUE),ModelCoefficients=c(1,0.0),SnowSimulationParameters=c(.61,1.54,.1,0,.27),DrainageCoefficient=0.02,
				EvaporationCoefficients=c(45,0.1,0.25,0.5),TranspirationCoefficients=c(45,0.1,0.50,1.10),IntrinsicSiteParams=c(0.681,1651,0,-1),SoilTemperatureFlag=TRUE,SoilTemperatureConstants=c(300,15,-4,600,0.00070,0.00030,0.18,6.69,15,990),
				TranspirationRegions=matrix(data=c(1,2,3,6,9,11),nrow=3,ncol=2,dimnames=list(NULL,c("ndx","layer")))) )

swSite_validity<-function(object){
	if(length(object@SWClimits) != 3)
		return("@SWClimits length != 3.")
	if(length(object@ModelFlags) != 2)
		return("@ModelFlags length != 2.")
	if(length(object@ModelCoefficients) != 2)
		return("@ModelCoefficients length != 2.")
	if(length(object@SnowSimulationParameters) != 5)
		return("@SnowSimulationParameters length != 5.")
	if(length(object@DrainageCoefficient) != 1)
		return("@DrainageCoefficient length != 1.")
	if(length(object@EvaporationCoefficients) != 4)
		return("@EvaporationCoefficients length != 4.")
	if(length(object@TranspirationCoefficients) != 4)
		return("@TranspirationCoefficients length != 4.")
	if(length(object@IntrinsicSiteParams) != 4)
		return("@IntrinsicSiteParams length != 4.")
	if(length(object@SoilTemperatureFlag) != 1)
		return("@SoilTemperatureFlag length != 1.")
	if(length(object@SoilTemperatureConstants) != 10)
		return("@SoilTemperatureConstants length != 10.")
	if(dim(object@TranspirationRegions)[2] != 2) 
		return("@TranspirationRegions columns != 2.")
	TRUE
}
setValidity("swSite",swSite_validity)
setMethod(f="initialize",signature="swSite",definition=function(.Object,SWClimits=c(-1,15,15),ModelFlags=c(FALSE,TRUE),ModelCoefficients=c(1,0.0),SnowSimulationParameters=c(.61,1.54,.1,0,.27),DrainageCoefficient=0.02,
				EvaporationCoefficients=c(45,0.1,0.25,0.5),TranspirationCoefficients=c(45,0.1,0.50,1.10),IntrinsicSiteParams=c(0.681,1651,0,-1),SoilTemperatureFlag=FALSE,SoilTemperatureConstants=c(300,15,-4,600,0.00070,0.00030,0.18,6.69,15,990),TranspirationRegions=NULL ){
			if(is.null(TranspirationRegions))
				TranspirationRegions<-matrix(data=c(1,2,3,6,9,11),nrow=3,ncol=2)
			colnames(TranspirationRegions)<-c("ndx","layer")
			names(SWClimits) <- c("swc_min", "swc_init", "swc_wet")
			names(ModelFlags) <- c("Reset", "DeepDrain")
			names(ModelCoefficients) <- c("PETmultiplier", "DailyRunoff")
			names(SnowSimulationParameters) <- c("TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax")
			names(DrainageCoefficient) <- c("SlowDrainCoefficientPerYear_cm/dy")
			names(EvaporationCoefficients) <- c("RateShift", "RateSlope", "InflectionPoint", "Range")
			names(TranspirationCoefficients) <- c("RateShift", "RateShape", "InflectionPoint", "Range")
			names(IntrinsicSiteParams) <- c("Latitude", "Altitude", "Slope", "Aspect")
			names(SoilTemperatureFlag) <- c("CalculateSoilTemp")
			names(SoilTemperatureConstants) <- c("BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b","T1constant_c", "cs_constant_SoilThermCondct", "cs_constant", "sh_constant_SpecificHeatCapacity", "ConstMeanAirTemp", "deltaX_Param", "MaxDepth")
			.Object@SWClimits<-SWClimits
			.Object@ModelFlags<-ModelFlags
			.Object@ModelCoefficients<-ModelCoefficients
			.Object@SnowSimulationParameters<-SnowSimulationParameters
			.Object@DrainageCoefficient<-DrainageCoefficient
			.Object@EvaporationCoefficients<-EvaporationCoefficients
			.Object@TranspirationCoefficients<-TranspirationCoefficients
			.Object@IntrinsicSiteParams<-IntrinsicSiteParams
			.Object@SoilTemperatureFlag<-SoilTemperatureFlag
			.Object@SoilTemperatureConstants<-SoilTemperatureConstants
			.Object@TranspirationRegions<-TranspirationRegions
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swSite",
		definition=function(object) {
			SWClimits=numeric(3)
			ModelFlags=logical(2)
			ModelCoefficients=numeric(2)
			SnowSimulationParameters=numeric(5)
			DrainageCoefficient=numeric(1)
			EvaporationCoefficients=numeric(4)
			TranspirationCoefficients=numeric(4)
			IntrinsicSiteParams=numeric(4)
			SoilTemperatureFlag=logical(1)
			SoilTemperatureConstants=numeric(10)
			TranspirationRegions=matrix(data=NA,nrow=3,ncol=2)
			colnames(TranspirationRegions)<-c("ndx","layer")
			names(SWClimits) <- c("swc_min", "swc_init", "swc_wet")
			names(ModelFlags) <- c("Reset", "DeepDrain")
			names(ModelCoefficients) <- c("PETmultiplier", "DailyRunoff")
			names(SnowSimulationParameters) <- c("TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax")
			names(DrainageCoefficient) <- c("SlowDrainCoefficientPerYear_cm/dy")
			names(EvaporationCoefficients) <- c("RateShift", "RateSlope", "InflectionPoint", "Range")
			names(TranspirationCoefficients) <- c("RateShift", "RateShape", "InflectionPoint", "Range")
			names(IntrinsicSiteParams) <- c("Latitude", "Altitude", "Slope", "Aspect")
			names(SoilTemperatureFlag) <- c("CalculateSoilTemp")
			names(SoilTemperatureConstants) <- c("BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b","T1constant_c", "cs_constant_SoilThermCondct", "cs_constant", "sh_constant_SpecificHeatCapacity", "ConstMeanAirTemp", "deltaX_Param", "MaxDepth")
			object@SWClimits<-SWClimits
			object@ModelFlags<-ModelFlags
			object@ModelCoefficients<-ModelCoefficients
			object@SnowSimulationParameters<-SnowSimulationParameters
			object@DrainageCoefficient<-DrainageCoefficient
			object@EvaporationCoefficients<-EvaporationCoefficients
			object@TranspirationCoefficients<-TranspirationCoefficients
			object@IntrinsicSiteParams<-IntrinsicSiteParams
			object@SoilTemperatureFlag<-SoilTemperatureFlag
			object@SoilTemperatureConstants<-SoilTemperatureConstants
			object@TranspirationRegions<-TranspirationRegions
			#Layers<-matrix(data=NA, nrow=8,ncol=12)
			#colnames(Layers)<-c("depth_cm","bulkDensity_g/cm^3","fieldCapacity_cm^3/cm^3","WiltingPoint_cm^3/cm^3","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			#object@Layers<-Layers
			return(object)
		})
setMethod("swSite_SWClimits", "swSite", function(object) {return(object@SWClimits)})
setMethod("swSite_ModelFlags", "swSite", function(object) {return(object@ModelFlags)})
setMethod("swSite_ModelCoefficients", "swSite", function(object) {return(object@ModelCoefficients)})
setMethod("swSite_SnowSimulationParams", "swSite", function(object) {return(object@SnowSimulationParameters)})
setMethod("swSite_DrainageCoefficient", "swSite", function(object) {return(object@DrainageCoefficient)})
setMethod("swSite_EvapCoefficients", "swSite", function(object) {return(object@EvaporationCoefficients)})
setMethod("swSite_TranspCoefficients", "swSite", function(object) {return(object@TranspirationCoefficients)})
setMethod("swSite_IntrinsicSiteParams", "swSite", function(object) {return(object@IntrinsicSiteParams)})
setMethod("swSite_SoilTemperatureFlag", "swSite", function(object) {return(object@SoilTemperatureFlag)})
setMethod("swSite_SoilTemperatureConsts", "swSite", function(object) {return(object@SoilTemperatureConstants)})
setMethod("swSite_TranspirationRegions", "swSite", function(object) {return(object@TranspirationRegions)})

setReplaceMethod(f="swSite_SWClimits", signature="swSite", definition=function(object,value) {object@SWClimits <- value; return(object)})
setReplaceMethod(f="swSite_ModelFlags", signature="swSite", definition=function(object,value) {object@ModelFlags <- value; return(object)})
setReplaceMethod(f="swSite_ModelCoefficients", signature="swSite", definition=function(object,value) {object@ModelCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_SnowSimulationParams", signature="swSite", definition=function(object,value) {object@SnowSimulationParameters <- value; return(object)})
setReplaceMethod(f="swSite_DrainageCoefficient", signature="swSite", definition=function(object,value) {object@DrainageCoefficient <- value; return(object)})
setReplaceMethod(f="swSite_EvapCoefficients", signature="swSite", definition=function(object,value) {object@EvaporationCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_TranspCoefficients", signature="swSite", definition=function(object,value) {object@TranspirationCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_IntrinsicSiteParams", signature="swSite", definition=function(object,value) {object@IntrinsicSiteParams <- value; return(object)})
setReplaceMethod(f="swSite_SoilTemperatureFlag", signature="swSite", definition=function(object,value) {object@SoilTemperatureFlag <- value; return(object)})
setReplaceMethod(f="swSite_SoilTemperatureConsts", signature="swSite", definition=function(object,value) {object@SoilTemperatureConstants <- value; return(object)})
setReplaceMethod(f="swSite_TranspirationRegions", signature="swSite", definition=function(object,value) {object@TranspirationRegions <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swSite", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(70+nrow(object@TranspirationRegions))
			
			infiletext[1] <- "# ---- SWC limits ----"
			infiletext[2] <- paste(format(object@SWClimits[1]),"\t# swc_min : cm/cm if 0 - <1.0, -bars if >= 1.0.; if < 0. then estimate residual water content for each layer",sep="")
			infiletext[3] <- paste(format(object@SWClimits[2]),"\t# swc_init: cm/cm if < 1.0, -bars if >= 1.0.",sep="")
			infiletext[4] <- paste(format(object@SWClimits[3]),"\t# swc_wet : cm/cm if < 1.0, -bars if >= 1.0.",sep="")
			
			infiletext[6] <- "# ---- Model flags and coefficients ----"
			infiletext[7] <- paste(format(as.integer(object@ModelFlags[1])),"\t# reset (1/0): reset/don't reset swc each new year",sep="")
			infiletext[8] <- paste(format(as.integer(object@ModelFlags[2])),"\t# deepdrain (1/0): allow/disallow deep drainage function.",sep="")
			infiletext[9] <- "\t\t#   if deepdrain == 1, model expects extra layer in soils file."
			infiletext[10] <- paste(format(object@ModelCoefficients[1]),"\t# multiplier for PET (eg for climate change).",sep="")
			infiletext[11] <- paste(format(object@ModelCoefficients[2]),"\t#proportion of ponded surface water removed as runoff daily (value ranges between 0 and 1; 0=no loss of surface water, 1=all ponded water lost via runoff)",sep="")
			
			infiletext[13] <- "# ---- Snow simulation parameters (SWAT2K model): Neitsch S, Arnold J, Kiniry J, Williams J. 2005. Soil and water assessment tool (SWAT) theoretical documentation. version 2005. Blackland Research Center, Texas Agricultural Experiment Station: Temple, TX."
			infiletext[14] <- "# these parameters are RMSE optimized values for 10 random SNOTEL sites for western US"
			infiletext[15] <- paste(format(object@SnowSimulationParameters[1]),"\t# TminAccu2 = Avg. air temp below which ppt is snow ( C)",sep="")
			infiletext[16] <- paste(format(object@SnowSimulationParameters[2]),"\t# TmaxCrit = Snow temperature at which snow melt starts ( C)",sep="")
			infiletext[17] <- paste(format(object@SnowSimulationParameters[3]),"\t# lambdasnow = Relative contribution of avg. air temperature to todays snow temperture vs. yesterday's snow temperature (0-1)",sep="")
			infiletext[18] <- paste(format(object@SnowSimulationParameters[4]),"\t# RmeltMin = Minimum snow melt rate on winter solstice (cm/day/C)",sep="")
			infiletext[19] <- paste(format(object@SnowSimulationParameters[5]),"\t# RmeltMax = Maximum snow melt rate on summer solstice (cm/day/C)",sep="")
			
			infiletext[21] <- "# ---- Drainage coefficient ----"
			infiletext[22] <- paste(format(object@DrainageCoefficient),"\t# slow-drain coefficient per layer (cm/day).  See Eqn 2.9 in ELM doc.",sep="")
			infiletext[23] <- "\t\t# ELM shows this as a value for each layer, but this way it's applied to all."
			infiletext[24] <- "\t\t# (Q=.02 in ELM doc, .06 in FORTRAN version)."
			
			infiletext[26] <- "# ---- Evaporation coefficients ----"
			infiletext[27] <- "# These control the tangent function (tanfunc) which affects the amount of soil"
			infiletext[28] <- "# water extractable by evaporation and transpiration."
			infiletext[29] <- "# These constants aren't documented by the ELM doc."
			infiletext[30] <- paste(format(object@EvaporationCoefficients[1]),"\t# rate shift (x value of inflection point).  lower value shifts curve",sep="")
			infiletext[31] <- "\t\t# leftward, meaning less water lost to evap at a given swp.  effectively"
			infiletext[32] <- "\t\t# shortens/extends high rate."
			infiletext[33] <- paste(format(object@EvaporationCoefficients[2]),"\t# rate slope: lower value (eg .01) straightens S shape meaning more gradual",sep="")
			infiletext[34] <- "\t\t# reduction effect; higher value (.5) makes abrupt transition"
			infiletext[35] <- paste(format(object@EvaporationCoefficients[3]),"\t# inflection point (y-value of inflection point)",sep="")
			infiletext[36] <- paste(format(object@EvaporationCoefficients[4]),"\t# range: diff btw upper and lower rates at the limits",sep="")
			
			infiletext[38] <- "# ---- Transpiration Coefficients ----"
			infiletext[39] <- "# comments from Evap constants apply."
			infiletext[40] <- paste(format(object@TranspirationCoefficients[1]),"\t# rate shift",sep="")
			infiletext[41] <- paste(format(object@TranspirationCoefficients[2]),"\t# rate shape",sep="")
			infiletext[42] <- paste(format(object@TranspirationCoefficients[3]),"\t# inflection point",sep="")
			infiletext[43] <- paste(format(object@TranspirationCoefficients[4]),"\t# range",sep="")
			
			infiletext[45] <- "# ---- Intrinsic site params ----"
			infiletext[46] <- paste(format(object@IntrinsicSiteParams[1]),"\t# latitude of the site in radians",sep="")
			infiletext[47] <- paste(format(object@IntrinsicSiteParams[2],nsmall=4),"\t# altitude of site (m a.s.l.)",sep="")
			infiletext[48] <- paste(format(object@IntrinsicSiteParams[3]),"\t# slope at site (degrees): no slope = 0",sep="")
			infiletext[49] <- paste(format(object@IntrinsicSiteParams[4]),"\t# aspect at site (degrees): N=0, E=90, S=180, W=270, no slope:-1",sep="")
			
			infiletext[51] <- "# ---- Soil Temperature Constants ----"
			infiletext[52] <- "# from Parton 1978, ch. 2.2.2 Temperature-profile Submodel"
			infiletext[53] <- paste(format(object@SoilTemperatureConstants[1]),"\t# biomass limiter, 300 g/m^2 in Parton's equation for T1(avg daily temperature at the top of the soil)",sep="")
			infiletext[54] <- paste(format(object@SoilTemperatureConstants[2]),"\t# constant for T1 equation (used if biomass <= biomass limiter), 15 in Parton's equation",sep="")
			infiletext[55] <- paste(format(object@SoilTemperatureConstants[3]),"\t# constant for T1 equation (used if biomass > biomass limiter), -4 in Parton's equation",sep="")
			infiletext[56] <- paste(format(object@SoilTemperatureConstants[4]),"\t# constant for T1 equation (used if biomass > biomass limiter), 600 in Parton's equation",sep="")
			infiletext[57] <- paste(format(object@SoilTemperatureConstants[5]),"\t# constant for cs (soil-thermal conductivity) equation, 0.00070 in Parton's equation",sep="")
			infiletext[58] <- paste(format(object@SoilTemperatureConstants[6]),"\t# constant for cs equation, 0.00030 in Parton's equation",sep="")
			infiletext[59] <- paste(format(object@SoilTemperatureConstants[7]),"\t# constant for s(h (specific heat capacity) equation, 0.18 in Parton's equation",sep="")
			infiletext[60] <- paste(format(object@SoilTemperatureConstants[8]),"\t# constant mean air temperature (the soil temperature at the lower boundary, 180 cm) in celsius",sep="")
			infiletext[61] <- paste(format(object@SoilTemperatureConstants[9]),"\t# deltaX parameter for soil_temperature function, default is 15.  (distance between profile points in cm)  max depth (the next number) should be evenly divisible by this number",sep="")
			infiletext[62] <- paste(format(object@SoilTemperatureConstants[10]),"\t# max depth for thereadNumeric(object@DrainageCoefficient) soil_temperature function equation, default is 180.  this number should be evenly divisible by deltaX",sep="")
			infiletext[63] <- paste(format(as.integer(object@SoilTemperatureFlag)),"\t# flag, 1 to calculate soil_temperature, 0 to not calculate soil_temperature",sep="")
			
			infiletext[66] <- "# ---- Transpiration regions ----"
			infiletext[67] <- "# ndx  : 1=shallow, 2=medium, 3=deep, 4=very deep"
			infiletext[68] <- "# layer: deepest layer number of the region."
			infiletext[69] <- "#        Layers are defined in soils.in."
			infiletext[70] <- "# ndx    layer"
			for(i in 1:nrow(object@TranspirationRegions)) {
				infiletext[i+70] <- paste(format(object@TranspirationRegions[i,1]),format(object@TranspirationRegions[i,2]),sep="\t")
			}
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
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
