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

##########################GENERIC FUNCTIONS############################################
#This generic will be used to set sw Objects to EMPTY. For example you have input data and want to set it to Nothing.

#' @export
setGeneric(
		name="swClear",
		def=function(object) {standardGeneric("swClear")}
)
#' @export
setGeneric(
		name="swWriteLines",
		def=function(object, file) {standardGeneric("swWriteLines")})
#' @export
setGeneric(
		name="swReadLines",
		def=function(object, file) {standardGeneric("swReadLines")})

#########FILES##########
#' @export
setGeneric(name="get_swFiles",function(object) {standardGeneric("get_swFiles")})
#' @export
setGeneric(name="swFiles_ProjDir",function(object) {standardGeneric("swFiles_ProjDir")})
#' @export
setGeneric(name="swFiles_filesIn",function(object) {standardGeneric("swFiles_filesIn")})
#' @export
setGeneric(name="swFiles_Years",function(object) {standardGeneric("swFiles_Years")})
#' @export
setGeneric(name="swFiles_LogFile",function(object) {standardGeneric("swFiles_LogFile")})
#' @export
setGeneric(name="swFiles_SiteParams",function(object) {standardGeneric("swFiles_SiteParams")})
#' @export
setGeneric(name="swFiles_Soils",function(object) {standardGeneric("swFiles_Soils")})
#' @export
setGeneric(name="swFiles_WeatherSetup",function(object) {standardGeneric("swFiles_WeatherSetup")})
#' @export
setGeneric(name="swFiles_MarkovProbs",function(object) {standardGeneric("swFiles_MarkovProbs")})
#' @export
setGeneric(name="swFiles_MarkovCov",function(object) {standardGeneric("swFiles_MarkovCov")})
#' @export
setGeneric(name="swFiles_Cloud",function(object) {standardGeneric("swFiles_Cloud")})
#' @export
setGeneric(name="swFiles_Prod",function(object) {standardGeneric("swFiles_Prod")})
#' @export
setGeneric(name="swFiles_Estab",function(object) {standardGeneric("swFiles_Estab")})
#' @export
setGeneric(name="swFiles_SWCsetup",function(object) {standardGeneric("swFiles_SWCsetup")})
#' @export
setGeneric(name="swFiles_Output",function(object) {standardGeneric("swFiles_Output")})
#' @export
setGeneric(name="swFiles_WeatherPrefix",function(object) {standardGeneric("swFiles_WeatherPrefix")})
#' @export
setGeneric(name="swFiles_OutputPrefix",function(object) {standardGeneric("swFiles_OutputPrefix")})

#' @export
setGeneric("set_swFiles", function(object) standardGeneric("set_swFiles"))
#' @export
setGeneric("set_swFiles<-", function(object, value) standardGeneric("set_swFiles<-"))
#' @export
setGeneric(name="swFiles_ProjDir<-",function(object, value) {standardGeneric("swFiles_ProjDir<-")})
#' @export
setGeneric(name="swFiles_filesIn<-",function(object, value) {standardGeneric("swFiles_filesIn<-")})
#' @export
setGeneric(name="swFiles_Years<-",function(object, value) {standardGeneric("swFiles_Years<-")})
#' @export
setGeneric(name="swFiles_LogFile<-",function(object, value) {standardGeneric("swFiles_LogFile<-")})
#' @export
setGeneric(name="swFiles_SiteParams<-",function(object, value) {standardGeneric("swFiles_SiteParams<-")})
#' @export
setGeneric(name="swFiles_Soils<-",function(object, value) {standardGeneric("swFiles_Soils<-")})
#' @export
setGeneric(name="swFiles_WeatherSetup<-",function(object, value) {standardGeneric("swFiles_WeatherSetup<-")})
#' @export
setGeneric(name="swFiles_MarkovProbs<-",function(object, value) {standardGeneric("swFiles_MarkovProbs<-")})
#' @export
setGeneric(name="swFiles_MarkovCov<-",function(object, value) {standardGeneric("swFiles_MarkovCov<-")})
#' @export
setGeneric(name="swFiles_Cloud<-",function(object, value) {standardGeneric("swFiles_Cloud<-")})
#' @export
setGeneric(name="swFiles_Prod<-",function(object, value) {standardGeneric("swFiles_Prod<-")})
#' @export
setGeneric(name="swFiles_Estab<-",function(object, value) {standardGeneric("swFiles_Estab<-")})
#' @export
setGeneric(name="swFiles_SWCsetup<-",function(object, value) {standardGeneric("swFiles_SWCsetup<-")})
#' @export
setGeneric(name="swFiles_Output<-",function(object, value) {standardGeneric("swFiles_Output<-")})
#' @export
setGeneric(name="swFiles_WeatherPrefix<-",function(object, value) {standardGeneric("swFiles_WeatherPrefix<-")})
#' @export
setGeneric(name="swFiles_OutputPrefix<-",function(object, value) {standardGeneric("swFiles_OutputPrefix<-")})
########################

########YEARS###########
#' @export
setGeneric(name="get_swYears",function(object) {standardGeneric("get_swYears")})
#' @export
setGeneric(name="swYears_StartYear",function(object) {standardGeneric("swYears_StartYear")})
#' @export
setGeneric(name="swYears_EndYear",function(object) {standardGeneric("swYears_EndYear")})
#' @export
setGeneric(name="swYears_FDOFY",function(object) {standardGeneric("swYears_FDOFY")})
#' @export
setGeneric(name="swYears_EDOEY",function(object) {standardGeneric("swYears_EDOEY")})
#' @export
setGeneric(name="swYears_isNorth",function(object) {standardGeneric("swYears_isNorth")})

#' @export
setGeneric(name="set_swYears<-",function(object, value) {standardGeneric("set_swYears<-")})
#' @export
setGeneric(name="swYears_StartYear<-",function(object, value) {standardGeneric("swYears_StartYear<-")})
#' @export
setGeneric(name="swYears_EndYear<-",function(object, value) {standardGeneric("swYears_EndYear<-")})
#' @export
setGeneric(name="swYears_FDOFY<-",function(object, value) {standardGeneric("swYears_FDOFY<-")})
#' @export
setGeneric(name="swYears_EDOEY<-",function(object, value) {standardGeneric("swYears_EDOEY<-")})
#' @export
setGeneric(name="swYears_isNorth<-",function(object, value) {standardGeneric("swYears_isNorth<-")})
########################

########WEATHER#########
#' @export
setGeneric(name="get_swWeather",function(object) {standardGeneric("get_swWeather")})
#' @export
setGeneric(name="swWeather_DaysRunningAverage",function(object) {standardGeneric("swWeather_DaysRunningAverage")})
#' @export
setGeneric(name="swWeather_FirstYearHistorical",function(object) {standardGeneric("swWeather_FirstYearHistorical")})
#' @export
setGeneric(name="swWeather_pct_SnowDrift",function(object) {standardGeneric("swWeather_pct_SnowDrift")})
#' @export
setGeneric(name="swWeather_pct_SnowRunoff",function(object) {standardGeneric("swWeather_pct_SnowRunoff")})
#' @export
setGeneric(name="swWeather_UseMarkov",function(object) {standardGeneric("swWeather_UseMarkov")})
#' @export
setGeneric(name="swWeather_UseSnow",function(object) {standardGeneric("swWeather_UseSnow")})
#' @export
setGeneric(name="swWeather_MonScalingParams",function(object) {standardGeneric("swWeather_MonScalingParams")})

#' @export
setGeneric("set_swWeather", signature = "object", function(object)
  standardGeneric("set_swWeather"))
#' @export
setGeneric("set_swWeather<-", signature = "object", function(object, value)
  standardGeneric("set_swWeather<-"))
#' @export
setGeneric(name="swWeather_DaysRunningAverage<-",function(object, value) {standardGeneric("swWeather_DaysRunningAverage<-")})
#' @export
setGeneric(name="swWeather_FirstYearHistorical<-",function(object, value) {standardGeneric("swWeather_FirstYearHistorical<-")})
#' @export
setGeneric(name="swWeather_pct_SnowDrift<-",function(object, value) {standardGeneric("swWeather_pct_SnowDrift<-")})
#' @export
setGeneric(name="swWeather_pct_SnowRunoff<-",function(object, value) {standardGeneric("swWeather_pct_SnowRunoff<-")})
#' @export
setGeneric(name="swWeather_UseMarkov<-",function(object, value) {standardGeneric("swWeather_UseMarkov<-")})
#' @export
setGeneric(name="swWeather_UseSnow<-",function(object, value) {standardGeneric("swWeather_UseSnow<-")})
#' @export
setGeneric(name="swWeather_MonScalingParams<-",function(object, value) {standardGeneric("swWeather_MonScalingParams<-")})
########################

########MARKOV##########
#' @export
setGeneric(name="get_Markov",function(object) {standardGeneric("get_Markov")})
#' @export
setGeneric(name="swMarkov_Prob",function(object) {standardGeneric("swMarkov_Prob")})
#' @export
setGeneric(name="swMarkov_Conv",function(object) {standardGeneric("swMarkov_Conv")})

#' @export
setGeneric(name="set_Markov<-",function(object, value) {standardGeneric("set_Markov<-")})
#' @export
setGeneric(name="swMarkov_Prob<-",function(object, value) {standardGeneric("swMarkov_Prob<-")})
#' @export
setGeneric(name="swMarkov_Conv<-",function(object, value) {standardGeneric("swMarkov_Conv<-")})
########################


#####WeatherData########
#' @export
setGeneric(name="get_WeatherHistory",function(object) {standardGeneric("get_WeatherHistory")})
#' @export
setGeneric(name="get_swWeatherData",function(object,year) {standardGeneric("get_swWeatherData")})

#' @export
setGeneric(name="set_WeatherHistory<-",function(object,value) {standardGeneric("set_WeatherHistory<-")})
#' @export
setGeneric(name="set_swWeatherData<-",function(object,value) {standardGeneric("set_swWeatherData<-")})
########################

#######CLOUD############
#' @export
setGeneric(name="get_swCloud",function(object) {standardGeneric("get_swCloud")})
#' @export
setGeneric(name="swCloud_SkyCover",function(object) {standardGeneric("swCloud_SkyCover")})
#' @export
setGeneric(name="swCloud_WindSpeed",function(object) {standardGeneric("swCloud_WindSpeed")})
#' @export
setGeneric(name="swCloud_Humidity", function(object) {standardGeneric("swCloud_Humidity")})
#' @export
setGeneric(name="swCloud_Transmissivity", function(object) {standardGeneric("swCloud_Transmissivity")})
#' @export
setGeneric(name="swCloud_SnowDensity", function(object) {standardGeneric("swCloud_SnowDensity")})

#' @export
setGeneric("set_swCloud", function(object) standardGeneric("set_swCloud"))
#' @export
setGeneric("set_swCloud<-", function(object, value) standardGeneric("set_swCloud<-"))
#' @export
setGeneric(name="swCloud_SkyCover<-",function(object,value) {standardGeneric("swCloud_SkyCover<-")})
#' @export
setGeneric(name="swCloud_WindSpeed<-",function(object,value) {standardGeneric("swCloud_WindSpeed<-")})
#' @export
setGeneric(name="swCloud_Humidity<-", function(object,value) {standardGeneric("swCloud_Humidity<-")})
#' @export
setGeneric(name="swCloud_Transmissivity<-", function(object,value) {standardGeneric("swCloud_Transmissivity<-")})
#' @export
setGeneric(name="swCloud_SnowDensity<-", function(object,value) {standardGeneric("swCloud_SnowDensity<-")})
########################

########PROD############
#' @export
setGeneric(name="get_swProd",function(object) {standardGeneric("get_swProd")})
#' @export
setGeneric(name="swProd_Composition",function(object) {standardGeneric("swProd_Composition")})
#' @export
setGeneric(name="swProd_Albedo",function(object) {standardGeneric("swProd_Albedo")})
#' @export
setGeneric(name="swProd_Cover_stcr",function(object) {standardGeneric("swProd_Cover_stcr")})
#' @export
setGeneric(name="swProd_CanopyHeight",function(object) {standardGeneric("swProd_CanopyHeight")})
#' @export
setGeneric(name="swProd_VegInterParam",function(object) {standardGeneric("swProd_VegInterParam")})
#' @export
setGeneric(name="swProd_LitterInterParam",function(object) {standardGeneric("swProd_LitterInterParam")})
#' @export
setGeneric(name="swProd_EsTpartitioning_param",function(object) {standardGeneric("swProd_EsTpartitioning_param")})
#' @export
setGeneric(name="swProd_Es_param_limit",function(object) {standardGeneric("swProd_Es_param_limit")})
#' @export
setGeneric(name="swProd_Shade",function(object) {standardGeneric("swProd_Shade")})
#' @export
setGeneric(name="swProd_HydrRedstro_use",function(object) {standardGeneric("swProd_HydrRedstro_use")})
#' @export
setGeneric(name="swProd_HydrRedstro",function(object) {standardGeneric("swProd_HydrRedstro")})
#' @export
setGeneric(name="swProd_CritSoilWaterPotential",function(object) {standardGeneric("swProd_CritSoilWaterPotential")})
#' @export
setGeneric(name="swProd_MonProd_grass",function(object) {standardGeneric("swProd_MonProd_grass")})
#' @export
setGeneric(name="swProd_MonProd_shrub",function(object) {standardGeneric("swProd_MonProd_shrub")})
#' @export
setGeneric(name="swProd_MonProd_tree",function(object) {standardGeneric("swProd_MonProd_tree")})
#' @export
setGeneric(name="swProd_MonProd_forb",function(object) {standardGeneric("swProd_MonProd_forb")})

#' @export
setGeneric("set_swProd", function(object) standardGeneric("set_swProd"))
#' @export
setGeneric("set_swProd<-", function(object, value) standardGeneric("set_swProd<-"))
#' @export
setGeneric(name="swProd_Composition<-",function(object, value) {standardGeneric("swProd_Composition<-")})
#' @export
setGeneric(name="swProd_Albedo<-",function(object, value) {standardGeneric("swProd_Albedo<-")})
#' @export
setGeneric(name="swProd_Cover_stcr<-",function(object, value) {standardGeneric("swProd_Cover_stcr<-")})
#' @export
setGeneric(name="swProd_CanopyHeight<-",function(object, value) {standardGeneric("swProd_CanopyHeight<-")})
#' @export
setGeneric(name="swProd_VegInterParam<-",function(object, value) {standardGeneric("swProd_VegInterParam<-")})
#' @export
setGeneric(name="swProd_LitterInterParam<-",function(object, value) {standardGeneric("swProd_LitterInterParam<-")})
#' @export
setGeneric(name="swProd_EsTpartitioning_param<-",function(object, value) {standardGeneric("swProd_EsTpartitioning_param<-")})
#' @export
setGeneric(name="swProd_Es_param_limit<-",function(object, value) {standardGeneric("swProd_Es_param_limit<-")})
#' @export
setGeneric(name="swProd_Shade<-",function(object, value) {standardGeneric("swProd_Shade<-")})
#' @export
setGeneric(name="swProd_HydrRedstro_use<-",function(object, value) {standardGeneric("swProd_HydrRedstro_use<-")})
#' @export
setGeneric(name="swProd_HydrRedstro<-",function(object, value) {standardGeneric("swProd_HydrRedstro<-")})
#' @export
setGeneric(name="swProd_CritSoilWaterPotential<-",function(object, value) {standardGeneric("swProd_CritSoilWaterPotential<-")})
#' @export
setGeneric(name="swProd_MonProd_grass<-",function(object, value) {standardGeneric("swProd_MonProd_grass<-")})
#' @export
setGeneric(name="swProd_MonProd_shrub<-",function(object, value) {standardGeneric("swProd_MonProd_shrub<-")})
#' @export
setGeneric(name="swProd_MonProd_tree<-",function(object, value) {standardGeneric("swProd_MonProd_tree<-")})
#' @export
setGeneric(name="swProd_MonProd_forb<-",function(object, value) {standardGeneric("swProd_MonProd_forb<-")})
########################

#######SITE#############
#' @export
setGeneric(name="get_swSite",function(object) {standardGeneric("get_swSite")})
#' @export
setGeneric(name="swSite_SWClimits",function(object) {standardGeneric("swSite_SWClimits")})
#' @export
setGeneric(name="swSite_ModelFlags",function(object) {standardGeneric("swSite_ModelFlags")})
#' @export
setGeneric(name="swSite_ModelCoefficients",function(object) {standardGeneric("swSite_ModelCoefficients")})
#' @export
setGeneric(name="swSite_SnowSimulationParams",function(object) {standardGeneric("swSite_SnowSimulationParams")})
#' @export
setGeneric(name="swSite_DrainageCoefficient",function(object) {swSite_DrainageCoefficient("swSite_DrainageCoefficient")})
#' @export
setGeneric(name="swSite_EvapCoefficients",function(object) {standardGeneric("swSite_EvapCoefficients")})
#' @export
setGeneric(name="swSite_TranspCoefficients",function(object) {standardGeneric("swSite_TranspCoefficients")})
#' @export
setGeneric(name="swSite_IntrinsicSiteParams",function(object) {standardGeneric("swSite_IntrinsicSiteParams")})
#' @export
setGeneric(name="swSite_SoilTemperatureFlag",function(object) {standardGeneric("swSite_SoilTemperatureFlag")})
#' @export
setGeneric(name="swSite_SoilTemperatureConsts",function(object) {standardGeneric("swSite_SoilTemperatureConsts")})
#' @export
setGeneric(name="swSite_TranspirationRegions",function(object) {standardGeneric("swSite_TranspirationRegions")})

#' @export
setGeneric("set_swSite", function(object) standardGeneric("set_swSite"))
#' @export
setGeneric("set_swSite<-", function(object, value) standardGeneric("set_swSite<-"))
#' @export
setGeneric(name="swSite_SWClimits<-",function(object, value) {standardGeneric("swSite_SWClimits<-")})
#' @export
setGeneric(name="swSite_ModelFlags<-",function(object, value) {standardGeneric("swSite_ModelFlags<-")})
#' @export
setGeneric(name="swSite_ModelCoefficients<-",function(object, value) {standardGeneric("swSite_ModelCoefficients<-")})
#' @export
setGeneric(name="swSite_SnowSimulationParams<-",function(object, value) {standardGeneric("swSite_SnowSimulationParams<-")})
#' @export
setGeneric(name="swSite_DrainageCoefficient<-",function(object, value) {standardGeneric("swSite_DrainageCoefficient<-")})
#' @export
setGeneric(name="swSite_EvapCoefficients<-",function(object, value) {standardGeneric("swSite_EvapCoefficients<-")})
#' @export
setGeneric(name="swSite_TranspCoefficients<-",function(object, value) {standardGeneric("swSite_TranspCoefficients<-")})
#' @export
setGeneric(name="swSite_IntrinsicSiteParams<-",function(object, value) {standardGeneric("swSite_IntrinsicSiteParams<-")})
#' @export
setGeneric(name="swSite_SoilTemperatureFlag<-",function(object, value) {standardGeneric("swSite_SoilTemperatureFlag<-")})
#' @export
setGeneric(name="swSite_SoilTemperatureConsts<-",function(object, value) {standardGeneric("swSite_SoilTemperatureConsts<-")})
#' @export
setGeneric(name="swSite_TranspirationRegions<-",function(object, value) {standardGeneric("swSite_TranspirationRegions<-")})
########################

#########SOILS##########
#' @export
setGeneric(name="get_swSoils",function(object) {standardGeneric("get_swSoils")})
#' @export
setGeneric(name="swSoils_Layers",function(object) {standardGeneric("swSoils_Layers")})

#' @export
setGeneric("set_swSoils", function(object) standardGeneric("set_swSoils"))
#' @export
setGeneric("set_swSoils<-",function(object,value) standardGeneric("set_swSoils<-"))
#' @export
setGeneric(name="swSoils_Layers<-",function(object,value) {standardGeneric("swSoils_Layers<-")})
########################

#########ESTAB##########
#' @export
setGeneric(name="get_swEstab",function(object) {standardGeneric("get_swEstab")})
#' @export
setGeneric(name="swEstab_useEstab",function(object) {standardGeneric("swEstab_useEstab")})
#species here#

#' @export
setGeneric(name="set_swEstab<-",function(object, value) {standardGeneric("set_swEstab<-")})
#' @export
setGeneric(name="swEstab_useEstab<-",function(object, value) {standardGeneric("swEstab_useEstab<-")})
#species here#
########################

#########SWC############
#' @export
setGeneric(name="get_swSWC",function(object) {standardGeneric("get_swSWC")})
#' @export
setGeneric(name="swSWC_use",function(object) {standardGeneric("swSWC_use")})
#' @export
setGeneric(name="swSWC_prefix",function(object) {standardGeneric("swSWC_prefix")})
#' @export
setGeneric(name="swSWC_FirstYear",function(object) {standardGeneric("swSWC_FirstYear")})
#' @export
setGeneric(name="swSWC_Method",function(object) {standardGeneric("swSWC_Method")})
#' @export
setGeneric(name="swSWC_HistoricList",function(object) {standardGeneric("swSWC_HistoricList")})
#' @export
setGeneric(name="swSWC_HistoricData",function(object, year) {standardGeneric("swSWC_HistoricData")})

#' @export
setGeneric(name="set_swSWC<-",function(object, value) {standardGeneric("set_swSWC<-")})
#' @export
setGeneric(name="swSWC_use<-",function(object, value) {standardGeneric("swSWC_use<-")})
#' @export
setGeneric(name="swSWC_prefix<-",function(object, value) {standardGeneric("swSWC_prefix<-")})
#' @export
setGeneric(name="swSWC_FirstYear<-",function(object, value) {standardGeneric("swSWC_FirstYear<-")})
#' @export
setGeneric(name="swSWC_Method<-",function(object, value) {standardGeneric("swSWC_Method<-")})
#' @export
setGeneric(name="swSWC_HistoricList<-",function(object, value) {standardGeneric("swSWC_HistoricList<-")})
#' @export
setGeneric(name="swSWC_HistoricData<-",function(object, value) {standardGeneric("swSWC_HistoricData<-")})
########################

#######OUT###########
#' @export
setGeneric(name="get_swOUT", function(object) {standardGeneric("get_swOUT")})
#' @export
setGeneric(name="swOUT_TimeStep", function(object) {standardGeneric("swOUT_TimeStep")})
#' @export
setGeneric(name="swOUT_OutputSeparator", function(object) {standardGeneric("swOUT_OutputSeparator")})
#' @export
setGeneric(name="swOUT_useTimeStep", function(object) {standardGeneric("swOUT_useTimeStep")})

#' @export
setGeneric(name="set_swOUT<-",function(object,value) {standardGeneric("set_swOUT<-")})
#' @export
setGeneric(name="swOUT_TimeStep<-",function(object,value) {standardGeneric("swOUT_TimeStep<-")})
#' @export
setGeneric(name="swOUT_OutputSeparator<-",function(object,value) {standardGeneric("swOUT_OutputSeparator<-")})
#' @export
setGeneric(name="swOUT_useTimeStep<-",function(object,value) {standardGeneric("swOUT_useTimeStep<-")})
########################

########LOG#############
#' @export
setGeneric(name="swLog_setLine<-",function(object, value) {standardGeneric("swLog_setLine<-")})
########################

########swOutput########
#' @export
setGeneric(name="swOutput_getKEY",function(object,index) {standardGeneric("swOutput_getKEY")})
#' @export
setGeneric(name="swOutput_KEY_Period",function(object,index) {standardGeneric("swOutput_KEY_Period")})
#' @export
setGeneric(name="swOutput_KEY_TimeStep",function(object) {standardGeneric("swOutput_KEY_TimeStep")})
#' @export
setGeneric(name="swOutput_KEY_Columns",function(object) {standardGeneric("swOutput_KEY_Columns")})

#' @export
setGeneric(name="swOutput_getKEY<-",function(object,index,value) {standardGeneric("swOutput_getKEY<-")})
#' @export
setGeneric(name="swOutput_KEY_Period<-",function(object,index,value) {standardGeneric("swOutput_KEY_Period<-")})
#' @export
setGeneric(name="swOutput_KEY_TimeStep<-",function(object,value) {standardGeneric("swOutput_KEY_TimeStep<-")})
#' @export
setGeneric(name="swOutput_KEY_Columns<-",function(object,value) {standardGeneric("swOutput_KEY_Columns<-")})
