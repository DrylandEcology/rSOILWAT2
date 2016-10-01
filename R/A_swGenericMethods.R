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
print("generics")
setGeneric(
		name="swClear",
		def=function(object) {standardGeneric("swClear")}
)
setGeneric(
		name="swWriteLines",
		def=function(object, file) {standardGeneric("swWriteLines")})
setGeneric(
		name="swReadLines",
		def=function(object, file) {standardGeneric("swReadLines")})

#########FILES##########
setGeneric(name="get_swFiles",function(object) {standardGeneric("get_swFiles")})
setGeneric(name="swFiles_ProjDir",function(object) {standardGeneric("swFiles_ProjDir")})
setGeneric(name="swFiles_filesIn",function(object) {standardGeneric("swFiles_filesIn")})
setGeneric(name="swFiles_Years",function(object) {standardGeneric("swFiles_Years")})
setGeneric(name="swFiles_LogFile",function(object) {standardGeneric("swFiles_LogFile")})
setGeneric(name="swFiles_SiteParams",function(object) {standardGeneric("swFiles_SiteParams")})
setGeneric(name="swFiles_Soils",function(object) {standardGeneric("swFiles_Soils")})
setGeneric(name="swFiles_WeatherSetup",function(object) {standardGeneric("swFiles_WeatherSetup")})
setGeneric(name="swFiles_MarkovProbs",function(object) {standardGeneric("swFiles_MarkovProbs")})
setGeneric(name="swFiles_MarkovCov",function(object) {standardGeneric("swFiles_MarkovCov")})
setGeneric(name="swFiles_Cloud",function(object) {standardGeneric("swFiles_Cloud")})
setGeneric(name="swFiles_Prod",function(object) {standardGeneric("swFiles_Prod")})
setGeneric(name="swFiles_Estab",function(object) {standardGeneric("swFiles_Estab")})
setGeneric(name="swFiles_SWCsetup",function(object) {standardGeneric("swFiles_SWCsetup")})
setGeneric(name="swFiles_Output",function(object) {standardGeneric("swFiles_Output")})
setGeneric(name="swFiles_WeatherPrefix",function(object) {standardGeneric("swFiles_WeatherPrefix")})
setGeneric(name="swFiles_OutputPrefix",function(object) {standardGeneric("swFiles_OutputPrefix")})

setGeneric(name="set_swFiles<-",function(object, value) {standardGeneric("set_swFiles<-")})
setGeneric(name="swFiles_ProjDir<-",function(object, value) {standardGeneric("swFiles_ProjDir<-")})
setGeneric(name="swFiles_filesIn<-",function(object, value) {standardGeneric("swFiles_filesIn<-")})
setGeneric(name="swFiles_Years<-",function(object, value) {standardGeneric("swFiles_Years<-")})
setGeneric(name="swFiles_LogFile<-",function(object, value) {standardGeneric("swFiles_LogFile<-")})
setGeneric(name="swFiles_SiteParams<-",function(object, value) {standardGeneric("swFiles_SiteParams<-")})
setGeneric(name="swFiles_Soils<-",function(object, value) {standardGeneric("swFiles_Soils<-")})
setGeneric(name="swFiles_WeatherSetup<-",function(object, value) {standardGeneric("swFiles_WeatherSetup<-")})
setGeneric(name="swFiles_MarkovProbs<-",function(object, value) {standardGeneric("swFiles_MarkovProbs<-")})
setGeneric(name="swFiles_MarkovCov<-",function(object, value) {standardGeneric("swFiles_MarkovCov<-")})
setGeneric(name="swFiles_Cloud<-",function(object, value) {standardGeneric("swFiles_Cloud<-")})
setGeneric(name="swFiles_Prod<-",function(object, value) {standardGeneric("swFiles_Prod<-")})
setGeneric(name="swFiles_Estab<-",function(object, value) {standardGeneric("swFiles_Estab<-")})
setGeneric(name="swFiles_SWCsetup<-",function(object, value) {standardGeneric("swFiles_SWCsetup<-")})
setGeneric(name="swFiles_Output<-",function(object, value) {standardGeneric("swFiles_Output<-")})
setGeneric(name="swFiles_WeatherPrefix<-",function(object, value) {standardGeneric("swFiles_WeatherPrefix<-")})
setGeneric(name="swFiles_OutputPrefix<-",function(object, value) {standardGeneric("swFiles_OutputPrefix<-")})
########################

########YEARS###########
setGeneric(name="get_swYears",function(object) {standardGeneric("get_swYears")})
setGeneric(name="swYears_StartYear",function(object) {standardGeneric("swYears_StartYear")})
setGeneric(name="swYears_EndYear",function(object) {standardGeneric("swYears_EndYear")})
setGeneric(name="swYears_FDOFY",function(object) {standardGeneric("swYears_FDOFY")})
setGeneric(name="swYears_EDOEY",function(object) {standardGeneric("swYears_EDOEY")})
setGeneric(name="swYears_isNorth",function(object) {standardGeneric("swYears_isNorth")})

setGeneric(name="set_swYears<-",function(object, value) {standardGeneric("set_swYears<-")})
setGeneric(name="swYears_StartYear<-",function(object, value) {standardGeneric("swYears_StartYear<-")})
setGeneric(name="swYears_EndYear<-",function(object, value) {standardGeneric("swYears_EndYear<-")})
setGeneric(name="swYears_FDOFY<-",function(object, value) {standardGeneric("swYears_FDOFY<-")})
setGeneric(name="swYears_EDOEY<-",function(object, value) {standardGeneric("swYears_EDOEY<-")})
setGeneric(name="swYears_isNorth<-",function(object, value) {standardGeneric("swYears_isNorth<-")})
########################

########WEATHER#########
setGeneric(name="get_swWeather",function(object) {standardGeneric("get_swWeather")})
setGeneric(name="swWeather_DaysRunningAverage",function(object) {standardGeneric("swWeather_DaysRunningAverage")})
setGeneric(name="swWeather_FirstYearHistorical",function(object) {standardGeneric("swWeather_FirstYearHistorical")})
setGeneric(name="swWeather_pct_SnowDrift",function(object) {standardGeneric("swWeather_pct_SnowDrift")})
setGeneric(name="swWeather_pct_SnowRunoff",function(object) {standardGeneric("swWeather_pct_SnowRunoff")})
setGeneric(name="swWeather_UseMarkov",function(object) {standardGeneric("swWeather_UseMarkov")})
setGeneric(name="swWeather_UseSnow",function(object) {standardGeneric("swWeather_UseSnow")})
setGeneric(name="swWeather_MonScalingParams",function(object) {standardGeneric("swWeather_MonScalingParams")})

setGeneric(name="set_swWeather<-",function(object, value) {standardGeneric("set_swWeather<-")})
setGeneric(name="swWeather_DaysRunningAverage<-",function(object, value) {standardGeneric("swWeather_DaysRunningAverage<-")})
setGeneric(name="swWeather_FirstYearHistorical<-",function(object, value) {standardGeneric("swWeather_FirstYearHistorical<-")})
setGeneric(name="swWeather_pct_SnowDrift<-",function(object, value) {standardGeneric("swWeather_pct_SnowDrift<-")})
setGeneric(name="swWeather_pct_SnowRunoff<-",function(object, value) {standardGeneric("swWeather_pct_SnowRunoff<-")})
setGeneric(name="swWeather_UseMarkov<-",function(object, value) {standardGeneric("swWeather_UseMarkov<-")})
setGeneric(name="swWeather_UseSnow<-",function(object, value) {standardGeneric("swWeather_UseSnow<-")})
setGeneric(name="swWeather_MonScalingParams<-",function(object, value) {standardGeneric("swWeather_MonScalingParams<-")})
########################

########MARKOV##########
setGeneric(name="get_Markov",function(object) {standardGeneric("get_Markov")})
setGeneric(name="swMarkov_Prob",function(object) {standardGeneric("swMarkov_Prob")})
setGeneric(name="swMarkov_Conv",function(object) {standardGeneric("swMarkov_Conv")})

setGeneric(name="set_Markov<-",function(object, value) {standardGeneric("set_Markov<-")})
setGeneric(name="swMarkov_Prob<-",function(object, value) {standardGeneric("swMarkov_Prob<-")})
setGeneric(name="swMarkov_Conv<-",function(object, value) {standardGeneric("swMarkov_Conv<-")})
########################


#####WeatherData########
setGeneric(name="get_WeatherHistory",function(object) {standardGeneric("get_WeatherHistory")})
setGeneric(name="get_swWeatherData",function(object,year) {standardGeneric("get_swWeatherData")})

setGeneric(name="set_WeatherHistory<-",function(object,value) {standardGeneric("set_WeatherHistory<-")})
setGeneric(name="set_swWeatherData<-",function(object,value) {standardGeneric("set_swWeatherData<-")})
########################

#######CLOUD############
setGeneric(name="get_swCloud",function(object) {standardGeneric("get_swCloud")})
setGeneric(name="swCloud_SkyCover",function(object) {standardGeneric("swCloud_SkyCover")})
setGeneric(name="swCloud_WindSpeed",function(object) {standardGeneric("swCloud_WindSpeed")})
setGeneric(name="swCloud_Humidity", function(object) {standardGeneric("swCloud_Humidity")})
setGeneric(name="swCloud_Transmissivity", function(object) {standardGeneric("swCloud_Transmissivity")})
setGeneric(name="swCloud_SnowDensity", function(object) {standardGeneric("swCloud_SnowDensity")})

setGeneric(name="set_swCloud<-",function(object,value) {standardGeneric("set_swCloud<-")})
setGeneric(name="swCloud_SkyCover<-",function(object,value) {standardGeneric("swCloud_SkyCover<-")})
setGeneric(name="swCloud_WindSpeed<-",function(object,value) {standardGeneric("swCloud_WindSpeed<-")})
setGeneric(name="swCloud_Humidity<-", function(object,value) {standardGeneric("swCloud_Humidity<-")})
setGeneric(name="swCloud_Transmissivity<-", function(object,value) {standardGeneric("swCloud_Transmissivity<-")})
setGeneric(name="swCloud_SnowDensity<-", function(object,value) {standardGeneric("swCloud_SnowDensity<-")})
########################

########PROD############
setGeneric(name="get_swProd",function(object) {standardGeneric("get_swProd")})
setGeneric(name="swProd_Composition",function(object) {standardGeneric("swProd_Composition")})
setGeneric(name="swProd_Albedo",function(object) {standardGeneric("swProd_Albedo")})
setGeneric(name="swProd_Cover_stcr",function(object) {standardGeneric("swProd_Cover_stcr")})
setGeneric(name="swProd_CanopyHeight",function(object) {standardGeneric("swProd_CanopyHeight")})
setGeneric(name="swProd_VegInterParam",function(object) {standardGeneric("swProd_VegInterParam")})
setGeneric(name="swProd_LitterInterParam",function(object) {standardGeneric("swProd_LitterInterParam")})
setGeneric(name="swProd_EsTpartitioning_param",function(object) {standardGeneric("swProd_EsTpartitioning_param")})
setGeneric(name="swProd_Es_param_limit",function(object) {standardGeneric("swProd_Es_param_limit")})
setGeneric(name="swProd_Shade",function(object) {standardGeneric("swProd_Shade")})
setGeneric(name="swProd_HydrRedstro_use",function(object) {standardGeneric("swProd_HydrRedstro_use")})
setGeneric(name="swProd_HydrRedstro",function(object) {standardGeneric("swProd_HydrRedstro")})
setGeneric(name="swProd_CritSoilWaterPotential",function(object) {standardGeneric("swProd_CritSoilWaterPotential")})
setGeneric(name="swProd_MonProd_grass",function(object) {standardGeneric("swProd_MonProd_grass")})
setGeneric(name="swProd_MonProd_shrub",function(object) {standardGeneric("swProd_MonProd_shrub")})
setGeneric(name="swProd_MonProd_tree",function(object) {standardGeneric("swProd_MonProd_tree")})
setGeneric(name="swProd_MonProd_forb",function(object) {standardGeneric("swProd_MonProd_forb")})

setGeneric(name="set_swProd<-",function(object, value) {standardGeneric("set_swProd<-")})
setGeneric(name="swProd_Composition<-",function(object, value) {standardGeneric("swProd_Composition<-")})
setGeneric(name="swProd_Albedo<-",function(object, value) {standardGeneric("swProd_Albedo<-")})
setGeneric(name="swProd_Cover_stcr<-",function(object, value) {standardGeneric("swProd_Cover_stcr<-")})
setGeneric(name="swProd_CanopyHeight<-",function(object, value) {standardGeneric("swProd_CanopyHeight<-")})
setGeneric(name="swProd_VegInterParam<-",function(object, value) {standardGeneric("swProd_VegInterParam<-")})
setGeneric(name="swProd_LitterInterParam<-",function(object, value) {standardGeneric("swProd_LitterInterParam<-")})
setGeneric(name="swProd_EsTpartitioning_param<-",function(object, value) {standardGeneric("swProd_EsTpartitioning_param<-")})
setGeneric(name="swProd_Es_param_limit<-",function(object, value) {standardGeneric("swProd_Es_param_limit<-")})
setGeneric(name="swProd_Shade<-",function(object, value) {standardGeneric("swProd_Shade<-")})
setGeneric(name="swProd_HydrRedstro_use<-",function(object, value) {standardGeneric("swProd_HydrRedstro_use<-")})
setGeneric(name="swProd_HydrRedstro<-",function(object, value) {standardGeneric("swProd_HydrRedstro<-")})
setGeneric(name="swProd_CritSoilWaterPotential<-",function(object, value) {standardGeneric("swProd_CritSoilWaterPotential<-")})
setGeneric(name="swProd_MonProd_grass<-",function(object, value) {standardGeneric("swProd_MonProd_grass<-")})
setGeneric(name="swProd_MonProd_shrub<-",function(object, value) {standardGeneric("swProd_MonProd_shrub<-")})
setGeneric(name="swProd_MonProd_tree<-",function(object, value) {standardGeneric("swProd_MonProd_tree<-")})
setGeneric(name="swProd_MonProd_forb<-",function(object, value) {standardGeneric("swProd_MonProd_forb<-")})
########################

#######SITE#############
setGeneric(name="get_swSite",function(object) {standardGeneric("get_swSite")})
setGeneric(name="swSite_SWClimits",function(object) {standardGeneric("swSite_SWClimits")})
setGeneric(name="swSite_ModelFlags",function(object) {standardGeneric("swSite_ModelFlags")})
setGeneric(name="swSite_ModelCoefficients",function(object) {standardGeneric("swSite_ModelCoefficients")})
setGeneric(name="swSite_SnowSimulationParams",function(object) {standardGeneric("swSite_SnowSimulationParams")})
setGeneric(name="swSite_DrainageCoefficient",function(object) {swSite_DrainageCoefficient("swSite_DrainageCoefficient")})
setGeneric(name="swSite_EvapCoefficients",function(object) {standardGeneric("swSite_EvapCoefficients")})
setGeneric(name="swSite_TranspCoefficients",function(object) {standardGeneric("swSite_TranspCoefficients")})
setGeneric(name="swSite_IntrinsicSiteParams",function(object) {standardGeneric("swSite_IntrinsicSiteParams")})
setGeneric(name="swSite_SoilTemperatureFlag",function(object) {standardGeneric("swSite_SoilTemperatureFlag")})
setGeneric(name="swSite_SoilTemperatureConsts",function(object) {standardGeneric("swSite_SoilTemperatureConsts")})
setGeneric(name="swSite_TranspirationRegions",function(object) {standardGeneric("swSite_TranspirationRegions")})

setGeneric(name="set_swSite<-",function(object, value) {standardGeneric("set_swSite<-")})
setGeneric(name="swSite_SWClimits<-",function(object, value) {standardGeneric("swSite_SWClimits<-")})
setGeneric(name="swSite_ModelFlags<-",function(object, value) {standardGeneric("swSite_ModelFlags<-")})
setGeneric(name="swSite_ModelCoefficients<-",function(object, value) {standardGeneric("swSite_ModelCoefficients<-")})
setGeneric(name="swSite_SnowSimulationParams<-",function(object, value) {standardGeneric("swSite_SnowSimulationParams<-")})
setGeneric(name="swSite_DrainageCoefficient<-",function(object, value) {standardGeneric("swSite_DrainageCoefficient<-")})
setGeneric(name="swSite_EvapCoefficients<-",function(object, value) {standardGeneric("swSite_EvapCoefficients<-")})
setGeneric(name="swSite_TranspCoefficients<-",function(object, value) {standardGeneric("swSite_TranspCoefficients<-")})
setGeneric(name="swSite_IntrinsicSiteParams<-",function(object, value) {standardGeneric("swSite_IntrinsicSiteParams<-")})
setGeneric(name="swSite_SoilTemperatureFlag<-",function(object, value) {standardGeneric("swSite_SoilTemperatureFlag<-")})
setGeneric(name="swSite_SoilTemperatureConsts<-",function(object, value) {standardGeneric("swSite_SoilTemperatureConsts<-")})
setGeneric(name="swSite_TranspirationRegions<-",function(object, value) {standardGeneric("swSite_TranspirationRegions<-")})
########################

#########SOILS##########
setGeneric(name="get_swSoils",function(object) {standardGeneric("get_swSoils")})
setGeneric(name="swSoils_Layers",function(object) {standardGeneric("swSoils_Layers")})

setGeneric(name="set_swSoils<-",function(object,value) {standardGeneric("set_swSoils<-")})
setGeneric(name="swSoils_Layers<-",function(object,value) {standardGeneric("swSoils_Layers<-")})
########################

#########ESTAB##########
setGeneric(name="get_swEstab",function(object) {standardGeneric("get_swEstab")})
setGeneric(name="swEstab_useEstab",function(object) {standardGeneric("swEstab_useEstab")})
#species here#

setGeneric(name="set_swEstab<-",function(object, value) {standardGeneric("set_swEstab<-")})
setGeneric(name="swEstab_useEstab<-",function(object, value) {standardGeneric("swEstab_useEstab<-")})
#species here#
########################

#########SWC############
setGeneric(name="get_swSWC",function(object) {standardGeneric("get_swSWC")})
setGeneric(name="swSWC_use",function(object) {standardGeneric("swSWC_use")})
setGeneric(name="swSWC_prefix",function(object) {standardGeneric("swSWC_prefix")})
setGeneric(name="swSWC_FirstYear",function(object) {standardGeneric("swSWC_FirstYear")})
setGeneric(name="swSWC_Method",function(object) {standardGeneric("swSWC_Method")})
setGeneric(name="swSWC_HistoricList",function(object) {standardGeneric("swSWC_HistoricList")})
setGeneric(name="swSWC_HistoricData",function(object, year) {standardGeneric("swSWC_HistoricData")})

setGeneric(name="set_swSWC<-",function(object, value) {standardGeneric("set_swSWC<-")})
setGeneric(name="swSWC_use<-",function(object, value) {standardGeneric("swSWC_use<-")})
setGeneric(name="swSWC_prefix<-",function(object, value) {standardGeneric("swSWC_prefix<-")})
setGeneric(name="swSWC_FirstYear<-",function(object, value) {standardGeneric("swSWC_FirstYear<-")})
setGeneric(name="swSWC_Method<-",function(object, value) {standardGeneric("swSWC_Method<-")})
setGeneric(name="swSWC_HistoricList<-",function(object, value) {standardGeneric("swSWC_HistoricList<-")})
setGeneric(name="swSWC_HistoricData<-",function(object, value) {standardGeneric("swSWC_HistoricData<-")})
########################

#######OUT###########
setGeneric(name="get_swOUT", function(object) {standardGeneric("get_swOUT")})
setGeneric(name="swOUT_TimeStep", function(object) {standardGeneric("swOUT_TimeStep")})
setGeneric(name="swOUT_OutputSeparator", function(object) {standardGeneric("swOUT_OutputSeparator")})
setGeneric(name="swOUT_useTimeStep", function(object) {standardGeneric("swOUT_useTimeStep")})

setGeneric(name="set_swOUT<-",function(object,value) {standardGeneric("set_swOUT<-")})
setGeneric(name="swOUT_TimeStep<-",function(object,value) {standardGeneric("swOUT_TimeStep<-")})
setGeneric(name="swOUT_OutputSeparator<-",function(object,value) {standardGeneric("swOUT_OutputSeparator<-")})
setGeneric(name="swOUT_useTimeStep<-",function(object,value) {standardGeneric("swOUT_useTimeStep<-")})
########################

########LOG#############
setGeneric(name="swLog_setLine<-",function(object, value) {standardGeneric("swLog_setLine<-")})
########################

########swOutput########
setGeneric(name="swOutput_getKEY",function(object,index) {standardGeneric("swOutput_getKEY")})
setGeneric(name="swOutput_KEY_Period",function(object,index) {standardGeneric("swOutput_KEY_Period")})
setGeneric(name="swOutput_KEY_TimeStep",function(object) {standardGeneric("swOutput_KEY_TimeStep")})
setGeneric(name="swOutput_KEY_Columns",function(object) {standardGeneric("swOutput_KEY_Columns")})

setGeneric(name="swOutput_getKEY<-",function(object,index,value) {standardGeneric("swOutput_getKEY<-")})
setGeneric(name="swOutput_KEY_Period<-",function(object,index,value) {standardGeneric("swOutput_KEY_Period<-")})
setGeneric(name="swOutput_KEY_TimeStep<-",function(object,value) {standardGeneric("swOutput_KEY_TimeStep<-")})
setGeneric(name="swOutput_KEY_Columns<-",function(object,value) {standardGeneric("swOutput_KEY_Columns<-")})
