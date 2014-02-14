\name{swInputData-class}
\Rdversion{1.1}
\docType{class}
\alias{swInputData-class}
\alias{get_Markov,swInputData-method}
\alias{get_swCloud,swInputData-method}
\alias{get_swFiles,swInputData-method}
\alias{get_swOUT,swInputData-method}
\alias{get_swProd,swInputData-method}
\alias{get_swSite,swInputData-method}
\alias{get_swSoils,swInputData-method}
\alias{get_swSWC,swInputData-method}
\alias{get_swWeatherData,swInputData-method}
\alias{get_swWeather,swInputData-method}
\alias{get_swYears,swInputData-method}
\alias{get_WeatherHistory,swInputData-method}
\alias{set_Markov<-,swInputData-method}
\alias{set_swCloud<-,swInputData-method}
\alias{set_swFiles<-,swInputData-method}
\alias{set_swOUT<-,swInputData-method}
\alias{set_swProd<-,swInputData-method}
\alias{set_swSite<-,swInputData-method}
\alias{set_swSoils<-,swInputData,swSoils-method}
\alias{set_swSWC<-,swInputData,swSWC-method}
\alias{set_swWeatherData<-,swInputData,swWeatherData-method}
\alias{set_swWeather<-,swInputData-method}
\alias{set_swYears<-,swInputData-method}
\alias{set_WeatherHistory<-,swInputData,list-method}
\alias{swClear,swInputData-method}
\alias{swCloud_Humidity<-,swInputData-method}
\alias{swCloud_Humidity,swInputData-method}
\alias{swCloud_SkyCover<-,swInputData-method}
\alias{swCloud_SkyCover,swInputData-method}
\alias{swCloud_SnowDensity<-,swInputData-method}
\alias{swCloud_SnowDensity,swInputData-method}
\alias{swCloud_Transmissivity<-,swInputData-method}
\alias{swCloud_Transmissivity,swInputData-method}
\alias{swCloud_WindSpeed<-,swInputData-method}
\alias{swCloud_WindSpeed,swInputData-method}
\alias{swFiles_Cloud<-,swInputData-method}
\alias{swFiles_Cloud,swInputData-method}
\alias{swFiles_Estab<-,swInputData-method}
\alias{swFiles_Estab,swInputData-method}
\alias{swFiles_filesIn<-,swInputData-method}
\alias{swFiles_filesIn,swInputData-method}
\alias{swFiles_LogFile<-,swInputData-method}
\alias{swFiles_LogFile,swInputData-method}
\alias{swFiles_MarkovCov<-,swInputData-method}
\alias{swFiles_MarkovCov,swInputData-method}
\alias{swFiles_MarkovProbs<-,swInputData-method}
\alias{swFiles_MarkovProbs,swInputData-method}
\alias{swFiles_OutputPrefix<-,swInputData-method}
\alias{swFiles_OutputPrefix,swInputData-method}
\alias{swFiles_Output<-,swInputData-method}
\alias{swFiles_Output,swInputData-method}
\alias{swFiles_Prod<-,swInputData-method}
\alias{swFiles_Prod,swInputData-method}
\alias{swFiles_ProjDir<-,swInputData-method}
\alias{swFiles_ProjDir,swInputData-method}
\alias{swFiles_SiteParams<-,swInputData-method}
\alias{swFiles_SiteParams,swInputData-method}
\alias{swFiles_Soils<-,swInputData-method}
\alias{swFiles_Soils,swInputData-method}
\alias{swFiles_SWCsetup<-,swInputData-method}
\alias{swFiles_SWCsetup,swInputData-method}
\alias{swFiles_WeatherPrefix<-,swInputData-method}
\alias{swFiles_WeatherPrefix,swInputData-method}
\alias{swFiles_WeatherSetup<-,swInputData-method}
\alias{swFiles_WeatherSetup,swInputData-method}
\alias{swFiles_Years<-,swInputData-method}
\alias{swFiles_Years,swInputData-method}
\alias{swLog_setLine<-,swInputData-method}
\alias{swMarkov_Conv<-,swInputData-method}
\alias{swMarkov_Conv,swInputData-method}
\alias{swMarkov_Prob<-,swInputData-method}
\alias{swMarkov_Prob,swInputData-method}
\alias{swOUT_OutputSeparator<-,swInputData-method}
\alias{swOUT_OutputSeparator,swInputData-method}
\alias{swOUT_TimeStep<-,swInputData-method}
\alias{swOUT_TimeStep,swInputData-method}
\alias{swProd_Albedo<-,swInputData-method}
\alias{swProd_Albedo,swInputData-method}
\alias{swProd_CanopyHeight<-,swInputData-method}
\alias{swProd_CanopyHeight,swInputData-method}
\alias{swProd_Composition<-,swInputData-method}
\alias{swProd_Composition,swInputData-method}
\alias{swProd_Cover_stcr<-,swInputData-method}
\alias{swProd_Cover_stcr,swInputData-method}
\alias{swProd_CritSoilWaterPotential<-,swInputData-method}
\alias{swProd_CritSoilWaterPotential,swInputData-method}
\alias{swProd_Es_param_limit<-,swInputData-method}
\alias{swProd_Es_param_limit,swInputData-method}
\alias{swProd_EsTpartitioning_param<-,swInputData-method}
\alias{swProd_EsTpartitioning_param,swInputData-method}
\alias{swProd_HydrRedstro<-,swInputData-method}
\alias{swProd_HydrRedstro,swInputData-method}
\alias{swProd_HydrRedstro_use<-,swInputData-method}
\alias{swProd_HydrRedstro_use,swInputData-method}
\alias{swProd_LitterInterParam<-,swInputData-method}
\alias{swProd_LitterInterParam,swInputData-method}
\alias{swProd_MonProd_grass<-,swInputData-method}
\alias{swProd_MonProd_grass,swInputData-method}
\alias{swProd_MonProd_shrub<-,swInputData-method}
\alias{swProd_MonProd_shrub,swInputData-method}
\alias{swProd_MonProd_tree<-,swInputData-method}
\alias{swProd_MonProd_tree,swInputData-method}
\alias{swProd_Shade<-,swInputData-method}
\alias{swProd_Shade,swInputData-method}
\alias{swProd_VegInterParam<-,swInputData-method}
\alias{swProd_VegInterParam,swInputData-method}
\alias{swReadLines,swInputData,character-method}
\alias{swSite_DrainageCoefficient<-,swInputData-method}
\alias{swSite_DrainageCoefficient,swInputData-method}
\alias{swSite_EvapCoefficients<-,swInputData-method}
\alias{swSite_EvapCoefficients,swInputData-method}
\alias{swSite_IntrinsicSiteParams<-,swInputData-method}
\alias{swSite_IntrinsicSiteParams,swInputData-method}
\alias{swSite_ModelCoefficients<-,swInputData-method}
\alias{swSite_ModelCoefficients,swInputData-method}
\alias{swSite_ModelFlags<-,swInputData-method}
\alias{swSite_ModelFlags,swInputData-method}
\alias{swSite_SnowSimulationParams<-,swInputData-method}
\alias{swSite_SnowSimulationParams,swInputData-method}
\alias{swSite_SoilTemperatureConsts<-,swInputData-method}
\alias{swSite_SoilTemperatureConsts,swInputData-method}
\alias{swSite_SoilTemperatureFlag<-,swInputData-method}
\alias{swSite_SoilTemperatureFlag,swInputData-method}
\alias{swSite_SWClimits<-,swInputData-method}
\alias{swSite_SWClimits,swInputData-method}
\alias{swSite_TranspCoefficients<-,swInputData-method}
\alias{swSite_TranspCoefficients,swInputData-method}
\alias{swSite_TranspirationRegions<-,swInputData-method}
\alias{swSite_TranspirationRegions,swInputData-method}
\alias{swSoils_Layers<-,swInputData,matrix-method}
\alias{swSoils_Layers,swInputData-method}
\alias{swSWC_FirstYear<-,swInputData,integer-method}
\alias{swSWC_FirstYear,swInputData-method}
\alias{swSWC_HistoricData<-,swInputData,swSWC_hist-method}
\alias{swSWC_HistoricData,swInputData-method}
\alias{swSWC_HistoricList<-,swInputData,list-method}
\alias{swSWC_HistoricList,swInputData-method}
\alias{swSWC_Method<-,swInputData,integer-method}
\alias{swSWC_Method,swInputData-method}
\alias{swSWC_prefix<-,swInputData,character-method}
\alias{swSWC_prefix,swInputData-method}
\alias{swSWC_use<-,swInputData,logical-method}
\alias{swSWC_use,swInputData-method}
\alias{swWeather_DaysRunningAverage<-,swInputData-method}
\alias{swWeather_DaysRunningAverage,swInputData-method}
\alias{swWeather_FirstYearHistorical<-,swInputData-method}
\alias{swWeather_FirstYearHistorical,swInputData-method}
\alias{swWeather_MonScalingParams<-,swInputData-method}
\alias{swWeather_MonScalingParams,swInputData-method}
\alias{swWeather_pct_SnowDrift<-,swInputData-method}
\alias{swWeather_pct_SnowDrift,swInputData-method}
\alias{swWeather_pct_SnowRunoff<-,swInputData-method}
\alias{swWeather_pct_SnowRunoff,swInputData-method}
\alias{swWeather_UseMarkov<-,swInputData-method}
\alias{swWeather_UseMarkov,swInputData-method}
\alias{swWeather_UseSnow<-,swInputData-method}
\alias{swWeather_UseSnow,swInputData-method}
\alias{swWriteLines,swInputData,character-method}
\alias{swYears_EDOEY<-,swInputData-method}
\alias{swYears_EDOEY,swInputData-method}
\alias{swYears_EndYear<-,swInputData-method}
\alias{swYears_EndYear,swInputData-method}
\alias{swYears_FDOFY<-,swInputData-method}
\alias{swYears_FDOFY,swInputData-method}
\alias{swYears_isNorth<-,swInputData-method}
\alias{swYears_isNorth,swInputData-method}
\alias{swYears_StartYear<-,swInputData-method}
\alias{swYears_StartYear,swInputData-method}

\title{Class \code{"swInputData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
	This class is a container class to the input file S4 objects.
	The generic functions listed work on this and the proper sw
	Class in the container's slots.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("swInputData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{files}:}{Object of class \code{"swFiles"} ~~ }
    \item{\code{years}:}{Object of class \code{"swYears"} ~~ }
    \item{\code{weather}:}{Object of class \code{"swWeather"} ~~ }
    \item{\code{cloud}:}{Object of class \code{"swCloud"} ~~ }
    \item{\code{weatherHistory}:}{Object of class \code{"list"} ~~ }
    \item{\code{markov}:}{Object of class \code{"swMarkov"} ~~ }
    \item{\code{prod}:}{Object of class \code{"swProd"} ~~ }
    \item{\code{site}:}{Object of class \code{"swSite"} ~~ }
    \item{\code{soils}:}{Object of class \code{"swSoils"} ~~ }
    \item{\code{estab}:}{Object of class \code{"swEstab"} ~~ }
    \item{\code{output}:}{Object of class \code{"swOUT"} ~~ }
    \item{\code{swc}:}{Object of class \code{"swSWC"} ~~ }
    \item{\code{log}:}{Object of class \code{"swLog"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{get_Markov}{\code{signature(object = "swInputData")}: ... }
    \item{get_swCloud}{\code{signature(object = "swInputData")}: ... }
    \item{get_swFiles}{\code{signature(object = "swInputData")}: ... }
    \item{get_swOUT}{\code{signature(object = "swInputData")}: ... }
    \item{get_swProd}{\code{signature(object = "swInputData")}: ... }
    \item{get_swSite}{\code{signature(object = "swInputData")}: ... }
    \item{get_swSoils}{\code{signature(object = "swInputData")}: ... }
    \item{get_swSWC}{\code{signature(object = "swInputData")}: ... }
    \item{get_swWeatherData}{\code{signature(object = "swInputData")}: ... }
    \item{get_swWeather}{\code{signature(object = "swInputData")}: ... }
    \item{get_swYears}{\code{signature(object = "swInputData")}: ... }
    \item{get_WeatherHistory}{\code{signature(object = "swInputData")}: ... }
    \item{set_Markov<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swCloud<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swFiles<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swOUT<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swProd<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swSite<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swSoils<-}{\code{signature(object = "swInputData", value = "swSoils")}: ... }
    \item{set_swSWC<-}{\code{signature(object = "swInputData", swSWC = "swSWC")}: ... }
    \item{set_swWeatherData<-}{\code{signature(object = "swInputData", value = "swWeatherData")}: ... }
    \item{set_swWeather<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_swYears<-}{\code{signature(object = "swInputData")}: ... }
    \item{set_WeatherHistory<-}{\code{signature(object = "swInputData", value = "list")}: ... }
    \item{swClear}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_Humidity<-}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_Humidity}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_SkyCover<-}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_SkyCover}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_SnowDensity<-}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_SnowDensity}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_Transmissivity<-}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_Transmissivity}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_WindSpeed<-}{\code{signature(object = "swInputData")}: ... }
    \item{swCloud_WindSpeed}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Cloud<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Cloud}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Estab<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Estab}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_filesIn<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_filesIn}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_LogFile<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_LogFile}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_MarkovCov<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_MarkovCov}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_MarkovProbs<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_MarkovProbs}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_OutputPrefix<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_OutputPrefix}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Output<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Output}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Prod<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Prod}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_ProjDir<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_ProjDir}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_SiteParams<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_SiteParams}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Soils<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Soils}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_SWCsetup<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_SWCsetup}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_WeatherPrefix<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_WeatherPrefix}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_WeatherSetup<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_WeatherSetup}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Years<-}{\code{signature(object = "swInputData")}: ... }
    \item{swFiles_Years}{\code{signature(object = "swInputData")}: ... }
    \item{swLog_setLine<-}{\code{signature(object = "swInputData")}: ... }
    \item{swMarkov_Conv<-}{\code{signature(object = "swInputData")}: ... }
    \item{swMarkov_Conv}{\code{signature(object = "swInputData")}: ... }
    \item{swMarkov_Prob<-}{\code{signature(object = "swInputData")}: ... }
    \item{swMarkov_Prob}{\code{signature(object = "swInputData")}: ... }
    \item{swOUT_OutputSeparator<-}{\code{signature(object = "swInputData")}: ... }
    \item{swOUT_OutputSeparator}{\code{signature(object = "swInputData")}: ... }
    \item{swOUT_TimeStep<-}{\code{signature(object = "swInputData")}: ... }
    \item{swOUT_TimeStep}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Albedo<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Albedo}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_CanopyHeight<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_CanopyHeight}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Composition<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Composition}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Cover_stcr<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Cover_stcr}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_CritSoilWaterPotential<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_CritSoilWaterPotential}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Es_param_limit<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Es_param_limit}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_EsTpartitioning_param<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_EsTpartitioning_param}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_HydrRedstro<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_HydrRedstro}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_HydrRedstro_use<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_HydrRedstro_use}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_LitterInterParam<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_LitterInterParam}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_grass<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_grass}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_shrub<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_shrub}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_tree<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_MonProd_tree}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Shade<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_Shade}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_VegInterParam<-}{\code{signature(object = "swInputData")}: ... }
    \item{swProd_VegInterParam}{\code{signature(object = "swInputData")}: ... }
    \item{swReadLines}{\code{signature(object = "swInputData", file = "character")}: ... }
    \item{swSite_DrainageCoefficient<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_DrainageCoefficient}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_EvapCoefficients<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_EvapCoefficients}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_IntrinsicSiteParams<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_IntrinsicSiteParams}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_ModelCoefficients<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_ModelCoefficients}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_ModelFlags<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_ModelFlags}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SnowSimulationParams<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SnowSimulationParams}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SoilTemperatureConsts<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SoilTemperatureConsts}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SoilTemperatureFlag<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SoilTemperatureFlag}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SWClimits<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_SWClimits}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_TranspCoefficients<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_TranspCoefficients}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_TranspirationRegions<-}{\code{signature(object = "swInputData")}: ... }
    \item{swSite_TranspirationRegions}{\code{signature(object = "swInputData")}: ... }
    \item{swSoils_Layers<-}{\code{signature(object = "swInputData", value = "matrix")}: ... }
    \item{swSoils_Layers}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_FirstYear<-}{\code{signature(object = "swInputData", SWCfirstYear = "integer")}: ... }
    \item{swSWC_FirstYear}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_HistoricData<-}{\code{signature(object = "swInputData", swSWCdata = "swSWC_hist")}: ... }
    \item{swSWC_HistoricData}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_HistoricList<-}{\code{signature(object = "swInputData", swSWC_HistoricList = "list")}: ... }
    \item{swSWC_HistoricList}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_Method<-}{\code{signature(object = "swInputData", SWCmethod = "integer")}: ... }
    \item{swSWC_Method}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_prefix<-}{\code{signature(object = "swInputData", SWCprefix = "character")}: ... }
    \item{swSWC_prefix}{\code{signature(object = "swInputData")}: ... }
    \item{swSWC_use<-}{\code{signature(object = "swInputData", UseSWC = "logical")}: ... }
    \item{swSWC_use}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_DaysRunningAverage<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_DaysRunningAverage}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_FirstYearHistorical<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_FirstYearHistorical}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_MonScalingParams<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_MonScalingParams}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_pct_SnowDrift<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_pct_SnowDrift}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_pct_SnowRunoff<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_pct_SnowRunoff}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_UseMarkov<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_UseMarkov}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_UseSnow<-}{\code{signature(object = "swInputData")}: ... }
    \item{swWeather_UseSnow}{\code{signature(object = "swInputData")}: ... }
    \item{swWriteLines}{\code{signature(object = "swInputData", file = "character")}: ... }
    \item{swYears_EDOEY<-}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_EDOEY}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_EndYear<-}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_EndYear}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_FDOFY<-}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_FDOFY}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_isNorth<-}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_isNorth}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_StartYear<-}{\code{signature(object = "swInputData")}: ... }
    \item{swYears_StartYear}{\code{signature(object = "swInputData")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
	https://github.com/Burke-Lauenroth-Lab/Rsoilwat
}
\author{
%%  ~~who you are~~
	Ryan Murphy
}
\note{
%%  ~~further notes~~
	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\linkS4class{swFiles}}
\code{\linkS4class{swYears}}
\code{\linkS4class{swWeather}}
\code{\linkS4class{swCloud}}
\code{\linkS4class{swMarkov}}
\code{\linkS4class{swProd}}
\code{\linkS4class{swSite}}
\code{\linkS4class{swSoils}}
\code{\linkS4class{swEstab}}
\code{\linkS4class{swOUT}}
\code{\linkS4class{swSWC}}
\code{\linkS4class{swLog}}
}
\examples{
showClass("swInputData")
}
\keyword{classes}
