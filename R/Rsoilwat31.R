#' Package `Rsoilwat31': summary information
#' 
#' The package `Rsoilwat' / 'Rsoilwat31' is a R implementation of SoilWat
#' Modeling Software.  The C code was converted into a library with
#' input/output functions to R.  SoilWat is a simulator of the dynamics of soil
#' water. Originally written in the 1900's by a highly trained team of Botany's
#' finest. It is now maintained by Dr. William Lauenroth's Lab and Dr. Daniel
#' Schlaepfer.  The input data structure is S4. Generic functions that
#' apply to the input container or individual object control setting/getting
#' input data. Output data is in the form of lists.
#' 
#' 
#' @aliases Rsoilwat31 Rsoilwat31-package
#' @section Version: The version level of the package is given by the command
#' \code{packageDescription("Rsoilwat31")}. The most recent version of the
#' package can be obtained from github at
#' \url{https://github.com/Burke-Lauenroth-Lab/Rsoilwat}
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @references Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth (2014)
#' Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The
#' consequences of climate change and disturbance.  \emph{Ecosystems}
#' \bold{17}:590--605.
#' 
#' Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford (2012)
#' Ecohydrological niche of sagebrush ecosystems.  \emph{Ecohydrology}
#' \bold{5}:453--466.
#' 
#' Parton, W.J. (1978).  \emph{Abiotic section of ELM}. In: Grassland
#' simulation model (ed. Innis, G.S.).  Springer New York, NY, pp. 31--53.
#' 
#' Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992) Long-term soil-water
#' dynamics in the shortgrass steppe.  \emph{Ecology} \bold{73}:1175--1181.
#'
#' @docType package
#' @name Rsoilwat31
#' @aliases Rsoilwat, SoilWat, SOILWAT, Rsoilwat-package
NULL





#' Class \code{"swInputData"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~ This class
#' is a container class to the input file S4 objects.  The generic functions
#' listed work on this and the proper sw Class in the container's slots.
#' 
#' 
#' @name swInputData-class
#' @aliases swInputData-class get_Markov,swInputData-method
#' get_swCloud,swInputData-method get_swFiles,swInputData-method
#' get_swOUT,swInputData-method get_swProd,swInputData-method
#' get_swSite,swInputData-method get_swSoils,swInputData-method
#' get_swSWC,swInputData-method get_swWeatherData,swInputData-method
#' get_swWeather,swInputData-method get_swYears,swInputData-method
#' get_WeatherHistory,swInputData-method set_Markov<-,swInputData-method
#' set_swCloud<-,swInputData-method set_swFiles<-,swInputData-method
#' set_swOUT<-,swInputData-method set_swProd<-,swInputData-method
#' set_swSite<-,swInputData-method set_swSoils<-,swInputData,swSoils-method
#' set_swSWC<-,swInputData,swSWC-method
#' set_swWeatherData<-,swInputData,swWeatherData-method
#' set_swWeather<-,swInputData-method set_swYears<-,swInputData-method
#' set_WeatherHistory<-,swInputData,list-method swClear,swInputData-method
#' swCloud_Humidity<-,swInputData-method swCloud_Humidity,swInputData-method
#' swCloud_SkyCover<-,swInputData-method swCloud_SkyCover,swInputData-method
#' swCloud_SnowDensity<-,swInputData-method
#' swCloud_SnowDensity,swInputData-method
#' swCloud_Transmissivity<-,swInputData-method
#' swCloud_Transmissivity,swInputData-method
#' swCloud_WindSpeed<-,swInputData-method swCloud_WindSpeed,swInputData-method
#' swFiles_Cloud<-,swInputData-method swFiles_Cloud,swInputData-method
#' swFiles_Estab<-,swInputData-method swFiles_Estab,swInputData-method
#' swFiles_filesIn<-,swInputData-method swFiles_filesIn,swInputData-method
#' swFiles_LogFile<-,swInputData-method swFiles_LogFile,swInputData-method
#' swFiles_MarkovCov<-,swInputData-method swFiles_MarkovCov,swInputData-method
#' swFiles_MarkovProbs<-,swInputData-method
#' swFiles_MarkovProbs,swInputData-method
#' swFiles_OutputPrefix<-,swInputData-method
#' swFiles_OutputPrefix,swInputData-method swFiles_Output<-,swInputData-method
#' swFiles_Output,swInputData-method swFiles_Prod<-,swInputData-method
#' swFiles_Prod,swInputData-method swFiles_ProjDir<-,swInputData-method
#' swFiles_ProjDir,swInputData-method swFiles_SiteParams<-,swInputData-method
#' swFiles_SiteParams,swInputData-method swFiles_Soils<-,swInputData-method
#' swFiles_Soils,swInputData-method swFiles_SWCsetup<-,swInputData-method
#' swFiles_SWCsetup,swInputData-method
#' swFiles_WeatherPrefix<-,swInputData-method
#' swFiles_WeatherPrefix,swInputData-method
#' swFiles_WeatherSetup<-,swInputData-method
#' swFiles_WeatherSetup,swInputData-method swFiles_Years<-,swInputData-method
#' swFiles_Years,swInputData-method swLog_setLine<-,swInputData-method
#' swMarkov_Conv<-,swInputData-method swMarkov_Conv,swInputData-method
#' swMarkov_Prob<-,swInputData-method swMarkov_Prob,swInputData-method
#' swOUT_OutputSeparator<-,swInputData-method
#' swOUT_OutputSeparator,swInputData-method swOUT_TimeStep<-,swInputData-method
#' swOUT_TimeStep,swInputData-method swProd_Albedo<-,swInputData-method
#' swProd_Albedo,swInputData-method swProd_CanopyHeight<-,swInputData-method
#' swProd_CanopyHeight,swInputData-method
#' swProd_Composition<-,swInputData-method
#' swProd_Composition,swInputData-method swProd_Cover_stcr<-,swInputData-method
#' swProd_Cover_stcr,swInputData-method
#' swProd_CritSoilWaterPotential<-,swInputData-method
#' swProd_CritSoilWaterPotential,swInputData-method
#' swProd_Es_param_limit<-,swInputData-method
#' swProd_Es_param_limit,swInputData-method
#' swProd_EsTpartitioning_param<-,swInputData-method
#' swProd_EsTpartitioning_param,swInputData-method
#' swProd_HydrRedstro<-,swInputData-method
#' swProd_HydrRedstro,swInputData-method
#' swProd_HydrRedstro_use<-,swInputData-method
#' swProd_HydrRedstro_use,swInputData-method
#' swProd_LitterInterParam<-,swInputData-method
#' swProd_LitterInterParam,swInputData-method
#' swProd_MonProd_grass<-,swInputData-method
#' swProd_MonProd_grass,swInputData-method
#' swProd_MonProd_shrub<-,swInputData-method
#' swProd_MonProd_shrub,swInputData-method
#' swProd_MonProd_tree<-,swInputData-method
#' swProd_MonProd_tree,swInputData-method swProd_Shade<-,swInputData-method
#' swProd_Shade,swInputData-method swProd_VegInterParam<-,swInputData-method
#' swProd_VegInterParam,swInputData-method
#' swReadLines,swInputData,character-method
#' swSite_DrainageCoefficient<-,swInputData-method
#' swSite_DrainageCoefficient,swInputData-method
#' swSite_EvapCoefficients<-,swInputData-method
#' swSite_EvapCoefficients,swInputData-method
#' swSite_IntrinsicSiteParams<-,swInputData-method
#' swSite_IntrinsicSiteParams,swInputData-method
#' swSite_ModelCoefficients<-,swInputData-method
#' swSite_ModelCoefficients,swInputData-method
#' swSite_ModelFlags<-,swInputData-method swSite_ModelFlags,swInputData-method
#' swSite_SnowSimulationParams<-,swInputData-method
#' swSite_SnowSimulationParams,swInputData-method
#' swSite_SoilTemperatureConsts<-,swInputData-method
#' swSite_SoilTemperatureConsts,swInputData-method
#' swSite_SoilTemperatureFlag<-,swInputData-method
#' swSite_SoilTemperatureFlag,swInputData-method
#' swSite_SWClimits<-,swInputData-method swSite_SWClimits,swInputData-method
#' swSite_TranspCoefficients<-,swInputData-method
#' swSite_TranspCoefficients,swInputData-method
#' swSite_TranspirationRegions<-,swInputData-method
#' swSite_TranspirationRegions,swInputData-method
#' swSoils_Layers<-,swInputData,matrix-method swSoils_Layers,swInputData-method
#' swSWC_FirstYear<-,swInputData,integer-method
#' swSWC_FirstYear,swInputData-method
#' swSWC_HistoricData<-,swInputData,swSWC_hist-method
#' swSWC_HistoricData,swInputData-method
#' swSWC_HistoricList<-,swInputData,list-method
#' swSWC_HistoricList,swInputData-method
#' swSWC_Method<-,swInputData,integer-method swSWC_Method,swInputData-method
#' swSWC_prefix<-,swInputData,character-method swSWC_prefix,swInputData-method
#' swSWC_use<-,swInputData,logical-method swSWC_use,swInputData-method
#' swWeather_DaysRunningAverage<-,swInputData-method
#' swWeather_DaysRunningAverage,swInputData-method
#' swWeather_FirstYearHistorical<-,swInputData-method
#' swWeather_FirstYearHistorical,swInputData-method
#' swWeather_MonScalingParams<-,swInputData-method
#' swWeather_MonScalingParams,swInputData-method
#' swWeather_pct_SnowDrift<-,swInputData-method
#' swWeather_pct_SnowDrift,swInputData-method
#' swWeather_pct_SnowRunoff<-,swInputData-method
#' swWeather_pct_SnowRunoff,swInputData-method
#' swWeather_UseMarkov<-,swInputData-method
#' swWeather_UseMarkov,swInputData-method
#' swWeather_UseSnow<-,swInputData-method swWeather_UseSnow,swInputData-method
#' swWriteLines,swInputData,character-method swYears_EDOEY<-,swInputData-method
#' swYears_EDOEY,swInputData-method swYears_EndYear<-,swInputData-method
#' swYears_EndYear,swInputData-method swYears_FDOFY<-,swInputData-method
#' swYears_FDOFY,swInputData-method swYears_isNorth<-,swInputData-method
#' swYears_isNorth,swInputData-method swYears_StartYear<-,swInputData-method
#' swYears_StartYear,swInputData-method
#' @docType class
#' @note %% ~~further notes~~
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swInputData", ...)}. %% ~~ describe objects here ~~
#' @author %% ~~who you are~~ Ryan Murphy
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{swFiles}} \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references %% ~~put references to the literature/web site here~~
#' https://github.com/Burke-Lauenroth-Lab/Rsoilwat
#' @keywords classes
#' @examples
#' 
#' showClass("swInputData")
#' 
NULL




