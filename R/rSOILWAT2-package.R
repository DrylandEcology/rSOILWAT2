#' Package `rSOILWAT2': summary information
#'
#' The package `rSOILWAT2' is a R implementation of SoilWat
#' Modeling Software.  The C code was converted into a library with
#' input/output functions to R.  SoilWat is a simulator of the dynamics of soil
#' water. Originally written in the 1900's by a highly trained team of Botany's
#' finest. It is now maintained by Dr. William Lauenroth's Lab and Dr. Daniel
#' Schlaepfer.  The input data structure is S4. Generic functions that
#' apply to the input container or individual object control setting/getting
#' input data. Output data is in the form of lists.
#'
#'
#' @section Version: The version level of the package is given by the command
#' \code{packageDescription("rSOILWAT2")}. The most recent version of the
#' package can be obtained from github at
#' \url{https://github.com/DrylandEcology/rSOILWAT2}
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
#' @name rSOILWAT2
#' @aliases rSOILWAT2 rSOILWAT2-package rSOILWAT2 SoilWat SOILWAT SOILWAT2
#   rSOILWAT2-package Rsoilwat Rsoilwat31
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


##------ Import from other packages
## Package uses S3/S4 classes - they are defined in package:methods
#' @importFrom methods slot slot<- as as<- initialize new slotNames inheritedSlotNames getSlots validObject callNextMethod
#' @importFrom stats aggregate na.exclude
#' @importFrom utils data head packageDescription read.csv read.table str tail write.table
NULL

##------ Access SOILWAT2 C code
#' @useDynLib rSOILWAT2, .registration = TRUE, .fixes = "C_"
NULL

#' Class \code{"swInputData"}
#'
#' This class is a container class to the input file S4 objects. The generic functions
#' listed work on this and the proper sw classes in the container's slots.
#'
#'
#' @name swInputData-class
#' @aliases swInputData-class
#' get_Markov swInputData-method
#' get_swCloud swInputData-method
#' get_swFiles swInputData-method
#' get_swOUT swInputData-method
#' get_swProd swInputData-method
#' get_swSite swInputData-method
#' get_swSoils swInputData-method
#' get_swSWC swInputData-method
#' get_swWeatherData swInputData-method
#' get_swWeather swInputData-method
#' get_swYears swInputData-method
#' get_WeatherHistory swInputData-method
#' get_swCarbon,swInputData-method
#' set_swCarbon<-,swInputData-method
#' set_Markov<- swInputData-method
#' set_swCloud<- swInputData-method
#' set_swFiles<- swInputData-method
#' set_swOUT<- swInputData-method
#' set_swProd<- swInputData-method
#' set_swSite<- swInputData-method
#' set_swSoils<- swInputData swSoils-method
#' set_swSWC<- swInputData swSWC-method
#' set_swWeatherData<- swInputData swWeatherData-method
#' set_swWeather<- swInputData-method
#' set_swYears<- swInputData-method
#' set_WeatherHistory<- swInputData list-method
#' swCloud_Humidity<- swInputData-method
#' swCloud_Humidity swInputData-method
#' swCloud_SkyCover<- swInputData-method
#' swCloud_SkyCover swInputData-method
#' swCloud_SnowDensity<- swInputData-method
#' swCloud_SnowDensity swInputData-method
#' swCloud_Transmissivity<- swInputData-method
#' swCloud_Transmissivity swInputData-method
#' swCloud_WindSpeed<- swInputData-method
#' swCloud_WindSpeed swInputData-method
#' swFiles_Cloud<- swInputData-method
#' swFiles_Cloud swInputData-method
#' swFiles_Estab<- swInputData-method
#' swFiles_Estab swInputData-method
#' swFiles_Carbon<-,swInputData-method
#' swFiles_Carbon,swInputData-method
#' swCarbon_Use_Bio,swInputData-method
#' swCarbon_Use_WUE,swInputData-method
#' swCarbon_Scenario,swInputData-method
#' swCarbon_DeltaYear,swInputData-method
#' swCarbon_CO2ppm,swInputData-method
#' swCarbon_Scenario<-,swInputData-method
#' swCarbon_DeltaYear<-,swInputData-method
#' swCarbon_CO2ppm<-,swInputData-method
#' swCarbon_Use_Bio<-,swInputData-method
#' swCarbon_Use_WUE<-,swInputData-method
#' swFiles_filesIn<- swInputData-method
#' swFiles_filesIn swInputData-method
#' swFiles_LogFile<- swInputData-method
#' swFiles_LogFile swInputData-method
#' swFiles_MarkovCov<- swInputData-method
#' swFiles_MarkovCov swInputData-method
#' swFiles_MarkovProbs<- swInputData-method
#' swFiles_MarkovProbs swInputData-method
#' swFiles_OutputPrefix<- swInputData-method
#' swFiles_OutputPrefix swInputData-method
#' swFiles_Output<- swInputData-method
#' swFiles_Output swInputData-method
#' swFiles_Prod<- swInputData-method
#' swFiles_Prod swInputData-method
#' swFiles_ProjDir<- swInputData-method
#' swFiles_ProjDir swInputData-method
#' swFiles_SiteParams<- swInputData-method
#' swFiles_SiteParams swInputData-method
#' swFiles_Soils<- swInputData-method
#' swFiles_Soils swInputData-method
#' swFiles_SWCsetup<- swInputData-method
#' swFiles_SWCsetup swInputData-method
#' swFiles_WeatherPrefix<- swInputData-method
#' swFiles_WeatherPrefix swInputData-method
#' swFiles_WeatherSetup<- swInputData-method
#' swFiles_WeatherSetup swInputData-method
#' swFiles_Years<- swInputData-method
#' swFiles_Years swInputData-method
#' swLog_setLine<- swInputData-method
#' swMarkov_Conv<- swInputData-method
#' swMarkov_Conv swInputData-method
#' swMarkov_Prob<- swInputData-method
#' swMarkov_Prob swInputData-method
#' swOUT_OutputSeparator<- swInputData-method
#' swOUT_OutputSeparator swInputData-method
#' swOUT_TimeStep<- swInputData-method
#' swOUT_TimeStep swInputData-method
#' swProd_Albedo<- swInputData-method
#' swProd_Albedo swInputData-method
#' swProd_CanopyHeight<- swInputData-method
#' swProd_CanopyHeight swInputData-method
#' swProd_Composition<- swInputData-method
#' swProd_Composition swInputData-method
#' swProd_Cover_stcr<- swInputData-method
#' swProd_Cover_stcr swInputData-method
#' swProd_CritSoilWaterPotential<- swInputData-method
#' swProd_CritSoilWaterPotential swInputData-method
#' swProd_Es_param_limit<- swInputData-method
#' swProd_Es_param_limit swInputData-method
#' swProd_EsTpartitioning_param<- swInputData-method
#' swProd_EsTpartitioning_param swInputData-method
#' swProd_HydrRedstro<- swInputData-method
#' swProd_HydrRedstro swInputData-method
#' swProd_HydrRedstro_use<- swInputData-method
#' swProd_HydrRedstro_use swInputData-method
#' swProd_LitterInterParam<- swInputData-method
#' swProd_LitterInterParam swInputData-method
#' swProd_MonProd_grass<- swInputData-method
#' swProd_MonProd_grass swInputData-method
#' swProd_MonProd_shrub<- swInputData-method
#' swProd_MonProd_shrub swInputData-method
#' swProd_MonProd_tree<- swInputData-method
#' swProd_MonProd_tree swInputData-method
#' swProd_Shade<- swInputData-method
#' swProd_Shade swInputData-method
#' swProd_VegInterParam<- swInputData-method
#' swProd_VegInterParam swInputData-method
#' swReadLines swInputData character-method
#' swSite_DrainageCoefficient<- swInputData-method
#' swSite_DrainageCoefficient swInputData-method
#' swSite_EvapCoefficients<- swInputData-method
#' swSite_EvapCoefficients swInputData-method
#' swSite_IntrinsicSiteParams<- swInputData-method
#' swSite_IntrinsicSiteParams swInputData-method
#' swSite_ModelCoefficients<- swInputData-method
#' swSite_ModelCoefficients swInputData-method
#' swSite_ModelFlags<- swInputData-method
#' swSite_ModelFlags swInputData-method
#' swSite_SnowSimulationParams<- swInputData-method
#' swSite_SnowSimulationParams swInputData-method
#' swSite_SoilTemperatureConsts<- swInputData-method
#' swSite_SoilTemperatureConsts swInputData-method
#' swSite_SoilTemperatureFlag<- swInputData-method
#' swSite_SoilTemperatureFlag swInputData-method
#' swSite_SWClimits<- swInputData-method
#' swSite_SWClimits swInputData-method
#' swSite_TranspCoefficients<- swInputData-method
#' swSite_TranspCoefficients swInputData-method
#' swSite_TranspirationRegions<- swInputData-method
#' swSite_TranspirationRegions swInputData-method
#' swSoils_Layers<- swInputData matrix-method
#' swSoils_Layers swInputData-method
#' swSWC_FirstYear<- swInputData integer-method
#' swSWC_FirstYear swInputData-method
#' swSWC_HistoricData<- swInputData swSWC_hist-method
#' swSWC_HistoricData swInputData-method
#' swSWC_HistoricList<- swInputData list-method
#' swSWC_HistoricList swInputData-method
#' swSWC_Method<- swInputData integer-method
#' swSWC_Method swInputData-method
#' swSWC_prefix<- swInputData character-method
#' swSWC_prefix swInputData-method
#' swSWC_use<- swInputData logical-method
#' swSWC_use swInputData-method
#' swWeather_DaysRunningAverage<- swInputData-method
#' swWeather_DaysRunningAverage swInputData-method
#' swWeather_FirstYearHistorical<- swInputData-method
#' swWeather_FirstYearHistorical swInputData-method
#' swWeather_MonScalingParams<- swInputData-method
#' swWeather_MonScalingParams swInputData-method
#' swWeather_pct_SnowDrift<- swInputData-method
#' swWeather_pct_SnowDrift swInputData-method
#' swWeather_pct_SnowRunoff<- swInputData-method
#' swWeather_pct_SnowRunoff swInputData-method
#' swWeather_UseMarkov<- swInputData-method
#' swWeather_UseMarkov swInputData-method
#' swWeather_UseSnow<- swInputData-method
#' swWeather_UseSnow swInputData-method
#' swYears_EDOEY<- swInputData-method
#' swYears_EDOEY swInputData-method
#' swYears_EndYear<- swInputData-method
#' swYears_EndYear swInputData-method
#' swYears_FDOFY<- swInputData-method
#' swYears_FDOFY swInputData-method
#' swYears_isNorth<- swInputData-method
#' swYears_isNorth swInputData-method
#' swYears_StartYear<- swInputData-method
#' swYears_StartYear swInputData-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swInputData", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swFiles}} \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' \code{\linkS4class{swCarbon}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swInputData")
#'
NULL

#' Class \code{"swFiles"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swFiles-class
#' @aliases swFiles_Cloud<- swFiles-method
#' swFiles_Cloud swFiles-method
#' swFiles_Estab<- swFiles-method
#' swFiles_Estab swFiles-method
#' swFiles_Carbon<- swFiles-method
#' swFiles_Carbon swFiles-method
#' swFiles_filesIn<- swFiles-method
#' swFiles_filesIn swFiles-method
#' swFiles_LogFile<- swFiles-method
#' swFiles_LogFile swFiles-method
#' swFiles_MarkovCov<- swFiles-method
#' swFiles_MarkovCov swFiles-method
#' swFiles_MarkovProbs<- swFiles-method
#' swFiles_MarkovProbs swFiles-method
#' swFiles_OutputPrefix<- swFiles-method
#' swFiles_OutputPrefix swFiles-method
#' swFiles_Output<- swFiles-method
#' swFiles_Output swFiles-method
#' swFiles_Prod<- swFiles-method
#' swFiles_Prod swFiles-method
#' swFiles_ProjDir<- swFiles-method
#' swFiles_ProjDir swFiles-method
#' swFiles_SiteParams<- swFiles-method
#' swFiles_SiteParams swFiles-method
#' swFiles_Soils<- swFiles-method
#' swFiles_Soils swFiles-method
#' swFiles_SWCsetup<- swFiles-method
#' swFiles_SWCsetup swFiles-method
#' swFiles_WeatherPrefix<- swFiles-method
#' swFiles_WeatherPrefix swFiles-method
#' swFiles_WeatherSetup<- swFiles-method
#' swFiles_WeatherSetup swFiles-method
#' swFiles_Years<- swFiles-method
#' swFiles_Years swFiles-method
#' swReadLines swFiles-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swFiles", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' \code{\linkS4class{swCarbon}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swFiles")
#'
NULL


#' Class \code{"swCarbon"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swCarbon-class
#' @aliases
#' swCarbon_Use_Bio,swCarbon-method
#' swCarbon_Use_WUE,swCarbon-method
#' swCarbon_Scenario,swCarbon-method
#' swCarbon_DeltaYear,swCarbon-method
#' swCarbon_CO2ppm,swCarbon-method
#' swCarbon_Scenario<-,swCarbon-method
#' swCarbon_DeltaYear<-,swCarbon-method
#' swCarbon_CO2ppm<-,swCarbon-method
#' swCarbon_Use_Bio<-,swCarbon-method
#' swCarbon_Use_WUE<-,swCarbon-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swCarbon", ...)}.
#' @author Zachary Kramer
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swCarbon")
#'
NULL


#' Class \code{"swYears"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swYears-class
#' @aliases swYears_EDOEY<- swYears-method
#' swYears_EDOEY swYears-method swYears_EndYear<- swYears-method
#' swYears_EndYear swYears-method swYears_FDOFY<- swYears-method
#' swYears_FDOFY swYears-method swYears_isNorth<- swYears-method
#' swYears_isNorth swYears-method swYears_StartYear<- swYears-method
#' swYears_StartYear swYears-method
#' swReadLines swYears-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swYears", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swYears")
#'
NULL


#' Class \code{"swMonthlyScalingParams"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swMonthlyScalingParams-class
#' @aliases swMonthlyScalingParams-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swMonthlyScalingParams", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swMonthlyScalingParams")
#'
NULL


#' Class \code{"swWeather"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swWeather-class
#' @aliases
#' swWeather_DaysRunningAverage<-,swWeather-method
#' swWeather_DaysRunningAverage,swWeather-method
#' swWeather_FirstYearHistorical<-,swWeather-method
#' swWeather_FirstYearHistorical,swWeather-method
#' swWeather_MonScalingParams<-,swWeather-method
#' swWeather_MonScalingParams,swWeather-method
#' swWeather_pct_SnowDrift<-,swWeather-method
#' swWeather_pct_SnowDrift,swWeather-method
#' swWeather_pct_SnowRunoff<-,swWeather-method
#' swWeather_pct_SnowRunoff,swWeather-method
#' swWeather_UseMarkov<-,swWeather-method
#' swWeather_UseMarkov,swWeather-method
#' swWeather_UseSnow<-,swWeather-method
#' swWeather_UseSnow,swWeather-method
#' swReadLines,swWeather-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swWeather", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swWeather")
#'
NULL


#' Class \code{"swWeatherData"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swWeatherData-class
#' @aliases
#' swReadLines,swWeatherData-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swWeatherData", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swWeatherData")
#'
NULL


#' Class \code{"swCloud"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swCloud-class
#' @aliases
#' swCloud_Humidity<-,swCloud-method
#' swCloud_Humidity,swCloud-method
#' swCloud_SkyCover<-,swCloud-method
#' swCloud_SkyCover,swCloud-method
#' swCloud_SnowDensity<-,swCloud-method
#' swCloud_SnowDensity,swCloud-method
#' swCloud_Transmissivity<-,swCloud-method
#' swCloud_Transmissivity,swCloud-method
#' swCloud_WindSpeed<-,swCloud-method
#' swCloud_WindSpeed,swCloud-method
#' swReadLines,swCloud-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swCloud", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swCloud")
#'
NULL


#' Class \code{"swMarkov"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swMarkov-class
#' @aliases
#' swMarkov_Conv<-,swMarkov-method
#' swMarkov_Conv,swMarkov-method
#' swMarkov_Prob<-,swMarkov-method
#' swMarkov_Prob,swMarkov-method
#' swReadLines,swMarkov-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swMarkov", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swMarkov")
#'
NULL


#' Class \code{"swProd"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swProd-class
#' @aliases
#' swProd_Albedo<-,swProd-method
#' swProd_Albedo,swProd-method
#' swProd_CanopyHeight<-,swProd-method
#' swProd_CanopyHeight,swProd-method
#' swProd_Composition<-,swProd-method
#' swProd_Composition,swProd-method
#' swProd_Cover_stcr<-,swProd-method
#' swProd_Cover_stcr,swProd-method
#' swProd_CritSoilWaterPotential<-,swProd-method
#' swProd_CritSoilWaterPotential,swProd-method
#' swProd_Es_param_limit<-,swProd-method
#' swProd_Es_param_limit,swProd-method
#' swProd_EsTpartitioning_param<-,swProd-method
#' swProd_EsTpartitioning_param,swProd-method
#' swProd_HydrRedstro<-,swProd-method
#' swProd_HydrRedstro,swProd-method
#' swProd_HydrRedstro_use<-,swProd-method
#' swProd_HydrRedstro_use,swProd-method
#' swProd_LitterInterParam<-,swProd-method
#' swProd_LitterInterParam,swProd-method
#' swProd_MonProd_grass<-,swProd-method
#' swProd_MonProd_grass,swProd-method
#' swProd_MonProd_shrub<-,swProd-method
#' swProd_MonProd_shrub,swProd-method
#' swProd_MonProd_tree<-,swProd-method
#' swProd_MonProd_tree,swProd-method
#' swProd_Shade<-,swProd-method
#' swProd_Shade,swProd-method
#' swProd_VegInterParam<-,swProd-method
#' swProd_VegInterParam,swProd-method
#' swReadLines,swProd-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swProd", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swProd")
#'
NULL


#' Class \code{"swSite"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swSite-class
#' @aliases
#' swSite_DrainageCoefficient<-,swSite-method
#' swSite_DrainageCoefficient,swSite-method
#' swSite_EvapCoefficients<-,swSite-method
#' swSite_EvapCoefficients,swSite-method
#' swSite_IntrinsicSiteParams<-,swSite-method
#' swSite_IntrinsicSiteParams,swSite-method
#' swSite_ModelCoefficients<-,swSite-method
#' swSite_ModelCoefficients,swSite-method
#' swSite_ModelFlags<-,swSite-method
#' swSite_ModelFlags,swSite-method
#' swSite_SnowSimulationParams<-,swSite-method
#' swSite_SnowSimulationParams,swSite-method
#' swSite_SoilTemperatureConsts<-,swSite-method
#' swSite_SoilTemperatureConsts,swSite-method
#' swSite_SoilTemperatureFlag<-,swSite-method
#' swSite_SoilTemperatureFlag,swSite-method
#' swSite_SWClimits<-,swSite-method
#' swSite_SWClimits,swSite-method
#' swSite_TranspCoefficients<-,swSite-method
#' swSite_TranspCoefficients,swSite-method
#' swSite_TranspirationRegions<-,swSite-method
#' swSite_TranspirationRegions,swSite-method
#' swReadLines,swSite-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swSite", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swSite")
#'
NULL


#' Class \code{"swSoils"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swSoils-class
#' @aliases
#' swReadLines,swSoils-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swSoils", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swSoils")
#'
NULL


#' Class \code{"swEstab"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swEstab-class
#' @aliases
#' swEstab_useEstab,swEstab-method
#' swEstab_useEstab<-,swEstab-method
#' swReadLines,swEstab-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swEstab", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swEstab")
#'
NULL


#' Class \code{"swEstabSpecies"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swEstabSpecies-class
#' @aliases
#' swReadLines,swEstabSpecies-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swEstabSpecies", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swEstabSpecies")
#'
NULL


#' Class \code{swOUT}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#' @name swOUT-class
#'
#' @slot outputSeparator A character string. Currently, only "\\t" is functional.
#' @slot timeSteps An integer matrix. See details.
#'
#' @details Output can be generated for four different time steps: daily (DY),
#'  weekly (WK), monthly (MO), and yearly (YR) periods.
#'  We have two options to specify time steps:\itemize{
#'    \item The same time step(s) for every output; this option corresponds to specifying
#'        a line with `TIMESTEP ...` in the SOILWAT2 input file `outsetup.in`. The matrix
#'        in slot `timeSteps` should have `SW_OUTNKEYS` rows and `used_SW_OUTNPERIODS`
#'        columns where each row contains identical values.
#'    \item A different time step for each output; however, only one time step per
#'        output variable can be specified. this option corresponds to specifying the
#'        time step in the column `PERIOD` in the SOILWAT2 input file `outsetup.in`. The
#'        matrix in slot `timeSteps` should have `SW_OUTNKEYS` rows and 1 column.
#' }
#'
#' @aliases
#' set_swOUT<-,swOUT-method
#' get_swOUT,swOUT-method
#' swOUT_OutputSeparator<-,swOUT-method
#' swOUT_OutputSeparator,swOUT-method
#' swOUT_TimeStep<-,swOUT-method
#' swOUT_TimeStep,swOUT-method
#' swReadLines,swOUT-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swOUT", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swOUT")
#'
NULL


#' Class \code{"swSWC"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swSWC-class
#' @aliases
#' swSWC_FirstYear<-,swSWC,integer-method
#' swSWC_FirstYear,swSWC-method
#' swSWC_HistoricData<-,swSWC-method
#' swSWC_HistoricData,swSWC-method
#' swSWC_HistoricList<-,swSWC,list-method
#' swSWC_HistoricList,swSWC-method
#' swSWC_Method<-,swSWC,integer-method
#' swSWC_Method,swSWC-method
#' swSWC_prefix<-,swSWC,character-method
#' swSWC_prefix,swSWC-method
#' swSWC_use<-,swSWC,logical-method
#' swSWC_use,swSWC-method
#' swReadLines,swSWC-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swSWC", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swSWC")
#'
NULL


#' Class \code{"swSWC_hist"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swSWC_hist-class
#' @aliases
#' swReadLines,swSWC_hist-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swSWC_hist", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swSWC_hist")
#'
NULL


#' Class \code{"swLog"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swLog-class
#' @aliases
#' swLog_setLine<-,swLog-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swLog", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swInputData}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swLog")
#'
NULL


#' Class \code{"swOutput_KEY"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swOutput_KEY-class
#' @aliases
#' swOutput_KEY_Period<-,swOUT-method
#' swOutput_KEY_Period,swOUT-method
#' swOutput_KEY_TimeStep<-,swOUT-method
#' swOutput_KEY_TimeStep,swOUT-method
#' swOutput_KEY_Columns<-,swOUT-method
#' swOutput_KEY_Columns,swOUT-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swOutput_KEY", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swOutput_KEY")
#'
NULL


#' Class \code{"swOutput"}
#'
#' The generic functions listed work on this and the proper sw Class in the container's slots.
#'
#'
#' @name swOutput-class
#' @aliases
#' swOutput_getKEY<-,swOutput-method
#' swOutput_getKEY,swOutput-method
#'
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swOutput", ...)}.
#' @author Ryan Murphy
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' @references https://github.com/DrylandEcology/rSOILWAT2
#' @keywords classes
#' @examples
#'
#' showClass("swOutput")
#'
NULL
