# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################

print("swContainer")
swInputData <- setClass(Class="swInputData", representation(files="swFiles",years="swYears",weather="swWeather",cloud="swCloud",weatherHistory="list",markov="swMarkov",prod="swProd",site="swSite",soils="swSoils",estab="swEstab",output="swOUT",swc="swSWC",log="swLog"))
setMethod(f="swClear",
		signature="swInputData",
		definition=function(object) {
			object@files=swClear(object@files)
			object@years=swClear(object@years)
			object@cloud=swClear(object@cloud)
			object@weather=swClear(object@weather)
			object@weatherHistory=swClear(object@weatherHistory)
			object@prod=swClear(object@prod)
			object@site=swClear(object@site)
			object@estab=swClear(object@estab)
			object@output=swClear(object@output)
			object@swc=swClear(object@swc)
			object@log=swClear(object@log)
			return(object)
		})
setMethod("get_swFiles", "swInputData", function(object) {return(object@files)})
setMethod("swFiles_ProjDir", "swInputData", function(object) {return(object@files@ProjDir)})
setMethod("swFiles_filesIn", "swInputData", function(object) {return(object@files@InFiles[1])})
setMethod("swFiles_Years", "swInputData", function(object) {return(object@files@InFiles[2])})
setMethod("swFiles_LogFile", "swInputData", function(object) {return(object@files@InFiles[3])})
setMethod("swFiles_SiteParams", "swInputData", function(object) {return(object@files@InFiles[4])})
setMethod("swFiles_Soils", "swInputData", function(object) {return(object@files@InFiles[5])})
setMethod("swFiles_WeatherSetup", "swInputData", function(object) {return(object@files@InFiles[6])})
setMethod("swFiles_MarkovProbs", "swInputData", function(object) {return(object@files@InFiles[7])})
setMethod("swFiles_MarkovCov", "swInputData", function(object) {return(object@files@InFiles[8])})
setMethod("swFiles_Cloud", "swInputData", function(object) {return(object@files@InFiles[9])})
setMethod("swFiles_Prod", "swInputData", function(object) {return(object@files@InFiles[10])})
setMethod("swFiles_Estab", "swInputData", function(object) {return(object@files@InFiles[11])})
setMethod("swFiles_SWCsetup", "swInputData", function(object) {return(object@files@InFiles[12])})
setMethod("swFiles_Output", "swInputData", function(object) {return(object@files@InFiles[13])})
setMethod("swFiles_WeatherPrefix", "swInputData", function(object) {return(object@files@WeatherPrefix)})
setMethod("swFiles_OutputPrefix", "swInputData", function(object) {return(object@files@OutputPrefix)})
setReplaceMethod(f="set_swFiles", signature="swInputData", definition=function(object,value) {object@files <- value; return(object)})
setReplaceMethod(f="swFiles_ProjDir", signature="swInputData", definition=function(object,value) {object@files@ProjDir <- value; return(object)})
setReplaceMethod(f="swFiles_filesIn", signature="swInputData", definition=function(object,value) {object@files@InFiles[1] <- value; return(object)})
setReplaceMethod(f="swFiles_Years", signature="swInputData", definition=function(object,value) {object@files@InFiles[2] <- value; return(object)})
setReplaceMethod(f="swFiles_LogFile", signature="swInputData", definition=function(object,value) {object@files@InFiles[3] <- value; return(object)})
setReplaceMethod(f="swFiles_SiteParams", signature="swInputData", definition=function(object,value) {object@files@InFiles[4] <- value; return(object)})
setReplaceMethod(f="swFiles_Soils", signature="swInputData", definition=function(object,value) {object@files@InFiles[5] <- value; return(object)})
setReplaceMethod(f="swFiles_WeatherSetup", signature="swInputData", definition=function(object,value) {object@files@InFiles[6] <- value; return(object)})
setReplaceMethod(f="swFiles_MarkovProbs", signature="swInputData", definition=function(object,value) {object@files@InFiles[7] <- value; return(object)})
setReplaceMethod(f="swFiles_MarkovCov", signature="swInputData", definition=function(object,value) {object@files@InFiles[8] <- value; return(object)})
setReplaceMethod(f="swFiles_Cloud", signature="swInputData", definition=function(object,value) {object@files@InFiles[9] <- value; return(object)})
setReplaceMethod(f="swFiles_Prod", signature="swInputData", definition=function(object,value) {object@files@InFiles[10] <- value; return(object)})
setReplaceMethod(f="swFiles_Estab", signature="swInputData", definition=function(object,value) {object@files@InFiles[11] <- value; return(object)})
setReplaceMethod(f="swFiles_SWCsetup", signature="swInputData", definition=function(object,value) {object@files@InFiles[12] <- value; return(object)})
setReplaceMethod(f="swFiles_Output", signature="swInputData", definition=function(object,value) {object@files@InFiles[13] <- value; return(object)})
setReplaceMethod(f="swFiles_WeatherPrefix", signature="swInputData", definition=function(object,value) {object@files@WeatherPrefix <- value; return(object)})
setReplaceMethod(f="swFiles_OutputPrefix", signature="swInputData", definition=function(object,value) {object@files@OutputPrefix <- value; return(object)})
##
setMethod("get_swYears", "swInputData", function(object) {return(object@years)})
setMethod("swYears_StartYear", "swInputData", function(object) {return(object@years@StartYear)})
setMethod("swYears_EndYear", "swInputData", function(object) {return(object@years@EndYear)})
setMethod("swYears_FDOFY", "swInputData", function(object) {return(object@years@FDOFY)})
setMethod("swYears_EDOEY", "swInputData", function(object) {return(object@years@EDOEY)})
setMethod("swYears_isNorth", "swInputData", function(object) {return(object@years@isNorth)})
setReplaceMethod(f="set_swYears", signature="swInputData", definition=function(object,value) {object@years <- as.integer(value); return(object)})
setReplaceMethod(f="swYears_StartYear", signature="swInputData", definition=function(object,value) {object@years@StartYear <- as.integer(value); return(object)})
setReplaceMethod(f="swYears_EndYear", signature="swInputData", definition=function(object,value) {object@years@EndYear <- as.integer(value); return(object)})
setReplaceMethod(f="swYears_FDOFY", signature="swInputData", definition=function(object,value) {object@years@FDOFY <- as.integer(value); return(object)})
setReplaceMethod(f="swYears_EDOEY", signature="swInputData", definition=function(object,value) {object@years@EDOEY <- as.integer(value); return(object)})
setReplaceMethod(f="swYears_isNorth", signature="swInputData", definition=function(object,value) {object@years@isNorth <- as.logical(value); return(object)})
##
setMethod("get_swCloud","swInputData",function(object) {return(object@cloud)})

setReplaceMethod(f="set_swCloud",signature="swInputData",function(object,value) {object@cloud <- value; return(object)})
##
setMethod("get_swWeather","swInputData",function(object) {return(object@weather)})
setMethod("swWeather_DaysRunningAverage","swInputData",function(object) {return(object@weather@DaysRunningAverage)})
setMethod("swWeather_FirstYearHistorical","swInputData",function(object) {return(object@weather@FirstYear_Historical)})
setMethod("swWeather_pct_SnowDrift","swInputData",function(object) {return(object@weather@pct_SnowDrift)})
setMethod("swWeather_pct_SnowRunoff","swInputData",function(object) {return(object@weather@pct_SnowRunoff)})
setMethod("swWeather_UseMarkov","swInputData",function(object) {return(object@weather@UseMarkov)})
setMethod("swWeather_UseSnow","swInputData",function(object) {return(object@weather@UseSnow)})
setMethod("swWeather_MonScalingParams","swInputData",function(object) {return(object@weather@MonthlyScalingParams)})

setReplaceMethod(f="set_swWeather",signature="swInputData",function(object,value) {object@weather <- value; return(object)})
setReplaceMethod(f="swWeather_DaysRunningAverage",signature="swInputData",function(object,value) {object@weather@DaysRunningAverage <- as.integer(value); return(object)})
setReplaceMethod(f="swWeather_FirstYearHistorical",signature="swInputData",function(object,value) {object@weather@FirstYear_Historical <- as.integer(value); return(object)})
setReplaceMethod(f="swWeather_pct_SnowDrift",signature="swInputData",function(object,value) {object@weather@pct_SnowDrift <- value; return(object)})
setReplaceMethod(f="swWeather_pct_SnowRunoff",signature="swInputData",function(object,value) {object@weather@pct_SnowRunoff <- value; return(object)})
setReplaceMethod(f="swWeather_UseMarkov",signature="swInputData",function(object,value) {object@weather@UseMarkov <- value; return(object)})
setReplaceMethod(f="swWeather_UseSnow",signature="swInputData",function(object,value) {object@weather@UseSnow <- value; return(object)})
setReplaceMethod(f="swWeather_MonScalingParams",signature="swInputData",function(object,value) {object@weather@MonthlyScalingParams <- value; return(object)})
##
setMethod("get_swCloud","swInputData",function(object) {return(object@cloud)})
setMethod("swCloud_SkyCover","swInputData",function(object) {return(object@cloud@Cloud[1,])})
setMethod("swCloud_WindSpeed","swInputData",function(object) {return(object@cloud@Cloud[2,])})
setMethod("swCloud_Humidity","swInputData",function(object) {return(object@cloud@Cloud[3,])})
setMethod("swCloud_Transmissivity","swInputData",function(object) {return(object@cloud@Cloud[4,])})
setMethod("swCloud_SnowDensity","swInputData",function(object) {return(object@cloud@Cloud[5,])})

setReplaceMethod(f="set_swCloud",signature="swInputData",function(object,value) {object@cloud@cloud <- value; return(object)})
setReplaceMethod(f="swCloud_SkyCover",signature="swInputData",function(object,value) {object@cloud@Cloud[1,] <- value; return(object)})
setReplaceMethod(f="swCloud_WindSpeed",signature="swInputData",function(object,value) {object@cloud@Cloud[2,] <- value; return(object)})
setReplaceMethod(f="swCloud_Humidity",signature="swInputData",function(object,value) {object@cloud@Cloud[3,] <- value; return(object)})
setReplaceMethod(f="swCloud_Transmissivity",signature="swInputData",function(object,value) {object@cloud@Cloud[4,] <- value; return(object)})
setReplaceMethod(f="swCloud_SnowDensity",signature="swInputData",function(object,value) {object@cloud@Cloud[5,] <- value; return(object)})
##
setMethod("get_Markov","swInputData",function(object) {return(object@markov)})
setMethod("swMarkov_Prob","swInputData",function(object) {return(object@markov@Prob)})
setMethod("swMarkov_Conv","swInputData",function(object) {return(object@markov@Conv)})

setReplaceMethod(f="set_Markov",signature="swInputData",function(object,value) {object@markov <- value; return(object)})
setReplaceMethod(f="swMarkov_Prob",signature="swInputData",function(object,value) {object@markov@Prob <- value; return(object)})
setReplaceMethod(f="swMarkov_Conv",signature="swInputData",function(object,value) {object@markov@Conv <- value; return(object)})
##
setMethod("get_WeatherHistory","swInputData",function(object) {return(object@weatherHistory)})
setMethod("get_swWeatherData","swInputData",function(object,year) {
			index<-which(names(object@weatherHistory) == as.character(year))
			if(length(index) != 1) {
				stop("Index has wrong length.")
			}
			if(object@weatherHistory[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object@weatherHistory[[index]])
		})
setMethod("get_swWeatherData","list",function(object,year) {
			index<-which(names(object) == as.character(year))
			if(length(index) != 1) {
				stop("Index has wrong length.")
			}
			if(object[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object[[index]])
		})

setReplaceMethod(f="set_WeatherHistory",signature=c(object="swInputData",value="list"),function(object,value) {object@weatherHistory <- value; return(object)})
setReplaceMethod(f="set_swWeatherData",signature=c(object="swInputData",value="swWeatherData"),function(object,value) {
			index<-which(names(object@weatherHistory) == as.character(value@year))
			if(length(index) == 0) {
				object@weatherHistory[[length(object@weatherHistory)+1]] <- value
				years <- unlist(lapply(object@weatherHistory, function(x) {return(x@year)}))
				object@weatherHistory <- object@weatherHistory[order(years)]
				names(object@weatherHistory)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("Weather data is Missing")
			} else if(length(index) == 1){
				object@weatherHistory[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})
setReplaceMethod(f="set_swWeatherData",signature=c(object="list",value="swWeatherData"),function(object,value) {
			index<-which(names(object) == as.character(value@year))
			if(length(index) == 0) {
				object[[length(object)+1]] <- value
				years <- unlist(lapply(object, function(x) {return(x@year)}))
				object <- object[order(years)]
				names(object)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("Weather data is Missing")
			} else if(length(index) == 1){
				object[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})
##
setMethod("get_swProd", "swInputData", function(object) {return(object@prod)})
setMethod("swProd_Composition", "swInputData", function(object) {return(object@prod@Composition)})
setMethod("swProd_Albedo", "swInputData", function(object) {return(object@prod@Albedo)})
setMethod("swProd_Cover_stcr", "swInputData", function(object) {return(object@prod@Cover_stcr)})
setMethod("swProd_CanopyHeight", "swInputData", function(object) {return(object@prod@CanopyHeight)})
setMethod("swProd_VegInterParam", "swInputData", function(object) {return(object@prod@VegetationInterceptionParameters)})
setMethod("swProd_LitterInterParam", "swInputData", function(object) {return(object@prod@LitterInterceptionParameters)})
setMethod("swProd_EsTpartitioning_param", "swInputData", function(object) {return(object@prod@EsTpartitioning_param)})
setMethod("swProd_Es_param_limit", "swInputData", function(object) {return(object@prod@Es_param_limit)})
setMethod("swProd_Shade", "swInputData", function(object) {return(object@prod@Shade)})
setMethod("swProd_HydrRedstro_use", "swInputData", function(object) {return(object@prod@HydraulicRedistribution_use)})
setMethod("swProd_HydrRedstro", "swInputData", function(object) {return(object@prod@HydraulicRedistribution)})
setMethod("swProd_CritSoilWaterPotential", "swInputData", function(object) {return(object@prod@CriticalSoilWaterPotential)})
setMethod("swProd_MonProd_grass", "swInputData", function(object) {return(object@prod@MonthlyProductionValues_grass)})
setMethod("swProd_MonProd_shrub", "swInputData", function(object) {return(object@prod@MonthlyProductionValues_shrub)})
setMethod("swProd_MonProd_tree", "swInputData", function(object) {return(object@prod@MonthlyProductionValues_tree)})

setReplaceMethod(f="set_swProd", signature="swInputData", definition=function(object,value) {object@prod <- value; return(object)})
setReplaceMethod(f="swProd_Composition", signature="swInputData", definition=function(object,value) {object@prod@Composition <- value; return(object)})
setReplaceMethod(f="swProd_Albedo", signature="swInputData", definition=function(object,value) {object@prod@Albedo <- value; return(object)})
setReplaceMethod(f="swProd_Cover_stcr", signature="swInputData", definition=function(object,value) {object@prod@Cover_stcr <- value; return(object)})
setReplaceMethod(f="swProd_CanopyHeight", signature="swInputData", definition=function(object,value) {object@prod@CanopyHeight <- value; return(object)})
setReplaceMethod(f="swProd_VegInterParam", signature="swInputData", definition=function(object,value) {object@prod@VegetationInterceptionParameters <- value; return(object)})
setReplaceMethod(f="swProd_LitterInterParam", signature="swInputData", definition=function(object,value) {object@prod@LitterInterceptionParameters <- value; return(object)})
setReplaceMethod(f="swProd_EsTpartitioning_param", signature="swInputData", definition=function(object,value) {object@prod@EsTpartitioning_param <- value; return(object)})
setReplaceMethod(f="swProd_Es_param_limit", signature="swInputData", definition=function(object,value) {object@prod@Es_param_limit <- value; return(object)})
setReplaceMethod(f="swProd_Shade", signature="swInputData", definition=function(object,value) {object@prod@Shade <- value; return(object)})
setReplaceMethod(f="swProd_HydrRedstro_use", signature="swInputData", definition=function(object,value) {object@prod@HydraulicRedistribution_use <- value; return(object)})
setReplaceMethod(f="swProd_HydrRedstro", signature="swInputData", definition=function(object,value) {object@prod@HydraulicRedistribution <- value; return(object)})
setReplaceMethod(f="swProd_CritSoilWaterPotential", signature="swInputData", definition=function(object,value) {object@prod@CriticalSoilWaterPotential <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_grass", signature="swInputData", definition=function(object,value) {object@prod@MonthlyProductionValues_grass <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_shrub", signature="swInputData", definition=function(object,value) {object@prod@MonthlyProductionValues_shrub <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_tree", signature="swInputData", definition=function(object,value) {object@prod@MonthlyProductionValues_tree <- value; return(object)})
##
setMethod("get_swSite", "swInputData", function(object) {return(object@site)})
setMethod("swSite_SWClimits", "swInputData", function(object) {return(object@site@SWClimits)})
setMethod("swSite_ModelFlags", "swInputData", function(object) {return(object@site@ModelFlags)})
setMethod("swSite_ModelCoefficients", "swInputData", function(object) {return(object@site@ModelCoefficients)})
setMethod("swSite_SnowSimulationParams", "swInputData", function(object) {return(object@site@SnowSimulationParameters)})
setMethod("swSite_DrainageCoefficient", "swInputData", function(object) {return(object@site@DrainageCoefficient)})
setMethod("swSite_EvapCoefficients", "swInputData", function(object) {return(object@site@EvaporationCoefficients)})
setMethod("swSite_TranspCoefficients", "swInputData", function(object) {return(object@site@TranspirationCoefficients)})
setMethod("swSite_IntrinsicSiteParams", "swInputData", function(object) {return(object@site@IntrinsicSiteParams)})
setMethod("swSite_SoilTemperatureFlag", "swInputData", function(object) {return(object@site@SoilTemperatureFlag)})
setMethod("swSite_SoilTemperatureConsts", "swInputData", function(object) {return(object@site@SoilTemperatureConstants)})
setMethod("swSite_TranspirationRegions", "swInputData", function(object) {return(object@site@TranspirationRegions)})

setReplaceMethod(f="set_swSite", signature="swInputData", definition=function(object,value) {object@site <- value; return(object)})
setReplaceMethod(f="swSite_SWClimits", signature="swInputData", definition=function(object,value) {object@site@SWClimits <- value; return(object)})
setReplaceMethod(f="swSite_ModelFlags", signature="swInputData", definition=function(object,value) {object@site@ModelFlags <- value; return(object)})
setReplaceMethod(f="swSite_ModelCoefficients", signature="swInputData", definition=function(object,value) {object@site@ModelCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_SnowSimulationParams", signature="swInputData", definition=function(object,value) {object@site@SnowSimulationParameters <- value; return(object)})
setReplaceMethod(f="swSite_DrainageCoefficient", signature="swInputData", definition=function(object,value) {object@site@DrainageCoefficient <- value; return(object)})
setReplaceMethod(f="swSite_EvapCoefficients", signature="swInputData", definition=function(object,value) {object@site@EvaporationCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_TranspCoefficients", signature="swInputData", definition=function(object,value) {object@site@TranspirationCoefficients <- value; return(object)})
setReplaceMethod(f="swSite_IntrinsicSiteParams", signature="swInputData", definition=function(object,value) {object@site@IntrinsicSiteParams <- value; return(object)})
setReplaceMethod(f="swSite_SoilTemperatureFlag", signature="swInputData", definition=function(object,value) {object@site@SoilTemperatureFlag <- value; return(object)})
setReplaceMethod(f="swSite_SoilTemperatureConsts", signature="swInputData", definition=function(object,value) {object@site@SoilTemperatureConstants <- value; return(object)})
setReplaceMethod(f="swSite_TranspirationRegions", signature="swInputData", definition=function(object,value) {object@site@TranspirationRegions <- value*1; return(object)})
##
setMethod("get_swSoils", "swInputData", function(object) {return(object@soils)})
setMethod("swSoils_Layers", "swInputData", function(object) {return(object@soils@Layers)})
setMethod("swSoils_Layers", "swSoils", function(object) {return(object@Layers)})

setReplaceMethod(f="set_swSoils", signature=c(object="swInputData",value="swSoils"), definition=function(object,value) {object@soils <- value; return(object)})
setReplaceMethod(f="swSoils_Layers", signature=c(object="swInputData",value="matrix"), definition=function(object,value) {object@soils@Layers <- value; return(object)})
##
setMethod("get_swSWC", "swInputData", definition=function(object) {return(object@swc)})
setMethod("swSWC_use","swInputData",function(object) {return(object@swc@UseSWCHistoricData)})
setMethod("swSWC_prefix","swInputData",function(object) {return(object@swc@DataFilePrefix)})
setMethod("swSWC_FirstYear","swInputData",function(object) {return(object@swc@FirstYear)})
setMethod("swSWC_Method","swInputData",function(object) {return(object@swc@Method)})
setMethod("swSWC_HistoricList","swInputData",function(object) {return(object@swc@History)})
setMethod("swSWC_HistoricData","swInputData",function(object,year) {
			index<-which(names(object@swc@History) == as.character(year))
			if(length(index) != 1) {
				print("swc historic data Index has wrong length.")
				return(NULL)
			}
			if(object@swc@History[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object@swc@History[[index]])
		})

setReplaceMethod(f="set_swSWC",signature=c(object="swInputData", swSWC="swSWC"), function(object, swSWC) {object@swc <- swSWC; return(object)})
setReplaceMethod(f="swSWC_use",signature=c(object="swInputData",UseSWC="logical"),function(object,UseSWC) {object@swc@UseSWCHistoricData <- UseSWC; return(object)})
setReplaceMethod(f="swSWC_prefix",signature=c(object="swInputData",SWCprefix="character"),function(object,SWCprefix) {object@swc@UseSWCHistoricData <- SWCprefix; return(object)})
setReplaceMethod(f="swSWC_FirstYear",signature=c(object="swInputData",SWCfirstYear="integer"),function(object,SWCfirstYear) {object@swc@UseSWCHistoricData <- SWCfirstYear; return(object)})
setReplaceMethod(f="swSWC_Method",signature=c(object="swInputData",SWCmethod="integer"),function(object,SWCmethod) {object@swc@UseSWCHistoricData <- SWCmethod; return(object)})
setReplaceMethod(f="swSWC_HistoricList",signature=c(object="swInputData",swSWC_HistoricList="list"),function(object,swSWC_HistoricList) {object@swc@UseSWCHistoricData <- swSWC_HistoricList; return(object)})
setReplaceMethod(f="swSWC_HistoricData",signature=c(object="swInputData",swSWCdata="swSWC_hist"),function(object,swSWCdata) {
			index<-which(names(object@swc@History) == as.character(swSWCdata@year))
			if(length(index) == 0) {
				object@swc@History[[length(object@swc@History)+1]] <- swSWCdata
				years <- unlist(lapply(object@swc@History, function(x) {return(x@year)}))
				object@swc@History <- object@swc@History[order(years)]
				names(object@swc@History)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("SWC data is Missing")
			} else if(length(index) == 1) {
				object@swc@History[[index]] <- swSWCdata
			} else {
				print("To many index. Not set")
			}
			return(object)
		})
##
setMethod("get_swOUT", "swInputData", function(object) {return(object@output)})
setMethod("swOUT_TimeStep","swInputData",function(object) {return(swOUT_TimeStep(get_swOUT(object)))})
setMethod("swOUT_OutputSeparator","swInputData",function(object) {return(swOUT_OutputSeparator(get_swOUT(object)))})

setReplaceMethod(f="set_swOUT",signature="swInputData",function(object,value) {object@output <- value; return(object)})
setReplaceMethod(f="swOUT_TimeStep",signature="swInputData",function(object,value) { swOUT_TimeStep(object@output) <- value; return(object)})
setReplaceMethod(f="swOUT_OutputSeparator",signature="swInputData",function(object,value) { swOUT_OutputSeparator(object@output) <- value; return(object)})
##
setReplaceMethod(f="swLog_setLine", signature="swInputData", function(object, value) {
			if(object@log@UsedLines <= object@log@MaxLines) {
				object@log@LogData[object@log@UsedLines] <- value
				object@log@UsedLines <- as.integer(object@log@UsedLines + 1)
			}
			return(object)
		})
##

setMethod(f="swWriteLines",signature=c(object="swInputData",file="character"), definition=function(object,file) { return(swWriteLines(object@files,file)) })
setMethod(f="swReadLines", signature=c(object="swInputData",file="character"), definition=function(object,file) { return(swReadLines(object@files,file)) })
