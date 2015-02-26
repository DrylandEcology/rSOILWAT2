# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################

print("swWeather")

#######################Monthly Scaling Params#################################

swMonthlyScalingParams<-setClass("swMonthlyScalingParams",representation(MonthlyScalingParams="matrix"),prototype=prototype(MonthlyScalingParams=matrix(data=c(rep(1,12),rep(0,12*3),rep(1,12),rep(0,12),rep(1,12)),nrow=12,ncol=7,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("PPT","MaxT","MinT","SkyCover","Wind","rH","Transmissivity")))))
swMonthlyScalingParams_validity<-function(object){
	if(dim(object@MonthlyScalingParams)[1]!=12)
		return("@MonthlyScalingParams needs 12 rows.")
	if(dim(object@MonthlyScalingParams)[2]!=7)
		return("@MonthlyScalingParams needs 7 columns.")
	TRUE
}
setValidity("swMonthlyScalingParams",swMonthlyScalingParams_validity)
setMethod(f="initialize",signature="swMonthlyScalingParams",definition=function(.Object,MonthlyScalingParams=NULL){
			if(is.null(MonthlyScalingParams))
				MonthlyScalingParams<-matrix(data=c(rep(1,12),rep(0,12*3),rep(1,12),rep(0,12),rep(1,12)),nrow=12,ncol=7)
			colnames(MonthlyScalingParams)<-c("PPT","MaxT","MinT","SkyCover","Wind","rH","Transmissivity")
			rownames(MonthlyScalingParams)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
			.Object@MonthlyScalingParams<-MonthlyScalingParams
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swMonthlyScalingParams",
		definition=function(object) {
			object@MonthlyScalingParams=matrix(data=c(rep(1,12),rep(0,12*3),rep(1,12),rep(0,12),rep(1,12)),nrow=12,ncol=7)
			colnames(object@MonthlyScalingParams)<-c("PPT","MaxT","MinT","SkyCover","Wind","rH","Transmissivity")
			rownames(object@MonthlyScalingParams)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
			return(object)
		})

#####################WEATHERSETUP.IN###################################

swWeather <- setClass("swWeather",representation(UseSnow="logical",pct_SnowDrift="numeric",pct_SnowRunoff="numeric",use_Markov="logical",
				FirstYear_Historical="integer",DaysRunningAverage="integer"),
		prototype=prototype(UseSnow=TRUE,pct_SnowDrift=0.0,pct_SnowRunoff=0.0,use_Markov=FALSE,FirstYear_Historical=as.integer(1982),DaysRunningAverage=as.integer(5)),
		contains=c("swMonthlyScalingParams"))

swWeather_validity<-function(object){
	if(length(object@UseSnow)!=1)
		return("@UseSnow needs to be of length 1.")
	if(length(object@pct_SnowDrift)!=1)
		return("@pct_SnowDrift needs to be of length 1.")
	if(length(object@pct_SnowRunoff)!=1)
		return("@pct_SnowRunoff needs to be of length 1.")
	if(length(object@use_Markov)!=1)
		return("@use_Markov needs to be of length 1.")
	if(length(object@FirstYear_Historical)!=1)
		return("@FirstYear_Historical needs to be of length 1.")
	if(length(object@DaysRunningAverage)!=1)
		return("@DaysRunningAverage needs to be of length 1.")
	if(length(object@weatherYearsIn)<1)
		return("@Weather needs to at least length of 1.")
}
setValidity("swWeather",swWeather_validity)
setMethod(f="initialize",signature="swWeather",definition=function(.Object,UseSnow=TRUE,pct_SnowDrift=0.0,pct_SnowRunoff=0.0,use_Markov=FALSE,FirstYear_Historical=as.integer(1982),DaysRunningAverage=as.integer(5)){#weatherYearsIn=NULL
			#if(is.null(weatherYearsIn))
				#weatherYearsIn<-list('1982'=swWeatherData(data=year1982,year=as.integer(1982)),
				#		'1983'=swWeatherData(data=year1983,year=as.integer(1983)),
				#		'1984'=swWeatherData(data=year1984,year=as.integer(1984)),
				#		'1985'=swWeatherData(data=year1985,year=as.integer(1985)),
				#		'1986'=swWeatherData(data=year1986,year=as.integer(1986)))
			.Object@UseSnow=UseSnow
			.Object@pct_SnowDrift=pct_SnowDrift
			.Object@pct_SnowRunoff=pct_SnowRunoff
			.Object@use_Markov=use_Markov
			.Object@FirstYear_Historical=FirstYear_Historical
			.Object@DaysRunningAverage=DaysRunningAverage
			#.Object@weatherYearsIn=weatherYearsIn
			#.Object@Cloud=swCloud()
			#.Object@MonthlyScalingParams=swMonthlyScalingParams()
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swWeather",
		definition=function(object) {
			object@UseSnow=logical(1)
			object@pct_SnowDrift=numeric(1)
			object@pct_SnowRunoff=numeric(1)
			object@use_Markov=logical(1)
			object@FirstYear_Historical=integer(1)
			object@DaysRunningAverage=integer(1)
			#object@weatherYearsIn=list(0)
			#object@markov = swClear(object@markov)
			object@MonthlyScalingParams=matrix(data=c(rep(1,12),rep(0,12*3),rep(1,12),rep(0,12),rep(1,12)),nrow=12,ncol=7)
			colnames(object@MonthlyScalingParams)<-c("PPT","MaxT","MinT","SkyCover","Wind","rH","Transmissivity")
			rownames(object@MonthlyScalingParams)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
			#object@Cloud=matrix(data=NA,nrow=5,ncol=12,byrow=T)
			#colnames(object@Cloud)<-c("January","February","March","April","May","June","July","August","September","October","November","December")		
			#rownames(object@Cloud)<-c("SkyCoverPCT","WindSpeed_m/s","HumidityPCT","Transmissivity","SnowDensity_kg/m^3")
			return(object)
		})
setMethod("swWeather_DaysRunningAverage","swWeather",function(object) {return(object@DaysRunningAverage)})
setMethod("swWeather_FirstYearHistorical","swWeather",function(object) {return(object@FirstYear_Historical)})
setMethod("swWeather_pct_SnowDrift","swWeather",function(object) {return(object@pct_SnowDrift)})
setMethod("swWeather_pct_SnowRunoff","swWeather",function(object) {return(object@pct_SnowRunoff)})
setMethod("swWeather_UseMarkov","swWeather",function(object) {return(object@UseMarkov)})
setMethod("swWeather_UseSnow","swWeather",function(object) {return(object@UseSnow)})
setMethod("swWeather_MonScalingParams","swWeather",function(object) {return(object@MonthlyScalingParams)})

setReplaceMethod(f="swWeather_DaysRunningAverage",signature="swWeather",function(object,value) {object@DaysRunningAverage <- value; return(object)})
setReplaceMethod(f="swWeather_FirstYearHistorical",signature="swWeather",function(object,value) {object@FirstYear_Historical <- value; return(object)})
setReplaceMethod(f="swWeather_pct_SnowDrift",signature="swWeather",function(object,value) {object@pct_SnowDrift <- value; return(object)})
setReplaceMethod(f="swWeather_pct_SnowRunoff",signature="swWeather",function(object,value) {object@pct_SnowRunoff <- value; return(object)})
setReplaceMethod(f="swWeather_UseMarkov",signature="swWeather",function(object,value) {object@UseMarkov <- value; return(object)})
setReplaceMethod(f="swWeather_UseSnow",signature="swWeather",function(object,value) {object@UseSnow <- value; return(object)})
setReplaceMethod(f="swWeather_MonScalingParams",signature="swWeather",function(object,value) {object@MonthlyScalingParams <- value; return(object)})

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
