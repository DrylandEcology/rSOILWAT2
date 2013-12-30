print("swFunctions")
adjustLayersDepth <- function(layers_depth, d) return(round(layers_depth[1:d])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- function(layers_depth) return(diff(c(0, layers_depth)))
setLayerSequence <- function(d) return(1:d)


sw_dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2){
	#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
	
	Month7th_MinTemp_C <- aggregate(dailyTempMin[simTime2$month_ForEachUsedDay_NSadj == 7], by=list(simTime2$year_ForEachUsedDay_NSadj[simTime2$month_ForEachUsedDay_NSadj == 7]), FUN=min)[, 2]
	LengthFreezeFreeGrowingPeriod_Days <- aggregate(dailyTempMin, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) max(rle(x > 0)$lengths, na.rm=TRUE))[, 2]
	DegreeDaysAbove65F_DaysC <- aggregate(dailyTempMean, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) sum(ifelse((temp <- x - ((65-32) * 5/9)) > 0, temp, 0)))[, 2]
	
	nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
	res <- c(apply(temp <- cbind(Month7th_MinTemp_C[nyrs], LengthFreezeFreeGrowingPeriod_Days[nyrs], DegreeDaysAbove65F_DaysC[nyrs]), MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
	names(res) <- c(temp <- c("Month7th_NSadj_MinTemp_C", "LengthFreezeFreeGrowingPeriod_NSadj_Days", "DegreeDaysAbove65F_NSadj_DaysC"), paste(temp, ".sd", sep=""))
	
	return(res)
}

sw_SiteClimate_Ambient <- function(weatherList, year.start, year.end, do.C4vars=FALSE, simTime2=NULL) {
	sw.weather.suffices <- as.numeric(names(weatherList))
	itemp <- year.start <= sw.weather.suffices & year.end >= sw.weather.suffices
	years <- sw.weather.suffices[itemp]
	
	temp <- ppt <- rep(0, times=12)
	mat <- NULL
	if(do.C4vars){
		dailyTempMin <- NULL
		dailyTempMean <- NULL
	}
	if((no.yrs <- length(years)) > 0) for(y in 1:no.yrs){
			temp.dailyTempMean <- apply(get_swWeatherData(weatherList, years[y])@data[, 2:3], 1, mean)
			mat <- c(mat, mean(temp.dailyTempMean))
			if(do.C4vars){
				dailyTempMin <- c(dailyTempMin, get_swWeatherData(weatherList, years[y])@data[, 3])
				dailyTempMean <- c(dailyTempMean, temp.dailyTempMean)
			}
			month_forEachDoy <- as.POSIXlt(seq(from=as.POSIXlt(paste(years[y], "-01-01", sep="")), to=as.POSIXlt(paste(years[y], "-12-31", sep="")), by="1 day"))$mon + 1
			temp <- temp + aggregate(temp.dailyTempMean, by=list(month_forEachDoy), FUN=mean)[, 2]
			ppt <- ppt + aggregate(get_swWeatherData(weatherList, years[y])@data[, 4], by=list(month_forEachDoy), FUN=sum)[, 2]
		}
	temp <- temp / no.yrs
	ppt <- ppt / no.yrs
	
	res <- list(meanMonthlyTempC=temp, meanMonthlyPPTcm=ppt, MAP_cm=sum(ppt), MAT_C=mean(mat))
	
	if(do.C4vars){
		res$dailyTempMin <- dailyTempMin
		res$dailyTempMean <- dailyTempMean
		res$dailyC4vars <- sw_dailyC4_TempVar(dailyTempMin, dailyTempMean, simTime2)
	}
	return(res)
}
onGetSiteId <- function(con, lat=NULL, long=NULL, weatherDirName=NULL) {
	lat<-as.numeric(lat)
	long<-as.numeric(long)
	if(!is.null(weatherDirName) & is.character(weatherDirName)) {
		file <- sub("Weather_NCEPCFSR_", "", weatherDirName)
		
		indexs <- gregexpr("-", file)[[1]]
		if(length(indexs) == 1)
			Cord <- (c(substr(file, 1, indexs-1) ,substring(file, indexs+1)))
		if(length(indexs) == 2)
			if(indexs[1] == 1) {
				Cord <- (c(substr(file, 1, indexs[2]-1) ,substring(file, indexs[2]+1)))
			} else {
				Cord <- (c(substr(file, 1, indexs[1]-1) ,substring(file, indexs[1]+1)))
			}
		if(length(indexs) == 3)
			Cord <- (c(substr(file, 1, indexs[2]-1) ,substring(file, indexs[2]+1)))
		
		Cord <- as.numeric(unlist(Cord))
		lat=Cord[1]
		long=Cord[2]
	}
	if(!is.null(lat) & !is.null(long) & !is.na(lat) & !is.na(long) & length(lat) == 1 & length(long) == 1) {
		SQL <- paste("SELECT Site_id FROM sites WHERE Latitude=",lat," AND Longitude=",long,";",sep="")
		site <- dbGetQuery(con,SQL)
		Site_id <- as.integer(site)
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		return(Site_id)
	} else {
		print("Could not obtain Site Id.")
		return(NULL)
	}
}
onGetWeatherData_folders <- function(LookupWeatherFolder=NULL, weatherDirName=NULL,filebasename=NULL,startYear=NULL,endYear=NULL) {
	if(is.null(LookupWeatherFolder) | is.null(weatherDirName) | is.null(filebasename))
		stop("Need LookupWeatherFolder and weatherDirName information to get weather data")
	useYears<-FALSE
	useStart<-FALSE
	useEnd  <-FALSE
	weatherDataFiles<-tryCatch(list.files(path=file.path(LookupWeatherFolder,weatherDirName),pattern=filebasename), warning=function(w) {stop("Path to weather data bad or filebasename not correct.")})
	weatherDataYears <- as.integer(na.exclude(gsub(filebasename,NA,unlist(strsplit(x=basename(weatherDataFiles),split=".",fixed=TRUE)))))
	if(!is.null(startYear) | !is.null(endYear)) {
		startYear <- as.integer(startYear)
		if(!is.na(startYear)) useStart<-TRUE
		endYear <- as.integer(endYear)
		if(!is.na(endYear)) useEnd<-TRUE
		if(useStart | useEnd) useYears<-TRUE
		if(useStart & useEnd) {
			if(startYear >= endYear | startYear<0 | endYear<0)
				stop("Wrong start or end year")
		}
	}
	if(useYears) {
		if(useStart & useEnd) {
			index <- which(weatherDataYears >= startYear & weatherDataYears <= endYear)
		} else if(useStart) {
			index <- which(weatherDataYears >= startYear)
		} else if(useEnd) {
			index <- which(weatherDataYears <= endYear)
		}
	} else {
		index <- 1:length(weatherDataYears)
	}
	weathDataList <- list()
	j <- 1
	for(i in index) {
		weathDataList[[j]]<-swReadLines(new("swWeatherData",year=weatherDataYears[i]),file.path(LookupWeatherFolder,weatherDirName,weatherDataFiles[i]))
		j <- j+1
	}
	names(weathDataList)<-as.character(weatherDataYears[index])
	return(weathDataList)
}
onGetWeatherData_database<- function(con, Site_id=NULL,lat=NULL,long=NULL,weatherDirName=NULL,startYear=NULL,endYear=NULL) {
	require(RSQLite)
	if(is.null(Site_id) && is.null(weatherDirName) && is.null(lat) && is.null(long))
		stop("No way to locate weather data from input")
	useYears<-FALSE
	useStart<-FALSE
	useEnd  <-FALSE
	if(!is.null(startYear) | !is.null(endYear)) {#See if we should narrow the start end year range
		startYear <- as.integer(startYear)
		if(!is.na(startYear)) useStart<-TRUE
		endYear <- as.integer(endYear)
		if(!is.na(endYear)) useEnd<-TRUE
		if(useStart | useEnd) useYears<-TRUE
		if(useStart & useEnd) {
			if(startYear >= endYear | startYear<0 | endYear<0)
				stop("Wrong start or end year")
		}
	}
	Site_id<-as.integer(Site_id)
	if(length(Site_id) == 0) {
		Site_id <- onGetSiteId(con, lat,long,weatherDirName)
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		SQL = paste("SELECT Year,DOY,Tmax_C,Tmin_C,PPT_cm FROM weatherdata WHERE Site_id=",Site_id,sep="")
	} else {
		stop("Site_id not obtained.")
	}
	
	if(useYears) {
		SQL <- paste(SQL," AND",sep="")
		
		if(useStart && useEnd) {
			SQL <- paste(SQL," Year BETWEEN ",startYear," AND ",endYear, sep="")
		} else if(useStart) {
			SQL <- paste(SQL," Year >=",startYear,sep="")
		} else if(useEnd) {
			SQL <- paste(SQL," Year <=",endYear,sep="")
		}
	}
	SQL <- paste(SQL,";",sep="")
	wData <- dbGetQuery(con,SQL)
	years <- unique(wData[,1])
	weathDataList <- list()
	for(i in 1:length(years)) {
		weathDataList[[i]]<-new("swWeatherData", data=data.matrix(wData[wData[,1]==years[i],2:5]),year=years[i])
	}
	names(weathDataList)<-as.character(years)
	return(weathDataList)
}
readCharacter <- function(text, showWarnings=FALSE) {
	temp <- strsplit(x=text,split="\t")[[1]][1]
	temp <- unlist(strsplit(x=temp,split=" "))[1]
	return(temp)
}
readInteger <- function(text,showWarnings=FALSE) {
	temp <- suppressWarnings(as.integer(strsplit(x=text,split="\t")[[1]][1]))
	if(is.na(temp)) {
		if(showWarnings) print(paste("Line: ",text,sep=""))
		if(showWarnings) print("Not formatted with \t. Going to try [space].")
		temp <- suppressWarnings(as.integer(strsplit(x=text,split=" ")[[1]][1]))
		if(is.na(temp)) {
			stop("Bad Line. Or Bad line numbers.")
		}
	}
	return(temp)
}
readLogical <- function(text,showWarnings=FALSE) {
	temp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split="\t")[[1]][1])))
	if(is.na(temp)) {
		if(showWarnings) print(paste("Line: ",text,sep=""))
		if(showWarnings) print("Not formatted with \t. Going to try [space].")
		temp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split=" ")[[1]][1])))
		if(is.na(temp)) {
			stop("Bad Line. Or Bad line numbers.")
		}
	}
	return(temp)
}
readNumeric <- function(text,showWarnings=FALSE) {
	temp <- suppressWarnings(as.numeric(strsplit(x=text,split="\t")[[1]][1]))
	if(is.na(temp)) {
		if(showWarnings) print(paste("Line: ",text,sep=""))
		if(showWarnings) print("Not formatted with \t. Going to try [space].")
		temp <- suppressWarnings(as.numeric(strsplit(x=text,split=" ")[[1]][1]))
		if(is.na(temp)) {
			stop("Bad Line. Or Bad line numbers.")
		}
	}
	return(temp)
}
readNumerics <- function(text,expectedArgs,showWarnings=FALSE) {
	temp <- strsplit(x=text,split="\t")[[1]]
	temp <- temp[temp != ""] #get rid of extra spaces
	if(length(temp) > expectedArgs) temp <- temp[1:expectedArgs] #get rid of comment?
	temp <- suppressWarnings(as.numeric(temp))
	if(any(is.na(temp))) {
		if(showWarnings & any(is.na(temp))) print(paste("Line: ",text,sep=""))
		if(showWarnings & any(is.na(temp))) print("Not formatted with \t. Going to try [space].")
		temp <- strsplit(x=text,split="\t")[[1]][1] #remove comment
		temp <- strsplit(x=temp,split=" ")[[1]]
		temp <- temp[temp!=""] #remove extra spaces
		temp <- suppressWarnings(as.numeric(temp[1:expectedArgs]))
		if(any(is.na(temp))) {
			#last try. tried set by \t then by space. Now try both
			temp <- strsplit(x=text,split=" ",fixed=T)[[1]]
			temp <- unlist(strsplit(x=temp,split="\t",fixed=T))
			temp <- temp[temp!=""] #remove extra spaces
			temp <- suppressWarnings(as.numeric(temp[1:expectedArgs]))
			if(any(is.na(temp))) stop("Bad Line. Or Bad line numbers.")
		}
	}
	if(length(temp) != expectedArgs) {
		if(showWarnings) print(paste("Line: ",text,sep=""))
			stop(paste("Expected ",expectedArgs," Got ",length(temp),sep=""))
	}
	return(temp)
}


f.digits <- 3
tolerance <- 1.1*10^-f.digits
cut0Inf <- function(x) {x[x < 0] <- NA; return(x)}
finite01 <- function(x) {x[x < 0 | is.na(x)] <- 0; x[x > 1] <- 1; return(x)}

PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 <- function(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
		use_Annuals_Fraction,Annuals_Fraction,
		use_C4_Fraction,C4_Fraction,
		use_C3_Fraction,C3_Fraction,
		use_Shrubs_Fraction,Shrubs_Fraction) {
	#Get the user specified fractions, if column is false set to NA
	tree.fraction <- 0 #option 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996' doesn't estimate tree cover, i.e., assumed to be == 0
	AnnC4C3ShrubFraction <- rep(NA, 4)
	if(use_Annuals_Fraction){
		AnnC4C3ShrubFraction[1] <- finite01(Annuals_Fraction)
	} else {
		AnnC4C3ShrubFraction[1] <- 0 #Annuals can not be NA
	}
	if(use_C4_Fraction)
		AnnC4C3ShrubFraction[2] <- C4_Fraction
	if(use_C3_Fraction)
		AnnC4C3ShrubFraction[3] <- C3_Fraction
	if(use_Shrubs_Fraction)
		AnnC4C3ShrubFraction[4] <- Shrubs_Fraction
	AnnC4C3ShrubFraction <- cut0Inf(AnnC4C3ShrubFraction) #treat negatives as if NA
	TotalFraction <- sum(AnnC4C3ShrubFraction, na.rm=TRUE)
	
	#Decide if all fractions are sufficiently defined or if they need to be calculated based on climate variables
	if(!isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) && TotalFraction < 1 && sum(is.na(AnnC4C3ShrubFraction)) == 0) {
		stop(print(paste(i, " run: User defined fractions of Shrub, C3, C4, Annuals are all set, but less than 1", sep=""))) #throw an error
	}
	
	if(isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) || TotalFraction > 1 || sum(is.na(AnnC4C3ShrubFraction)) == 1){
		
		if(sum(is.na(AnnC4C3ShrubFraction)) == 1){ #if only one is NA, then this can be calculated
			AnnC4C3ShrubFraction[which(is.na(AnnC4C3ShrubFraction))] <- cut0Inf(1 - TotalFraction)
		} else {					
			AnnC4C3ShrubFraction <- finite01(AnnC4C3ShrubFraction) #the composition is >= 1, so set eventually remaining NA to 0
		}
		
		TotalFraction <- sum(AnnC4C3ShrubFraction, na.rm=TRUE)
		AnnC4C3ShrubFraction <- AnnC4C3ShrubFraction / TotalFraction #Rescale, in case it is needed	
		
	} else { #i.e., (TotalFraction < 1 && sum(is.na(AnnC4C3ShrubFraction)) > 1) is TRUE; thus, calculate some fractions based on climate variables
		if(isNorth){ #Northern hemisphere
			Months_WinterTF <- c(12, 1:2)
			Months_SummerTF <- c(6:8)
		} else {
			Months_WinterTF <- c(6:8)
			Months_SummerTF <- c(12, 1:2)
		}
		ppt.SummerToMAP <- sum(monthly.ppt[Months_SummerTF]) / MAP_mm
		ppt.WinterToMAP <- sum(monthly.ppt[Months_WinterTF]) / MAP_mm
		
		#---Potential natural vegetation
		#1. step: Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
		shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
		grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
		grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
		grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
		grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub.fraction.limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)
		
		grass.Annual.fraction <- AnnC4C3ShrubFraction[1] #Ann will be 0 or something <= 1
		
		#2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
		#This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
		x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
		x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
		x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
		grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
		grass.c4.fractionNA <- ifelse(grass.c4.species > 0, grass.c4.fractionNA, NA)
		
		#3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
		if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
			if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
			if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
			if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
		}
		
		#4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
		#if na then use calc fraction else use the user defined fraction
		shrubs.fraction <- finite01(shrubs.fractionNA)
		grass.c4.fraction <- finite01(grass.c4.fractionNA)
		grass.c3.fraction <- finite01(grass.c3.fractionNA)
		
		sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
		shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
		grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
		grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
		
		calcAnnC4C3ShrubFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
		naIndex <- which(is.na(AnnC4C3ShrubFraction))
		#replace missing values
		if(isTRUE(all.equal(sum(calcAnnC4C3ShrubFraction[naIndex]), 0)) && isTRUE(all.equal(temp <- sum(AnnC4C3ShrubFraction[!naIndex]), 0))){ #there would be no vegetation, so force vegetation > 0
			AnnC4C3ShrubFraction[naIndex] <- (1 - temp) / length(naIndex)
		} else {
			AnnC4C3ShrubFraction[naIndex] <- calcAnnC4C3ShrubFraction[naIndex]
		}
		#now we need to get the sum and scale the naIndex values accordingly
		AnnC4C3ShrubFraction[naIndex] <- sapply(AnnC4C3ShrubFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubFraction[naIndex])) * (1-sum(AnnC4C3ShrubFraction[-naIndex])))
	}
	
	#Scale Grass components to one (or set to 0)
	if(!isTRUE(all.equal(AnnC4C3ShrubFraction[4], 1))){
		grass.c4.fractionG <- AnnC4C3ShrubFraction[2] / (1-AnnC4C3ShrubFraction[4])
		grass.c3.fractionG <- AnnC4C3ShrubFraction[3] / (1-AnnC4C3ShrubFraction[4])
		grass.Annual.fractionG <- AnnC4C3ShrubFraction[1] / (1-AnnC4C3ShrubFraction[4])
	} else {
		grass.c4.fractionG <- grass.c3.fractionG <- grass.Annual.fractionG <- 0
	}
	grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
	
	return(list("Composition"=c("Grasses"=grass.fraction, "Shrubs"=AnnC4C3ShrubFraction[4], "Trees"=tree.fraction),"grasses.c3c4ann.fractions"=c(grass.c3.fractionG,grass.c4.fractionG,grass.Annual.fractionG)))
}

st_mo <- 1:12

AdjMonthlyBioMass <- function(tr_VegetationComposition,AdjMonthlyBioMass_Temperature,AdjMonthlyBioMass_Precipitation,grasses.c3c4ann.fractions,growing.season.threshold.tempC,isNorth,MAP_mm,monthly.temp) {
	tr_VegComp_Adj <- tr_VegetationComposition	#Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
	#Describe conditions for which the default vegetation biomass values are valid
	std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing.season.threshold.tempC == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
	std.growing <- st_mo[-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
	#Default site for the grass description is SGS LTER
	StandardGrasses_MAP_mm <- 340
	StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
	#Default site for the shrub description is Reynolds Creek, ID
	StandardShrub_MAP_mm <- 250
	StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4
	
	#Calculate 'live biomass amount'
	tr_VegComp_Adj$Sh.Amount.Live <- tr_VegComp_Adj$Sh.Biomass * tr_VegComp_Adj$Sh.Perc.Live
	tr_VegComp_Adj$C3.Amount.Live <- tr_VegComp_Adj$C3.Biomass * tr_VegComp_Adj$C3.Perc.Live
	tr_VegComp_Adj$C4.Amount.Live <- tr_VegComp_Adj$C4.Biomass * tr_VegComp_Adj$C4.Perc.Live
	tr_VegComp_Adj$Annual.Amount.Live <- tr_VegComp_Adj$Annual.Biomass * tr_VegComp_Adj$Annual.Perc.Live
	
	#Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
	colmax <- apply(tr_VegComp_Adj[, itemp <- grepl("Litter", names(tr_VegComp_Adj)) | grepl("Amount.Live", names(tr_VegComp_Adj))], MARGIN=2, FUN=max)
	colmin <- apply(tr_VegComp_Adj[, itemp], MARGIN=2, FUN=min)
	tr_VegComp_Adj[, itemp] <- sweep(tr_VegComp_Adj[, itemp], MARGIN=2, STATS=colmax, FUN="/")
	
	#Pull different composition types
	shrubs_Composition <- shrubs_Standard <- tr_VegComp_Adj[, grepl("Sh", names(tr_VegComp_Adj))]
	C3_Composition <- C3_Standard <- tr_VegComp_Adj[, grepl("C3", names(tr_VegComp_Adj))]
	C4_Composition <- C4_Standard <- tr_VegComp_Adj[, grepl("C4", names(tr_VegComp_Adj))]
	AnnGrass_Composition <- AnnGrass_Standard <- tr_VegComp_Adj[, grepl("Annual", names(tr_VegComp_Adj))]
	
	adjCompPPT <- function(shrubs_Composition, C3_Composition, C4_Composition, AnnGrass_Composition, ShrubsMAP_mm, GrassMAP_mm) {
		#Equations: Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
		Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2
		Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5
		
		#Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
		#Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
		Shrub_ANPPintercept <- (StandardShrub_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardShrub_VegComposition[2]*colmax["C3.Amount.Live"] + StandardShrub_VegComposition[3]*colmax["C4.Amount.Live"]) - Shrub_ANPP(StandardShrub_MAP_mm)	#Default input for shrubs (IM_USC00107648_Reynolds; 70% shrubs, 30% C3): biomass was estimated at MAP = 450 mm/yr
		Grasses_ANPPintercept <- (StandardGrasses_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardGrasses_VegComposition[2]*colmax["C3.Amount.Live"] + StandardGrasses_VegComposition[3]*colmax["C4.Amount.Live"]) - Grass_ANPP(StandardGrasses_MAP_mm)		#Default input for sgs-grassland (GP_SGSLTER; 12% shrubs, 22% C3, and 66% C4): biomass was estimated at MAP = 340 mm/yr
		
		#Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
		minimum.totalBiomass <- 0 #This is a SoilWat parameter
		Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(ShrubsMAP_mm) + Shrub_ANPPintercept)
		Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(GrassMAP_mm) + Grasses_ANPPintercept)
		
		#Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
		shrubs_Composition$Sh.Amount.Live <- shrubs_Composition$Sh.Amount.Live * Shrub_BiomassScaler					
		C3_Composition$C3.Amount.Live <- C3_Composition$C3.Amount.Live * Grass_BiomassScaler					
		C4_Composition$C4.Amount.Live <- C4_Composition$C4.Amount.Live * Grass_BiomassScaler					
		AnnGrass_Composition$Annual.Amount.Live <- AnnGrass_Composition$Annual.Amount.Live * Grass_BiomassScaler					
		
		#Scale litter amount by productivity and adjust for ratio of litter/live
		shrubs_Composition$Sh.Litter <- shrubs_Composition$Sh.Litter * Shrub_BiomassScaler * colmax["Sh.Litter"] / colmax["Sh.Amount.Live"]	
		C3_Composition$C3.Litter <- C3_Composition$C3.Litter * Grass_BiomassScaler * colmax["C3.Litter"] / colmax["C3.Amount.Live"]	
		C4_Composition$C4.Litter <- C4_Composition$C4.Litter * Grass_BiomassScaler * colmax["C4.Litter"] / colmax["C4.Amount.Live"]	
		AnnGrass_Composition$Annual.Litter <- AnnGrass_Composition$Annual.Litter * Grass_BiomassScaler * colmax["Annual.Litter"] / colmax["Annual.Amount.Live"]	
		
		#Guarantee that live fraction = ]0, 1]
		shrubs_Composition$Sh.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), shrubs_Composition$Sh.Perc.Live))
		C3_Composition$C3.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), C3_Composition$C3.Perc.Live))
		C4_Composition$C4.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), C4_Composition$C4.Perc.Live))
		AnnGrass_Composition$Annual.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), AnnGrass_Composition$Annual.Perc.Live))
		
		#Calculate total biomass based on scaled live biomass amount
		shrubs_Composition$Sh.Biomass <- shrubs_Composition$Sh.Amount.Live / shrubs_Composition$Sh.Perc.Live
		C3_Composition$C3.Biomass <- C3_Composition$C3.Amount.Live / C3_Composition$C3.Perc.Live
		C4_Composition$C4.Biomass <- C4_Composition$C4.Amount.Live / C4_Composition$C4.Perc.Live
		AnnGrass_Composition$Annual.Biomass <- AnnGrass_Composition$Annual.Amount.Live / AnnGrass_Composition$Annual.Perc.Live
		
		return(list("shrubs_Composition"=shrubs_Composition,"C3_Composition"=C3_Composition,"C4_Composition"=C4_Composition,"AnnGrass_Composition"=AnnGrass_Composition))
	}
	
	#adjust phenology for mean monthly temperatures
	if(AdjMonthlyBioMass_Temperature) {
		growing.season <- monthly.temp >= growing.season.threshold.tempC
		
		if(!isNorth) growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi
		
		predict.season <- function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq){
			#length(std.season.seq) >= 3 because of padding and test that season duration > 0
			calc.loess_coeff <- function(N, span){
				#prevent call to loessc.c:ehg182(104): "span too small.   fewer data values than degrees of freedom"
				lcoef <- list(span=min(1, span), degree=2)
				if(span > 1) return(lcoef)
				nf <- floor(lcoef$span * N) - 1 #see R/trunk/src/library/stats/src/loessf.f:ehg136()
				if(nf > 2){
					lcoef$degree <- 2
				} else if(nf > 1){
					lcoef$degree <- 1
				} else {
					lcoef <- calc.loess_coeff(N, lcoef$span+0.1)
				}
				return(lcoef)		
			}
			lcoef <- calc.loess_coeff(N=length(std.season.seq), span=0.4)
			
			op <- options(c("warn", "error"))
			options(warn=-1, error=traceback) #loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.
			res <- sapply(apply(biomass_Standard, MARGIN=2, function(x) {lf<-loess(x[std.season.padded] ~ std.season.seq, span=lcoef$span, degree=lcoef$degree); predict(lf, newdata=data.frame(std.season.seq=site.season.seq) ) }), FUN=function(x) max(0, x)) # guarantee that > 0
			options(op)
			return(res)
		}
		
		#Adjust for timing and duration of non-growing season
		if(sum(!growing.season) > 0) {
			if(sum(!growing.season) < 12) {
				std.winter.padded <- (c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1) %% 12 + 1
				std.winter.seq <- 0:(length(std.winter.padded) - 1)
				site.winter.seq <- seq(from=1, to=length(std.winter), length=sum(!growing.season))
				site.winter.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(!growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[length(temp3)] #Calculate first month of winter
				site.winter.months <- (site.winter.start + 1:sum(!growing.season) - 2) %% 12 + 1
				
				shrubs_Composition[site.winter.months,] <- predict.season(shrubs_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
				C3_Composition[site.winter.months,] <- predict.season(C3_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
				C4_Composition[site.winter.months,] <- predict.season(C4_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
				AnnGrass_Composition[site.winter.months,] <- predict.season(AnnGrass_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
				
			} else { #if winter lasts 12 months
				#Take the mean of the winter months
				shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
				C3_Composition[] <- matrix(apply(C3_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
				C4_Composition[] <- matrix(apply(C4_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
				AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
			}
		}
		
		#Adjust for timing and duration of growing season
		if(sum(growing.season)>0) {
			if(sum(growing.season) < 12) {
				std.growing.padded <- (c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1) %% 12 + 1
				std.growing.seq <- 0:(length(std.growing.padded) - 1)
				site.growing.seq <- seq(from=1, to=length(std.growing), length=sum(growing.season))
				site.growing.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[1] #Calculate first month of growing season
				site.growing.months <- (site.growing.start + 1:sum(growing.season) - 2) %% 12 + 1
				
				shrubs_Composition[site.growing.months,] <- predict.season(shrubs_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
				C3_Composition[site.growing.months,] <- predict.season(C3_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
				C4_Composition[site.growing.months,] <- predict.season(C4_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
				AnnGrass_Composition[site.growing.months,] <- predict.season(AnnGrass_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
				
			} else { #if growing season lasts 12 months
				shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
				C3_Composition[] <- matrix(apply(C3_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
				C4_Composition[] <- matrix(apply(C4_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
				AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
			}
		}
		if(!isNorth) { #Adjustements were done as if on nothern hemisphere
			shrubs_Composition <- rbind(shrubs_Composition[7:12,], shrubs_Composition[1:6,])
			C3_Composition <- rbind(C3_Composition[7:12,], C3_Composition[1:6,])
			C4_Composition <- rbind(C4_Composition[7:12,], C4_Composition[1:6,])
			AnnGrass_Composition <- rbind(AnnGrass_Composition[7:12,], AnnGrass_Composition[1:6,])
		}
		if(!AdjMonthlyBioMass_Precipitation){
			temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=StandardShrub_MAP_mm, GrassMAP_mm=StandardGrasses_MAP_mm)
			shrubs_Composition <- temp$shrubs_Composition
			C3_Composition <- temp$C3_Composition
			C4_Composition <- temp$C4_Composition
			AnnGrass_Composition <- temp$AnnGrass_Composition
		}
	}
	
	#Adjust biomass amounts by productivity relationship with MAP
	if(AdjMonthlyBioMass_Precipitation) {
		temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=MAP_mm, GrassMAP_mm=MAP_mm)
		shrubs_Composition <- temp$shrubs_Composition
		C3_Composition <- temp$C3_Composition
		C4_Composition <- temp$C4_Composition
		AnnGrass_Composition <- temp$AnnGrass_Composition
	}
	
	Grass_Composition <- C3_Composition*grasses.c3c4ann.fractions[1] + C4_Composition*grasses.c3c4ann.fractions[2] + AnnGrass_Composition*grasses.c3c4ann.fractions[3]
	return(list("grass"=as.matrix(Grass_Composition),"shrub"=as.matrix(shrubs_Composition)))
}
