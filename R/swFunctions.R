print("swFunctions")

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
