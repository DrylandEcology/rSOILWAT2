print("swFunctions")

getWeatherData_folders <- function(LookupWeatherFolder=NULL, weatherDirName=NULL,filebasename=NULL,startYear=NULL,endYear=NULL) {
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

getSiteId <- function(con, lat=NULL, long=NULL, weatherDirName=NULL) {
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
		SQL <- paste("SELECT Site_id FROM Sites WHERE Latitude=",lat," AND Longitude=",long,";",sep="")
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

getWeatherData_database <- function(dbWeatherDataFile, Site_id=NULL,lat=NULL,long=NULL,weatherDirName=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	require(RSQLite)
	drv <- dbDriver("SQLite")
	con <- dbConnect(drv, dbname=dbWeatherDataFile)	
	
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
		Site_id <- getSiteId(con, lat,long,weatherDirName)
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- dbGetQuery(con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- dbGetQuery(con, paste("SELECT data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""))[[1]][[1]];
		data <- unserialize(memDecompress(result,type="gzip"))
	} else {
		stop("Site_id not obtained.")
	}
	
	if(useYears) {
		if(useStart && useEnd) {
			startYear_idx <- match(startYear,as.integer(names(data)))
			endYear_idx <- match(endYear,as.integer(names(data)))
			data <- data[startYear_idx:endYear_idx]
		} else if(useStart) {
			startYear_idx <- match(startYear,as.integer(names(data)))
			data <- data[startYear_idx:length(as.integer(names(data)))]
		} else if(useEnd) {
			endYear_idx <- match(endYear,as.integer(names(data)))
			data <- data[1:endYear_idx]
		}
	}
	dbDisconnect(con)
	return(weathDataList)
}

addWeatherDataToDataBase <- function(databasePath, Site_id, ScenarioName, weatherData) {
	library(RSQLite)
	drv <- dbDriver("SQLite")
	
	settings <- c("PRAGMA page_size=8192","PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
	
	tfile <- file.path(databasePath)
	con_blob <- dbConnect(drv, dbname = tfile)
	lapply(settings, function(x) dbGetQuery(con_blob,x))
	
	Scenarios <- dbReadTable(con_blob,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(1,'",ScenarioName,"');",sep="")
		dbGetQuery(con_blob, SQL)
		Scenarios <- dbReadTable(con_blob,"Scenarios")$Scenario
		scenarioID <- which(ScenarioName %in% Scenarios)
	}
	
	data_blob <- paste0("x'",paste0(memCompress(serialize(weatherData,NULL),type="gzip"),collapse = ""),"'",sep="")
	dbGetQuery(con_blob, paste("INSERT INTO WeatherData (Site_id, Scenario, data) VALUES (",Site_id,",",scenarioID,",",data_blob,");",sep=""))
	dbCommit(con_blob)
	
	dbDisconnect(con_blob)
}

createDatabaseFromLookupWeatherFolder <- function(dbNamePath="dbWeatherData.sqlite", LookupWeatherFolderPath, ScenarioName="Current") {
	
	setwd(LookupWeatherFolderPath)
	
	require(RSQLite)
	drv <- dbDriver("SQLite")
	
	settings <- c("PRAGMA page_size=8192","PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
	
	tfile <- file.path(dbNamePath)
	con_blob <- dbConnect(drv, dbname = tfile)
	lapply(settings, function(x) dbGetQuery(con_blob,x))
	
	getSplit <- function(string) {
		indexs <- gregexpr("-", string)[[1]]
		if(length(indexs) == 1)
			return(c(substr(string, 1, indexs-1) ,substring(string, indexs+1)))
		if(length(indexs) == 2)
			if(indexs[1] == 1)
				return(c(substr(string, 1, indexs[2]-1) ,substring(string, indexs[2]+1)))
			else
				return(c(substr(string, 1, indexs[1]-1) ,substring(string, indexs[1]+1)))
		if(length(indexs) == 3)
			return(c(substr(string, 1, indexs[2]-1) ,substring(string, indexs[2]+1)))
	}
	
	files <- list.files(pattern = "Weather")
	files <- sub("Weather_NCEPCFSR_", "", files)
	#SWRunInformation <- tryCatch(read.csv(file.path("/home/ryan/Documents/Work/Projects/1_PC_TempDry_Simulations_Prj04_r3mini/1_Data_SWInput/SWRuns_InputMaster_TemperateArid_SiteSubSubSubsample_MiniProjects_v11.csv"), as.is=TRUE),error=function(e) { print("datafile.SWRunInformation: Bad Path"); print(e)})
	#weatherFolders <- SWRunInformation$WeatherFolder
	#weatherFolders <- sub("Weather_NCEPCFSR_", "", weatherFolders)
		
	Cord <- lapply(files, getSplit)
	mCord <- matrix(data=as.numeric(unlist(Cord)), nrow=length(Cord), ncol=2, byrow=TRUE)
	Cord <- list(lat=mCord[,1], long=mCord[,2])
	Cord$lat <- as.numeric(Cord$lat)
	Cord$long <- as.numeric(Cord$long)
	
	#Test to see if split works
	#same <- logical(0)
	#for(i in 1:length(Cord$lat)) {
	#	same<-c(same,identical(paste("Weather_NCEPCFSR_", format(Cord$lat[i], digits=1, nsmall=3), "-", formatC(Cord$long[i], digits=3, format="f"), sep=""), SWRunInformation$WeatherFolder[i]))
	#}
	#print(all(same))
	
	#TABLE SITES
	SQL <- paste("CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL, \"Longitude\" REAL);", sep="")
	dbGetQuery(con_blob, SQL)
	#TABLE WEATHER DATA
	SQL <- paste("CREATE TABLE \"WeatherData\" (\"Site_id\" integer, \"Scenario\", \"data\" BLOB, PRIMARY KEY (\"Site_id\", \"Scenario\"));", sep="")
	dbGetQuery(con_blob, SQL)
	#Scenario Names
	SQL <- "CREATE TABLE \"Scenarios\" (\"id\" integer PRIMARY KEY, \"Scenario\" TEXT);"
	dbGetQuery(con_blob, SQL)
	SQL <- paste("INSERT INTO \"Scenarios\" VALUES(1,'",ScenarioName,"');",sep="")
	dbGetQuery(con_blob, SQL)
	
	#library(Rsoilwat31)
	
	for(i in 1:length(Cord$lat)) {#
		dbBeginTransaction(con_blob)
		dbGetQuery(con_blob, paste("INSERT INTO \"Sites\" VALUES(",paste("'",c(i,Cord$lat[i],Cord$long[i]),"'", sep="",collapse=","),");"))
		
		WeatherFolder <- file.path(LookupWeatherFolderPath,paste("Weather_NCEPCFSR_", format(Cord$lat[i], digits=1, nsmall=3), "-", formatC(Cord$long[i], digits=3, format="f"), sep=""))
		weath <- list.files(WeatherFolder)
		years <- as.numeric(sub(pattern="weath.",replacement="",weath))
		weatherData <- list()
		for(j in 1:length(weath)) {
			year <- as.numeric(sub(pattern="weath.",replacement="",weath[j]))
			temp <-read.csv(file.path(WeatherFolder,weath[j]),header=FALSE,skip=2,sep="\t")
			weatherData[[j]] <- swReadLines(new("swWeatherData",year),file.path(WeatherFolder,weath[j]))
		}
		names(weatherData) <- years
		data_blob <- paste0("x'",paste0(memCompress(serialize(weatherData,NULL),type="gzip"),collapse = ""),"'",sep="")
		dbGetQuery(con_blob, paste("INSERT INTO WeatherData (Site_id, Scenario, data) VALUES (",i,",",1,",",data_blob,");",sep=""))
		dbCommit(con_blob)
		
		if(i %in% c(10,100,1000,5000,10000,15000,20000)) print(i)
	}
	dbDisconnect(con_blob)
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