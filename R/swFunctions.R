print("swFunctions")

con.env <- new.env()
con.env$con <- NULL

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

dbW_blob_to_weatherData <- function(StartYear, EndYear, data_blob) {
	if(typeof(data_blob) == "list")
		data_blob <- data_blob[[1]]
	data <- strsplit(rawToChar(memDecompress(data_blob, type="gzip")), ";")[[1]]
	years <- seq(from=StartYear, to=EndYear)
	
	weatherData <- list()
	for(i in 1:length(years)) {
		ydata <- read.table(textConnection(data[i]),header=FALSE,sep=",",stringsAsFactors=FALSE)
		ydata <- as.matrix(cbind(seq(from=1,to=nrow(ydata)),ydata))
		colnames(ydata) <- c("DOY","Tmax_C","Tmin_C","PPT_cm")
		weatherData[[i]] <- new("swWeatherData",year=years[i],data=ydata)
	}
	names(weatherData) <- years
	
	return(weatherData)
}

dbW_weatherData_to_blob <- function(weatherData) {
	string <- character(length=length(weatherData))
	for(i in 1:length(weatherData)) {
		zz <- textConnection("dataString","w")
		write.table(x=weatherData[[i]]@data[,2:4], file=zz, col.names=FALSE, sep="," ,row.names=FALSE)
		close(zz)
		string[i] <-paste(dataString,collapse="\n")
	}
	string<-paste(string,collapse=";")
	data_blob <- paste0("x'",paste0(memCompress(string,type="gzip"),collapse = ""),"'",sep="")
	return(data_blob)
}

dbW_getSiteId <- function(lat=NULL, long=NULL, Label=NULL) {
	lat<-as.numeric(lat)
	long<-as.numeric(long)
	if(!is.null(Label)) {
		if(is.character(Label)) {
			SQL <- paste("SELECT Site_id FROM Sites WHERE Label='",Label,"';",sep="")
			site <- dbGetQuery(con.env$con,SQL)
			Site_id <- as.integer(site)
		}
	} else  if(!is.null(lat) & !is.null(long)) {
		if(!is.na(lat) & !is.na(long) & length(lat) == 1 & length(long) == 1) {
			SQL <- paste("SELECT Site_id FROM Sites WHERE Latitude=",lat," AND Longitude=",long,";",sep="")
			site <- dbGetQuery(con.env$con,SQL)
			Site_id <- as.integer(site)
		}
	}
	if(is.na(Site_id))
		return(NULL)
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		return(Site_id)
	} else {
		warning("Could not obtain Site Id.")
		return(NULL)
	}
}

dbW_getSiteTable <- function() {
	return(dbReadTable(con.env$con, "Sites"))
}

dbW_getScenariosTable <- function() {
	return(dbReadTable(con.env$con, "Scenarios"))
}

dbW_getWeatherData <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	if(is.null(Site_id) && is.null(Label) && is.null(lat) && is.null(long)) {
		stop("No way to locate weather data from input")
	}
	
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
			if(startYear >= endYear | startYear<0 | endYear<0) {
				stop("Wrong start or end year")
			}
		}
	}
	Site_id<-as.integer(Site_id)
	if(length(Site_id) == 0) {
		Site_id <- dbW_getSiteId(lat,long,Label)
	} else {
		if(!dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.")
		}
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- dbGetQuery(con.env$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- dbGetQuery(con.env$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- dbW_blob_to_weatherData(result$StartYear, result$EndYear, result$data)
		if(inherits(data, "try-error")) stop(paste("Weather data for Site_id", Site_id, "is corrupted"))
	} else {
		stop(paste("Site_id for", Label, "not obtained."))
	}
	
	if(useYears) {
		if(useStart && useEnd) {
		        # adjusting so we actually explore the values of the "year" slots of our soilwatDB object list
			# startYear_idx <- match(startYear,as.integer(names(data)))
			startYear_idx <- match(startYear, 
			                       as.integer(unlist(lapply(data, FUN=slot, "year"))))
			# endYear_idx <- match(endYear,as.integer(names(data)))
			endYear_idx <- match(endYear, 
					     as.integer(unlist(lapply(data, FUN=slot, "year"))))
			data <- data[startYear_idx:endYear_idx]
		} else if(useStart) {
			#startYear_idx <- match(startYear,as.integer(names(data)))
			startYear_idx <- match(startYear, 
					       as.integer(unlist(lapply(data, FUN=slot, "year"))))
			# data <- data[startYear_idx:length(as.integer(names(data)))]
			data <- data[startYear_idx:length(as.integer(unlist(lapply(data, FUN=slot, "year"))))]
		} else if(useEnd) {
			# endYear_idx <- match(endYear,as.integer(names(data)))
			endYear_idx <- match(endYear,
			                     as.integer(unlist(lapply(data, FUN=slot, "year"))))
			data <- data[1:endYear_idx]
		}
	}
	return(data)
}

dbW_addSite <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL) {
	#First See if Site_id exists
	Site_id<-as.integer(Site_id)
	if(length(Site_id) == 0) {#Site_id is null
		Site_id <- dbW_getSiteId(lat,long,Label)
		if(is.null(Site_id)) { #Site_id does not exist for given lat,long, and/or Label. Create it
			if(is.null(lat)) lat <- "NULL"
			if(is.null(long)) long <- "NULL"
			if(is.null(Label)) Label <- "NULL" else Label <- paste("'",Label,"'",sep="")
			temp<-dbGetQuery(con.env$con, "SELECT MAX(Site_id) FROM Sites;")[1,1]
			Site_id <- ifelse(is.na(temp),1,temp+1)
			dbGetQuery(con.env$con, paste("INSERT INTO Sites VALUES(",Site_id,",",lat,",",long,",",Label,");",sep=""))
			return(Site_id)
		} else { #Site_id exists already
			SiteData <- dbGetQuery(con.env$con, paste("SELECT * FROM Sites WHERE Site_id=",Site_id,sep=""))
			SiteData_lat <- SiteData[1,2]
			if(is.na(SiteData_lat)) SiteData_lat <- NULL
			SiteData_long <- SiteData[1,3]
			if(is.na(SiteData_long)) SiteData_long <- NULL
			SiteData_label <- SiteData[1,4]
			if(is.na(SiteData_label)) SiteData_label <- NULL
			if( (ifelse(length(temp<-SiteData_lat != lat) == 0, TRUE, temp)) | (ifelse(length(temp<-SiteData_long != long) == 0, TRUE, temp)) | (ifelse(length(temp <- Label != "") == 0, TRUE, temp) & ifelse(length(temp <- SiteData_label != Label) == 0, TRUE, temp)) ) {
				stop(paste("Site_id: ",Site_id," already existed in database. Data mismatch, NULL where ignored : (database:given) lat(",SiteData[1,2],":",lat,") long(",SiteData[1,3],":",long,") label(",SiteData[1,4],":",Label,").",sep=""))
			}
			return(Site_id)
		}
	} else {
		if(dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM Sites WHERE Site_id=",Site_id,sep=""))[1,1]) {#Site id already Exists
			SiteData <- dbGetQuery(con.env$con, paste("SELECT * FROM Sites WHERE Site_id=",Site_id,sep=""))
			if(is.na(SiteData_lat)) SiteData_lat <- NULL
			SiteData_long <- SiteData[1,3]
			if(is.na(SiteData_long)) SiteData_long <- NULL
			SiteData_label <- SiteData[1,4]
			if(is.na(SiteData_label)) SiteData_label <- NULL
			if( (!is.null(lat) & SiteData_lat != lat) | (!is.null(long) & SiteData_long != long) | (!is.null(Label) & Label != "" & SiteData_label != Label) ) {
				stop(paste("Site_id: ",Site_id," already existed in database. Data mismatch, NULL where ignored : (database:given) lat(",SiteData[1,2],":",lat,") long(",SiteData[1,3],":",long,") label(",SiteData[1,4],":",Label,").",sep=""))
			}
			return(Site_id)
		} else {#Create it
			if(is.null(lat)) lat <- "NULL"
			if(is.null(long)) long <- "NULL"
			if(is.null(Label)) Label <- "NULL" else Label <- paste("'",Label,"'",sep="")
			temp<-dbGetQuery(con.env$con, "SELECT MAX(Site_id) FROM Sites;")[1,1]
			Site_id <- ifelse(is.na(temp),1,temp+1)
			dbGetQuery(con.env$con, paste("INSERT INTO Sites VALUES(",Site_id,",",lat,",",long,",",Label,");",sep=""))
			return(Site_id)
		}
	}
}

dbW_setConnection <- function(dbFilePath, createAdd=FALSE) {
	require(RSQLite)
	drv <- dbDriver("SQLite")
	
	#settings <- c("PRAGMA page_size=8192","PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
	
	tfile <- file.path(dbFilePath)
	if(!file.exists(dbFilePath)) {
		print("dbFilePath does not exist. Creating database.")
	}
	#assign("con", dbConnect(drv, dbname = tfile), envir="package:Rsoilwat")
	con.env$con <- dbConnect(drv, dbname = tfile)
	#if(createAdd) lapply(settings, function(x) dbGetQuery(con.env$con,x))
}

dbW_disconnectConnection <- function() {
	dbDisconnect(con.env$con)
}

dbW_addSites <- function(dfLatitudeLongitudeLabel) {#lat #long #Label 1 .... 20165
	dbGetPreparedQuery(con.env$con, "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)", bind.data = as.data.frame(dfLatitudeLongitudeLabel,stringsAsFactors=FALSE))
}

dbW_addScenarios <- function(dfScenario) {#names 1 ... 32
	dbGetPreparedQuery(con.env$con, "INSERT INTO Scenarios VALUES(NULL, :Scenario)", bind.data = as.data.frame(dfScenario,stringsAsFactors=FALSE))
}

dbW_addWeatherDataNoCheck <- function(Site_id, Scenario_id, StartYear, EndYear, weatherData) {
	dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",Scenario_id,",",StartYear,",",EndYear,",",weatherData,");",sep=""))
}

dbW_addWeatherData <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	if( (is.null(weatherFolderPath) | ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)) & (is.null(weatherData) | !is.list(weatherData) | class(weatherData[[1]]) != "swWeatherData") ) stop("addWeatherDataToDataBase does not have folder path or weatherData to insert")
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.")
	
	Site_id <- dbW_addSite(Site_id=Site_id, lat=lat, long=long, Label=ifelse(!is.null(weatherFolderPath) & is.null(label), basename(weatherFolderPath), label))
	
	Scenarios <- dbReadTable(con.env$con,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		temp <- dbGetQuery(con.env$con, "SELECT MAX(id) FROM \"Scenarios\";")[1,1]
		scenarioID <- ifelse(is.na(temp),1,temp+1)
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(",scenarioID,",'",ScenarioName,"');",sep="")
		dbGetQuery(con.env$con, SQL)
	}
	
	if(!is.null(weatherData)) {
		data_blob <- dbW_weatherData_to_blob(weatherData)
		StartYear <- head(as.integer(names(weatherData)),n=1)
		EndYear <- tail(as.integer(names(weatherData)),n=1)
		dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
		#dbCommit(con.env$con)
	} else {
		weath <- list.files(weatherFolderPath)
		years <- as.numeric(sub(pattern="weath.",replacement="",weath))
		weatherData <- list()
		for(j in 1:length(weath)) {
			year <- as.numeric(sub(pattern="weath.",replacement="",weath[j]))
			temp <-read.csv(file.path(weatherFolderPath,weath[j]),header=FALSE,skip=2,sep="\t")
			weatherData[[j]] <- swReadLines(new("swWeatherData",year),file.path(weatherFolderPath,weath[j]))
		}
		StartYear <- head(years,n=1)
		EndYear <- tail(years,n=1)
		data_blob <- dbW_weatherData_to_blob(weatherData)
		dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
		#dbCommit(con.env$con)
	}
}

dbW_createDatabase <- function(dbFilePath="dbWeatherData.sqlite") {
	dbW_setConnection(dbFilePath, FALSE)
	SQL <- paste("CREATE TABLE \"Version\" (\"Version\" integer);")
	dbGetQuery(con.env$con, SQL)
	
	dbGetQuery(con.env$con, "INSERT INTO Version (Version) VALUES (1);")
	
	SQL <- paste("CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL, \"Longitude\" REAL, \"Label\" TEXT);", sep="")
	dbGetQuery(con.env$con, SQL)
	#TABLE WEATHER DATA
	SQL <- paste("CREATE TABLE \"WeatherData\" (\"Site_id\" integer, \"Scenario\" integer,  \"StartYear\" integer, \"EndYear\" integer, \"data\" BLOB, PRIMARY KEY (\"Site_id\", \"Scenario\"));", sep="")
	dbGetQuery(con.env$con, SQL)
	#Scenario Names
	SQL <- "CREATE TABLE \"Scenarios\" (\"id\" integer PRIMARY KEY, \"Scenario\" TEXT);"
	dbGetQuery(con.env$con, SQL)	
}

#dataframe of columns folder, lat, long, label where label can equal folderName
dbW_addFromFolders <- function(MetaData=NULL, FoldersPath, ScenarioName="Current") {
	if(!is.null(MetaData)) {
		temp <- apply(MetaData, MARGIN = 1, function(x) dbW_addWeatherData(Site_id = NULL, lat=x[2], long=x[3], weatherFolderPath = file.path(FoldersPath, x[1]), weatherData = NULL, label = x[4], ScenarioName = ScenarioName) )
	} else {
		files <- list.files(path=FoldersPath)
		temp <- lapply(files, function(x) dbW_addWeatherData(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=file.path(FoldersPath, x), weatherData=NULL, ScenarioName = ScenarioName))
	}
}

dbW_deleteSite <- function(Site_id) {
	dbGetQuery(con.env$con, paste("DELETE FROM \"Sites\" WHERE Site_id=",Site_id,";",sep=""))
}

dbW_deleteSiteData <- function(Site_id, Scenario=NULL) {
	if(is.null(Scenario)) { #Remove all data for this site
		dbGetQuery(con.env$con, paste("DELETE FROM \"WeatherData\" WHERE Site_id=",Site_id,";",sep=""))
		dbW_deleteSite(Site_id)
	} else {
		dbGetQuery(con.env$con, paste("DELETE FROM \"WeatherData\" WHERE Site_id=",Site_id," AND Scenario='",Scenario,"';",sep=""))
		if(!dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM Sites WHERE Site_id=",Site_id,sep=""))[1,1]) {
			dbW_deleteSite(Site_id)
		}
	}
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