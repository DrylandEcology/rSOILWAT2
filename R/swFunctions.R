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


## ------SQLite weather database functions
# Daily weather data is stored in database as SQL-blob of a list of R objects of class 'swWeatherData'

con.env <- new.env()
con.env$con <- NULL
con.env$dbW_version <- "3.1.0"
con.env$default_blob_compression_type <- "gzip"
con.env$blob_compression_type <- NULL

dbW_version <- function() {
	stopifnot(requireNamespace("RSQLite"))

	numeric_version(as.character(DBI::dbGetQuery(con.env$con, "SELECT Value FROM Meta WHERE Desc=\'Version\'")[1, 1]))
}

dbW_compression <- function() {
	stopifnot(requireNamespace("RSQLite"))

	as.character(DBI::dbGetQuery(con.env$con, "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'")[1, 1])
}

#' Extract weather database key to connect a site with weather data
#'
#' @details The key (SiteId) can be located by either providing a \code{Label} or
#' by providing \code{lat} and \code{long} of the requested site.
#'
#' @param lat A numeric value or \code{NULL}. The latitude in decimal degrees of WGS84. Northern latitude are positive, sites on the southern hemisphere have negative values.
#' @param long A numeric value or \code{NULL}. The longitude in decimal degrees of WGS84. Eastern longitudes are positive, sites on the western hemisphere have negative values.
#' @param Label A character string or \code{NULL}.
#' @return An integer value or \code{NULL}.
dbW_getSiteId <- function(lat = NULL, long = NULL, Label = NULL) {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	lat <- as.numeric(lat)
	long <- as.numeric(long)
	SQL <- NULL

	if (!is.null(Label)) {
		if (is.character(Label)) {
			SQL <- paste("SELECT Site_id FROM Sites WHERE Label='", Label, "';", sep = "")
		}
	} else if (!is.null(lat) && !is.null(long)) {
		if (!is.na(lat) && !is.na(long) && length(lat) == 1 && length(long) == 1) {
			SQL <- paste("SELECT Site_id FROM Sites WHERE Latitude=", lat, " AND Longitude=", long, ";", sep = "")
		}
	}

	Site_id <- if (!is.null(SQL)) {
			as.integer(DBI::dbGetQuery(con.env$con, SQL))
		} else NULL

	if (!is.finite(Site_id) || Site_id < 0)
		Site_id <- NULL

	if (is.null(Site_id))
		warning("'dbW_getSiteId': could not obtain site ID")

	Site_id
}

dbW_getSiteTable <- function() {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	DBI::dbReadTable(con.env$con, "Sites")
}

dbW_getScenariosTable <- function() {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	DBI::dbReadTable(con.env$con, "Scenarios")
}



#' Extracts daily weather data from a SQLite database
#'
#' Reads weather data from database. Returns list of weather data.
#'
#' Weather data for the soil water simulation run can be stored in the input
#' data or it can be separate to keep the input data size down for multiple
#' variations of the same site. This function is used to return the weather
#' data from a pre defined weather database. Using the database was faster then
#' reading in multiple weather files from disk.
#'
#' SOILWAT does not handle missing weather data. If you have missing data, then
#' you have to impute yourself or use the built-in Markov weather generator
#' (see examples for \code{\link{sw_exec}}).
#'
#' The output from this function can be passed directly to sw_exec with input
#' data.
#'
#' @param con RSQLite con object. A connection to the weather data database.
#' requires(RSQLite)
#' @param Site_id Numeric. Used to identify site and extract weather data.
#' @param lat Numeric. Latitude used with longitude to identify site id if
#' Site_id is missing.
#' @param long Numeric. Longitude and Latitude are used to identify site if
#' Site_id is missing.
#' @param weatherDirName String. If Site_id, lat, and long is missing then this
#' will be used to identify Site_id by parsing the lat and long out of the
#' name.
#' @param startYear Numeric. Extracted weather data will start with this year.
#' @param endYear Numeric. Extracted weather data will end with this year.
#' @return Returns weather data as list. Each element contains data for one
#' year.
#' @author Ryan Murphy
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @examples
#'
#' 	## Default data set without weather data.
#' 	## Column Names are also turned on for output
#' 	library(Rsoilwat31)
#' 	library(RSQLite)
#' 	drv <- dbDriver("SQLite")
#' 	\dontrun{
#'    con <- dbConnect(drv, "/path/to/weather/database/lookupWeatherDB.sqlite3")
#'    inputData <- sw_inputDataFromFiles(dir="/path/to/project",files.in="files_v27.in")
#'    weatherData <- dbW_getWeatherData(con, Site_id=200)
#'    run<-sw_exec(data=inputData, weatherList=weatherData, colNames=TRUE)
#'	}
dbW_getWeatherData <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

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

	Site_id <- as.integer(Site_id)
	if (length(Site_id) == 0 || !is.finite(Site_id)) {
		Site_id <- dbW_getSiteId(lat, long, Label)
	} else {
		if (!DBI::dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.")
		}
	}

	if (!is.null(Site_id)) {
		Scenario <- DBI::dbGetQuery(con.env$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- DBI::dbGetQuery(con.env$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- try(dbW_blob_to_weatherData(result$data, con.env$blob_compression_type))
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

dbW_getWeatherData_old <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

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
		if(!DBI::dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.")
		}
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- DBI::dbGetQuery(con.env$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- DBI::dbGetQuery(con.env$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- dbW_blob_to_weatherData_old(result$StartYear, result$EndYear, result$data, con.env$blob_compression_type)
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
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	#First See if Site_id exists
	Site_id <- as.integer(Site_id)
	if (length(Site_id) == 0) { #Site_id is null
		Site_id <- dbW_getSiteId(lat,long,Label)
	}

	if (is.null(Site_id) ||
		1 > DBI::dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM Sites WHERE Site_id=",Site_id,sep=""))[1,1]) { #Site_id does not exist for given lat,long, and/or Label. Create it

		if (is.null(lat)) lat <- "NULL"
		if (is.null(long)) long <- "NULL"
		Label <- if (is.null(Label)) "NULL" else paste("'", Label, "'", sep = "")
		temp <- DBI::dbGetQuery(con.env$con, "SELECT MAX(Site_id) FROM Sites;")[1,1]
		Site_id <- if(is.na(temp)) 1 else {temp + 1}
		DBI::dbGetQuery(con.env$con, paste("INSERT INTO Sites VALUES(", Site_id, ",", lat, ",", long, ",", Label, ");", sep = ""))

	} else { #Site_id exists already
		SiteData <- DBI::dbGetQuery(con.env$con, paste("SELECT * FROM Sites WHERE Site_id=", Site_id, sep = ""))

		if( (!is.null(lat) && !is.null(SiteData[1, "Latitude"]) && SiteData[1, "Latitude"] != lat) ||
			(!is.null(long) && !is.null(SiteData[1, "Longitude"]) && SiteData[1, "Longitude"] != long) ||
			(!is.null(Label) && nchar(Label) > 0 && !is.null(SiteData[1, "Label"]) && SiteData[1, "Label"] != Label) ) {
				stop(paste("Site_id: ",Site_id," already existed in database. Data mismatch, NULL where ignored : (database:given) lat(",SiteData[1,2],":",lat,") long(",SiteData[1,3],":",long,") label(",SiteData[1,4],":",Label,").",sep=""))
		}
	}

	Site_id
}

dbW_setConnection <- function(dbFilePath) {
	stopifnot(requireNamespace("RSQLite"))

	dbFilePath <- file.path(dbFilePath)
	if (!file.exists(dbFilePath)) {
		print(paste("'dbW_setConnection':", basename(dbFilePath), "does not exist. Creating database."))
	}
	con.env$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbFilePath)
	con.env$blob_compression_type <- if (DBI::dbExistsTable(con.env$con, "Meta")) dbW_compression() else con.env$default_blob_compression_type

	#settings <- c("PRAGMA page_size=8192","PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
	#lapply(settings, function(x) DBI::dbGetQuery(con.env$con,x))
}

dbW_disconnectConnection <- function() {
	stopifnot(requireNamespace("RSQLite"))

	DBI::dbDisconnect(con.env$con)
	con.env$con <- NULL
	con.env$blob_compression_type <- NULL
}

dbW_addSites <- function(dfLatitudeLongitudeLabel) {#lat #long #Label 1 .... 20165
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	RSQLite::dbGetPreparedQuery(con.env$con, "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)", bind.data = as.data.frame(dfLatitudeLongitudeLabel,stringsAsFactors=FALSE))
}

dbW_addScenarios <- function(dfScenario) {#names 1 ... 32
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	RSQLite::dbGetPreparedQuery(con.env$con, "INSERT INTO Scenarios VALUES(NULL, :Scenario)", bind.data = as.data.frame(dfScenario,stringsAsFactors=FALSE))
}

dbW_addWeatherDataNoCheck <- function(Site_id, Scenario_id, StartYear, EndYear, weather_blob) {
	DBI::dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",Scenario_id,",",StartYear,",",EndYear,",",weather_blob,");",sep=""))
}

dbW_addWeatherData <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	if( (is.null(weatherFolderPath) | ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)) & (is.null(weatherData) | !is.list(weatherData) | class(weatherData[[1]]) != "swWeatherData") ) stop("addWeatherDataToDataBase does not have folder path or weatherData to insert")
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.")

	Site_id <- dbW_addSite(
		Site_id = Site_id,
		lat = lat,
		long = long,
		Label = if (!is.null(weatherFolderPath) && is.null(label)) basename(weatherFolderPath) else label)

	Scenarios <- DBI::dbReadTable(con.env$con,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		temp <- DBI::dbGetQuery(con.env$con, "SELECT MAX(id) FROM \"Scenarios\";")[1,1]
		scenarioID <- ifelse(is.na(temp),1,temp+1)
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(",scenarioID,",'",ScenarioName,"');",sep="")
		DBI::dbGetQuery(con.env$con, SQL)
	}

	if(!is.null(weatherData)) {
		data_blob <- dbW_weatherData_to_blob(weatherData, con.env$blob_compression_type)
		StartYear <- head(as.integer(names(weatherData)),n=1)
		EndYear <- tail(as.integer(names(weatherData)),n=1)
		DBI::dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
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
		data_blob <- dbW_weatherData_to_blob(weatherData, con.env$blob_compression_type)
		DBI::dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
		#dbCommit(con.env$con)
	}
}

dbW_addWeatherData_old <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	if( (is.null(weatherFolderPath) | ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)) & (is.null(weatherData) | !is.list(weatherData) | class(weatherData[[1]]) != "swWeatherData") ) stop("addWeatherDataToDataBase does not have folder path or weatherData to insert")
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.")

	Site_id <- dbW_addSite(
		Site_id = Site_id,
		lat = lat,
		long = long,
		Label = if (!is.null(weatherFolderPath) && is.null(label)) basename(weatherFolderPath) else label)

	Scenarios <- DBI::dbReadTable(con.env$con,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		temp <- DBI::dbGetQuery(con.env$con, "SELECT MAX(id) FROM \"Scenarios\";")[1,1]
		scenarioID <- ifelse(is.na(temp),1,temp+1)
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(",scenarioID,",'",ScenarioName,"');",sep="")
		DBI::dbGetQuery(con.env$con, SQL)
	}

	if(!is.null(weatherData)) {
		data_blob <- dbW_weatherData_to_blob_old(weatherData, con.env$blob_compression_type)
		StartYear <- head(as.integer(names(weatherData)),n=1)
		EndYear <- tail(as.integer(names(weatherData)),n=1)
		DBI::dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
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
		data_blob <- dbW_weatherData_to_blob_old(weatherData, con.env$blob_compression_type)
		DBI::dbGetQuery(con.env$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))
		#dbCommit(con.env$con)
	}
}


dbW_createDatabase <- function(dbFilePath = "dbWeatherData.sqlite", site_data = NULL, site_subset = NULL, scenarios = NULL, compression_type) {
	stopifnot(requireNamespace("RSQLite"))

	dbW_setConnection(dbFilePath)

	#---Create tables
	# Meta information
	if (missing(compression_type) || !(compression_type %in% eval(formals(memCompress)[[2]]))) {
		compression_type <- con.env$default_blob_compression_type
	}
	con.env$blob_compression_type <- compression_type

	stopifnot(DBI::dbIsValid(con.env$con))
	DBI::dbGetQuery(con.env$con, "CREATE TABLE \"Meta\" (\"Desc\" TEXT PRIMARY KEY, \"Value\" TEXT);")
	RSQLite::dbGetPreparedQuery(con.env$con, "INSERT INTO Meta VALUES(:Desc, :Value)",
		bind.data = data.frame(Desc = c("Version", "Compression_type"),
								Value = c(con.env$dbW_version, con.env$blob_compression_type)))

	# Table of sites
	DBI::dbGetQuery(con.env$con, "CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL, \"Longitude\" REAL, \"Label\" TEXT);")
	# Table for weather data
	DBI::dbGetQuery(con.env$con, "CREATE TABLE \"WeatherData\" (\"Site_id\" integer, \"Scenario\" integer,  \"StartYear\" integer, \"EndYear\" integer, \"data\" BLOB, PRIMARY KEY (\"Site_id\", \"Scenario\"));")
	# Table of scenario names
	DBI::dbGetQuery(con.env$con, "CREATE TABLE \"Scenarios\" (\"id\" integer PRIMARY KEY, \"Scenario\" TEXT);")

	#---Add sites
	if (NROW(site_data) > 0 && sapply(c("Site_id", "Latitude", "Longitude", "Label"), function(x) x %in% colnames(site_data))) {
		# Default values
		MetaData <- data.frame(Site_id = seq_len(max(site_data[, "Site_id"])),
								Latitude = -999, Longitude = -999,
								Label = NA)
		# Fill in data
		if (is.null(site_subset)) site_subset <- seq_len(nrow(site_data))
		im <- match(site_data[site_subset, "Site_id"], MetaData[, "Site_id"])
		MetaData[im, c("Latitude", "Longitude", "Label")] <- site_data[site_subset, c("Latitude", "Longitude", "Label")]

		dbW_addSites(dfLatitudeLongitudeLabel = MetaData)
	}

	#---Add scenario names
	if (NROW(scenarios) > 0 && "Scenario" %in% colnames(scenarios)) {
		dbW_addScenarios(dfScenario = scenarios)
	}

	invisible(0)
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
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	DBI::dbGetQuery(con.env$con, paste("DELETE FROM \"Sites\" WHERE Site_id=",Site_id,";",sep=""))
}

dbW_deleteSiteData <- function(Site_id, Scenario=NULL) {
	stopifnot(requireNamespace("RSQLite"), DBI::dbIsValid(con.env$con))

	if(is.null(Scenario)) { #Remove all data for this site
		DBI::dbGetQuery(con.env$con, paste("DELETE FROM \"WeatherData\" WHERE Site_id=",Site_id,";",sep=""))
		dbW_deleteSite(Site_id)
	} else {
		DBI::dbGetQuery(con.env$con, paste("DELETE FROM \"WeatherData\" WHERE Site_id=",Site_id," AND Scenario='",Scenario,"';",sep=""))
		if(!DBI::dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM Sites WHERE Site_id=",Site_id,sep=""))[1,1]) {
			dbW_deleteSite(Site_id)
		}
	}
}


## ------ Conversion of weather data formats
#' Conversion: (Compressed) raw vector (e.g., SQL-retrieved blob) to (uncompressed) object
#'
#' The Rsoilwat SQlite-DB which manages daily weather data (each as a list of elements
#' of class 'swWeatherData'), uses internally (compressed) blobs. This function is used
#' to convert the blob object to the object used by Rsoilwat's simulation functions.
#'
#' @param data_blob A raw vector
#' @param type A character string. One of c("gzip", "bzip2", "xz", "none").
#'
#' @seealso \code{\link{memDecompress}}, \code{\link{unserialize}}
dbW_blob_to_weatherData <- function(data_blob, type = "gzip") {
	if (inherits(data_blob, "list") && inherits(data_blob[[1]], "raw") && length(data_blob) == 1)
			data_blob <- data_blob[[1]]

	unserialize(memDecompress(data_blob, type = type))
}

#' Conversion: R object to (compressed) SQL-blob-ready character vector
#'
#' The Rsoilwat SQLite-DB which manages daily weather data (each as a list of elements
#' of class 'swWeatherData'), uses internally (compressed) blobs. This function is used
#' to a list of daily weather data used by Rsoilwat's simulation functions to a blob object
#' which can be inserted into a SQLite DB.
#'
#' @param weatherData A list of elements of class 'swWeatherData' or any suitable object.
#' @param type A character string. One of c("gzip", "bzip2", "xz", "none").
#'
#' @seealso \code{\link{memCompress}}, \code{\link{serialize}}
dbW_weatherData_to_blob <- function(weatherData, type = "gzip") {
	paste0("x'", paste0(memCompress(serialize(weatherData, connection = NULL), type = type), collapse = ""), "'")
}

# Conversion: SQL-blob to object of class 'swWeatherData'
dbW_blob_to_weatherData_old <- function(StartYear, EndYear, data_blob, type = "gzip") {
	if (typeof(data_blob) == "list")
		data_blob <- data_blob[[1]]
	data <- strsplit(memDecompress(data_blob, type = type, asChar = TRUE), ";")[[1]]
	years <- StartYear:EndYear

	weatherData <- list()
	for (i in seq_along(years)) {
		zz <- textConnection(data[i])
		ydata <- read.table(zz, header = FALSE, sep = ",", stringsAsFactors = FALSE)
		close(zz)
		ydata <- as.matrix(cbind(seq_len(nrow(ydata)), ydata))
		colnames(ydata) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
		weatherData[[i]] <- new("swWeatherData", year = years[i], data = ydata)
	}
	names(weatherData) <- years

	weatherData
}

# Conversion: object of class 'swWeatherData' to SQL-blob
dbW_weatherData_to_blob_old <- function(weatherData, type = "gzip") {
	string <- character(length = length(weatherData))
	for(i in seq_along(weatherData)) {
		zz <- textConnection("dataString", "w")
		write.table(x = weatherData[[i]]@data[, -1], file = zz, col.names = FALSE, sep = "," , row.names = FALSE)
		close(zz)
		string[i] <- paste(dataString, collapse = "\n")
	}
	string <- paste(string, collapse=";")

	paste0("x'", paste0(memCompress(string, type = type), collapse = ""), "'")
}

# Conversion: reading of SOILWAT input text files to object of class 'swWeatherData'


#' Rsoilwat getWeatherData_folders
#'
#' Reads weather data from files.  Returns list of weather data.
#'
#' Weather data for the soil water simulation run can be stored in the input
#' data or it can be separate to keep the input data size down for multiple
#' variations of the same site. This function is used to return the weather
#' data from folders.  The other option is to use onGetWeatherData_database.
#'
#' SOILWAT does not handle missing weather data. If you have missing data, then
#' you have to impute yourself or use the built-in Markov weather generator
#' (see examples for \code{\link{sw_exec}}).
#'
#' The output from this function can be passed directly to sw_exec with input
#' data.
#'
#' @param LookupWeatherFolder String Path. Path to the LookupWeatherFolder
#' location.
#' @param weatherDirName String. Name of the folder containing weather data
#' files.
#' @param filebasename String. File prefix for weather data. Usually 'weath'.
#' @param startYear Numeric. Extracted weather data will start with this year.
#' @param endYear Numeric. Extracted weather data will end with this year.
#' @return Returns weather data as list. Each element contains data for one
#' year.
#' @author Ryan Murphy
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @examples
#'
#' path_demo <- system.file("extdata", "example1", package = "Rsoilwat31")
#'
#' ## ------ Simulation with data prepared beforehand and separate weather data ------------
#' ## Read inputs from files on disk
#' sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files_v31.in")
#'
#' ## Read forcing weather data from files on disk (there are also functions to set up a SQLite database for the weather data)
#' sw_weath3 <- getWeatherData_folders(LookupWeatherFolder=file.path(path_demo, "Input"), weatherDirName="data_weather", filebasename="weath", startYear=1979, endYear=2010)
#'
#' ## List of the slots of the input objects of class 'swWeatherData'
#' str(sw_weath3, max.level=1)
#'
#' ## Execute the simulation run
#' sw_out3 <- sw_exec(inputData = sw_in3, weatherList = sw_weath3)
#'
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

# Conversion: object of class 'swWeatherData' to data.frame
dbW_weatherData_to_dataframe <- function(weatherData){
	do.call(rbind, lapply(weatherData, FUN=function(x) {
							temp <- x@data
							Year <- rep(x@year, times=nrow(temp))
							cbind(Year, temp)
						}))
}

# Conversion: object of class 'swWeatherData' to matrix of monthly values (mean Tmax, mean Tmin, sum PPT)
dbW_weatherData_to_monthly <- function(dailySW) {
	monthly <- matrix(NA, nrow = length(dailySW) * 12, ncol = 5, dimnames = list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
	for(y in seq_along(dailySW)){
		weath <- dailySW[[y]]
		month <- as.POSIXlt(paste(weath@year, weath@data[, "DOY"], sep = "-"),
												format = "%Y-%j", tz = "UTC")$mon + 1
		monthly[1:12 + 12*(y - 1), ] <- data.matrix(cbind(
			Year = weath@year, Month = 1:12,
			aggregate(weath@data[, c("Tmax_C", "Tmin_C")], by = list(month), FUN = mean)[, 2:3],
			PPT_cm = aggregate(weath@data[, "PPT_cm"], by = list(month), FUN = sum)[, 2]))
	}

	monthly
}

# Conversion: object of daily weather data.frame to matrix of monthly values (mean Tmax, mean Tmin, sum PPT)
dbW_dataframe_to_monthly <- function(dailySW) {
	month <- as.POSIXlt(apply(dailySW[, c("Year", "DOY")], 1, paste, collapse = "-"), format = "%Y-%j", tz = "UTC")$mon + 1
	as.matrix(cbind(Year = tempT[, 2], Month = tempT[, 1],
		Tmax_C = as.vector(tapply(dailySW[, "Tmax_C"], INDEX = list(month, dailySW[, "Year"]), FUN = mean)),
		Tmin_C = as.vector(tapply(dailySW[, "Tmin_C"], INDEX = list(month, dailySW[, "Year"]), FUN = mean)),
		PPT_cm = as.vector(tapply(dailySW[, "PPT_cm"], INDEX = list(month, dailySW[, "Year"]), FUN = sum))
	))
}



get_years_from_weatherDF <- function(weatherDF, years, weatherDF_dataColumns){
	if(!is.null(years)){
		if(length(years) == nrow(weatherDF)){
			year_ts <- years
		} else if(length(years) == sum(weatherDF[, weatherDF_dataColumns[1]] == 1)){
			year_ts <- rep(years, times = diff(c(which(weatherDF[, weatherDF_dataColumns[1]] == 1), nrow(weatherDF) + 1)))
		} else {
			stop("Not sufficient year information was provided with the 'weatherDF' object")
		}
	} else if(any(temp <- grepl("year", colnames(weatherDF), ignore.case = TRUE))){
		year_ts <- weatherDF[, which(temp)[1]]
	} else {
		stop("No year information was provided with the 'weatherDF' object")
	}

	years <- sort(unique(year_ts))

	return(list(years=years, year_ts=year_ts))
}


# Conversion: data.frame to object of class 'swWeatherData'
dbW_dataframe_to_weatherData <- function(weatherDF, years=NULL, weatherDF_dataColumns=c("DOY","Tmax_C","Tmin_C","PPT_cm"), round = 2){
	if(!(length(weatherDF_dataColumns) == 4) || !all(weatherDF_dataColumns %in% colnames(weatherDF)))
		stop("Not every required weatherDF_dataColumns is available in the 'weatherDF' object")

	ylist <- get_years_from_weatherDF(weatherDF, years, weatherDF_dataColumns)

	if (isTRUE(is.logical(round) && round || is.numeric(round))) {
		weatherDF <- round(weatherDF, digits = if (is.logical(round)) 2 else round)
	}

	weatherData <- list()
	for(i in 1:length(ylist$years)) {
		ydata <- as.matrix(weatherDF[ylist$year_ts == ylist$years[i], weatherDF_dataColumns])
		colnames(ydata) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
		weatherData[[i]] <- new("swWeatherData", year=ylist$years[i], data=ydata)
	}
	names(weatherData) <- ylist$years

	weatherData
}


# Conversion: object of class 'swWeatherData' or data.frame to SOILWAT input text files
dbW_weather_to_SOILWATfiles <- function(path, site.label, weatherData=NULL, weatherDF=NULL, years=NULL, weatherDF_dataColumns=c("DOY","Tmax_C","Tmin_C","PPT_cm")){
	stopifnot(is.null(weatherData) || is.null(weatherDF))
	dir.create(path, recursive = TRUE, showWarnings = FALSE)

	if(!is.null(weatherData)){
		years <- sapply(weatherData, FUN=function(x) x@year)
	} else if(!is.null(weatherDF)){
		if(!(length(weatherDF_dataColumns) == 4) || !all(weatherDF_dataColumns %in% colnames(weatherDF)))
			stop("Not every required weatherDF_dataColumns is available in the 'weatherDF' object")
		temp <- get_years_from_weatherDF(weatherDF, years, weatherDF_dataColumns)
		years <- temp$years
		year_ts <- temp$year_ts
	} else {
		stop("Provide daily weather data either as 'weatherData' or 'weatherDF' object")
	}

	for(y in seq_along(years)){
		data.sw <- if(!is.null(weatherData)) weatherData[[y]]@data else weatherDF[year_ts == years[y], weatherDF_dataColumns]
		sw.filename <- file.path(path, paste0("weath.", years[y]))
		sw.comments <- c(paste("# weather for site", site.label, "year = ", years[y]), "# DOY Tmax(C) Tmin(C) PPT(cm)")

		write.table(sw.comments, file=sw.filename, sep="\t", eol="\r\n", quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(data.frame(data.sw[,1], formatC(data.sw[, 2], digits=2, format="f"), formatC(data.sw[, 3], digits=2, format="f"), formatC(data.sw[, 4], digits=2, format="f")), file=sw.filename, append=TRUE, sep="\t", eol="\r\n", quote=FALSE, row.names=FALSE, col.names=FALSE)
	}

	invisible(years)
}




## ------ Scanning of SOILWAT input text files
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
