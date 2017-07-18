###############################################################################
#rSOILWAT2
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
con.env$dbW_version <- "3.1.1"
con.env$default_blob_compression_type <- "gzip"
con.env$blob_compression_type <- NULL

#' @export
dbW_IsValid <- function() {
	!is.null(con.env$con) && DBI::dbIsValid(con.env$con)
}

#' @export
dbW_version <- function() {
	stopifnot(dbW_IsValid())

	sql <- "SELECT Value FROM Meta WHERE Desc=\'Version\'"
	numeric_version(as.character(DBI::dbGetQuery(con.env$con, sql)[1, 1]))
}

#' @export
dbW_compression <- function() {
	stopifnot(dbW_IsValid())

	sql <- "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'"
	as.character(DBI::dbGetQuery(con.env$con, sql)[1, 1])
}


#' @export
dbW_has_sites <- function(Labels) {
	sapply(Labels, function(x) {
		sql <- paste0("SELECT COUNT(*) FROM Sites WHERE Label=", x)
		DBI::dbGetQuery(con.env$con, sql)[1, 1] > 0
	})
}


#' @export
dbW_has_siteIDs <- function(Site_ids) {
	sapply(Site_ids, function(id) {
		sql <- paste0("SELECT COUNT(*) FROM Sites WHERE Site_id=", id)
		DBI::dbGetQuery(con.env$con, sql)[1, 1] > 0
	})
}


#' @export
dbW_has_scenarioIDs <- function(scenario_ids) {
	sapply(scenario_ids, function(x) {
		sql <- paste0("SELECT COUNT(*) FROM Scenarios WHERE id=", x)
		DBI::dbGetQuery(con.env$con, sql)[1, 1] > 0
	})
}


#' @export
dbW_has_scenarios <- function(scenarios, ignore.case = FALSE) {
	sapply(scenarios, function(x) {
		sql <- paste0("SELECT COUNT(*) FROM Scenarios WHERE Scenario=", shQuote(x),
			if (ignore.case) " COLLATE NOCASE")
		DBI::dbGetQuery(con.env$con, sql)[1, 1] > 0
	})
}


#' Extract weather database key to connect a site with weather data
#'
#' @details The key (Site_id) can be located by either providing a \code{Label} or
#' by providing \code{lat} and \code{long} of the requested site.
#'
#' @param lat A numeric value or \code{NULL}. The latitude in decimal degrees of WGS84.
#'	Northern latitude are positive, sites on the southern hemisphere have negative values.
#' @param long A numeric value or \code{NULL}. The longitude in decimal degrees of WGS84.
#'	Eastern longitudes are positive, sites on the western hemisphere have negative values.
#' @param Label A character string or \code{NULL}.
#' @return An integer value or \code{NULL}.
#' @export
dbW_getSiteId <- function(lat = NULL, long = NULL, Label = NULL, ignore.case = FALSE,
	verbose = FALSE) {
	stopifnot(dbW_IsValid())

	lat <- as.numeric(lat)
	long <- as.numeric(long)
	SQL <- NULL

	if (!is.null(Label)) {
		if (is.character(Label)) {
			SQL <- paste0("SELECT Site_id FROM Sites WHERE Label=", shQuote(Label),
				if (ignore.case) " COLLATE NOCASE")
		}
	} else if (!is.null(lat) && !is.null(long)) {
		if (!is.na(lat) && !is.na(long) && length(lat) == 1 && length(long) == 1) {
			SQL <- paste0("SELECT Site_id FROM Sites WHERE Latitude=", lat, " AND Longitude=",
				long)
		}
	}

	Site_id <- if (!is.null(SQL)) {
			as.integer(DBI::dbGetQuery(con.env$con, SQL))
		} else NULL

	if (!is.finite(Site_id) || Site_id < 0)
		Site_id <- NULL

	if (is.null(Site_id) && verbose)
		message("'dbW_getSiteId': could not obtain site ID")

	Site_id
}

#' @export
dbW_getSiteTable <- function() {
	stopifnot(dbW_IsValid())

	DBI::dbReadTable(con.env$con, "Sites")
}

#' @export
dbW_getScenariosTable <- function() {
	stopifnot(dbW_IsValid())

	DBI::dbReadTable(con.env$con, "Scenarios")
}


# Index along years to narrow the start and/or end year if not NULL
select_years <- function(years, start_year = NULL, end_year = NULL) {

	if (!is.null(start_year) || !is.null(end_year)) {
		start_year <- as.integer(start_year)
		use_start <- !is.na(start_year)
		end_year <- as.integer(end_year)
		use_end <- !is.na(end_year)

		if (use_start && use_end && (start_year >= end_year || start_year < 0 || end_year < 0)) {
			warning("'select_years': wrong value for argument 'start_year' and/or 'end_year'")
		}

	} else {
		use_start <- use_end <- FALSE
	}

	idx_start_year <- 1L
	if (use_start) {
		temp <- match(start_year, years)
		if (!is.na(temp)) {
			idx_start_year <- temp
		}
	}

	idx_end_year <- length(years)
	if (use_end) {
		temp <- match(end_year, years)
		if (!is.na(temp)) {
			idx_end_year <- temp
		}
	}

	idx_start_year:idx_end_year
}

#' export
get_years_from_weatherData <- function(wd) {
	as.integer(unlist(lapply(wd, FUN = slot, "year")))
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
#' @param Site_id Numeric. Used to identify site and extract weather data.
#' @param lat Numeric. Latitude used with longitude to identify site id if
#' Site_id is missing.
#' @param long Numeric. Longitude and Latitude are used to identify site if
#' Site_id is missing.
#' @param Label A character string.
#' @param startYear Numeric. Extracted weather data will start with this year.
#' @param endYear Numeric. Extracted weather data will end with this year.
#' @param Scenario A character string.
#'
#' @return Returns weather data as list. Each element contains data for one
#' year.
#' @author Ryan Murphy
#' @seealso \itemize{
#'    \item \code{\link{sw_exec}} for running a simulation
#'    \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input
#' }
#' @export
dbW_getWeatherData <- function(Site_id = NULL, lat = NULL, long = NULL, Label = NULL,
	startYear = NULL, endYear = NULL, Scenario = "Current", ignore.case = FALSE) {

	stopifnot(dbW_IsValid())

	if (all(sapply(list(Site_id, Label, lat, long), is.null))) {
		stop("No way to locate weather data from input")
	}

	Site_id <- as.integer(Site_id)
	if (length(Site_id)  ==  0 || !is.finite(Site_id)) {
		Site_id <- try(dbW_getSiteId(lat, long, Label, ignore.case = ignore.case),
			silent = TRUE)

		if (inherits(Site_id, "try_error")) {
			stop(paste("Site_id for", Label, "not found in weather database."))
		}
	} else {
		stopifnot(dbW_has_siteIDs(Site_id))
	}

	sql <- paste0("SELECT id FROM Scenarios WHERE Scenario =", shQuote(Scenario),
		if (!ignore.case) " COLLATE NOCASE")
	Scenario_id <- DBI::dbGetQuery(con.env$con, sql)[1, 1]
	if (is.na(Scenario_id)) {
		stop(paste("Scenario", shQuote(Scenario), "does not exist in weather database."))
	}

	sql <- paste("SELECT data FROM WeatherData WHERE Site_id =", Site_id,
		"AND Scenario =", Scenario_id)
	res <- DBI::dbGetQuery(con.env$con, sql)[1, 1]
	if (is.na(res)) {
		stop(paste("Weather data for site", shQuote(Site_id), "and scenario",
			shQuote(Scenario), "does not exist in weather database."))
	}

	wd <- try(dbW_blob_to_weatherData(res, con.env$blob_compression_type))
	if (inherits(wd, "try-error")) {
		stop(paste("Weather data for site", shQuote(Site_id), "and scenario",
			shQuote(Scenario), "is corrupted."))
	}

	temp <- class(wd[[1]])
	if (!(attr(temp, "package") == "rSOILWAT2")) {
		message(paste("WARNING: The class of the extracted weather data object is",
			shQuote(temp), "from package", shQuote(attr(temp, "package")), "which is",
			"outdated. Please, upgrade weather database with function",
			"'dbW_upgrade_to_rSOILWAT2'."))
	}

	years <- get_years_from_weatherData(wd)
	ids <- select_years(years, startYear, endYear)

	wd[ids]
}

#' @export
dbW_addSite <- function(Site_id = NULL, lat = NULL, long = NULL, Label = NULL) {
	stopifnot(dbW_IsValid())

	#Does Site_id exist
	Site_id <- as.integer(Site_id)
	if (length(Site_id) == 0) { #Site_id is NULL or integer(0)
		Site_id <- dbW_getSiteId(lat, long, Label)
	}
	stopifnot(length(Site_id) == 1L)

	if (!dbW_has_siteIDs(Site_id)) {
		# Site_id does not exist in database: create it
		if (is.null(lat)) lat <- "NULL"
		if (is.null(long)) long <- "NULL"
		Label <- if (is.null(Label)) "NULL" else shQuote(Label)
		sql <- "SELECT MAX(Site_id) FROM Sites"
		temp <- DBI::dbGetQuery(con.env$con, sql)[1,1]
		Site_id <- if (is.na(temp)) 1L else {temp + 1}
		sql <- paste0("INSERT INTO Sites VALUES(", Site_id, ",", lat, ",", long, ",",
			Label, ")")
		DBI::dbExecute(con.env$con, sql)

	} else {
		# Site_id exists already
		sql <- paste("SELECT * FROM Sites WHERE Site_id=", Site_id)
		SiteData <- DBI::dbGetQuery(con.env$con, sql)

		bad_lat <- !is.null(lat) &&
			!(is.null(SiteData[1, "Latitude"]) || identical(SiteData[1, "Latitude"], "NULL")) &&
			SiteData[1, "Latitude"] != lat
		bad_long <- !is.null(long) &&
			!(is.null(SiteData[1, "Longitude"]) || identical(SiteData[1, "Longitude"], "NULL")) &&
			SiteData[1, "Longitude"] != long
		bad_label <- !is.null(Label) && nchar(Label) > 0 &&
			!(is.null(SiteData[1, "Label"]) || identical(SiteData[1, "Label"], "NULL")) &&
			SiteData[1, "Label"] != Label
		if (bad_lat || bad_long || bad_label) {
				stop("Site_id: ", Site_id, " already existed in database, but data mismatch ",
				"with NULL being ignored. Compare data (database:input) ",
				"lat(", SiteData[1, "Latitude"], ":", lat, ") ",
				"long(", SiteData[1, "Longitude"], ":", long, ") ",
				"label(", SiteData[1, "Label"], ":", Label, ").")
		}
	}

	Site_id
}

#' @export
dbW_setConnection <- function(dbFilePath, create_if_missing = FALSE, verbose = FALSE) {
	dbFilePath <- try(normalizePath(dbFilePath, mustWork = FALSE), silent = TRUE)

	if (inherits(dbFilePath, "try-error") || !file.exists(dbFilePath)) {
		if (verbose) {
			message(paste("'dbW_setConnection':", basename(dbFilePath), "does not exist."))
		}
		if (create_if_missing) {
			if (verbose) {
				message(paste("'dbW_setConnection': creating a new database."))
			}
		} else {
			return(invisible(FALSE))
		}
	}

	con.env$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbFilePath)
	con.env$blob_compression_type <- if (DBI::dbExistsTable(con.env$con, "Meta")) {
			dbW_compression()
		} else {
			con.env$default_blob_compression_type
		}

	invisible(dbW_IsValid())
}

#' @export
dbW_disconnectConnection <- function() {
	if (dbW_IsValid())
		DBI::dbDisconnect(con.env$con)
	con.env$con <- NULL
	con.env$blob_compression_type <- NULL

	invisible(TRUE)
}

#' @export
dbW_addSites <- function(dfLatitudeLongitudeLabel) {
	stopifnot(dbW_IsValid())

	dos_add <- sapply(dfLatitudeLongitudeLabel[, "Label"], function(x)
		is.null(suppressMessages(dbW_getSiteId(Label = x))))

	if (any(dos_add)) {
		sql <- "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)"
		rs <- DBI::dbSendStatement(con.env$con, sql)
		DBI::dbBind(rs, param = as.list(dfLatitudeLongitudeLabel[dos_add,
			c("Latitude", "Longitude", "Label")]))
		DBI::dbClearResult(rs)
	}

	invisible(TRUE)
}

#' @export
dbW_updateSites <- function(site_ids, new_data) {
	stopifnot(dbW_IsValid())

	dos_update <- dbW_has_siteIDs(site_ids)
	dos_add <- !dos_update

	if (any(dos_update)) {
		sql <- paste("UPDATE Sites SET Latitude=:Latitude, Longitude=:Longitude, Label=:Label",
			"WHERE Site_id=:id")
		rs <- DBI::dbSendStatement(con.env$con, sql)
		on.exit(DBI::dbClearResult(rs), add = TRUE)

		for (k in which(dos_update)) {
			DBI::dbBind(rs, param = c(as.list(new_data[k, c("Latitude", "Longitude", "Label")]),
				list(id = site_ids[k])))
		}
	}

	if (any(dos_add)) {
		stopifnot(dbW_addSites(new_data[dos_add, ]))
	}

	invisible(TRUE)
}

#' @export
dbW_addScenarios <- function(dfScenario, ignore.case = FALSE) {
	stopifnot(dbW_IsValid())

	dos_add <- !dbW_has_scenarios(dfScenario, ignore.case = ignore.case)

	if (any(dos_add)) {
		sql <- "INSERT INTO Scenarios VALUES(NULL, :sc)"
		rs <- DBI::dbSendStatement(con.env$con, sql)
		DBI::dbBind(rs, param = list(sc = unlist(dfScenario[dos_add])))
		DBI::dbClearResult(rs)
	}

	invisible(TRUE)
}

dbW_addWeatherDataNoCheck <- function(Site_id, Scenario_id, StartYear, EndYear,
	weather_blob) {
	sql <- paste0("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, ",
		"data) VALUES (", Site_id, ",", Scenario_id, ",", StartYear, ",", EndYear, ",",
		weather_blob, ")")
	DBI::dbExecute(con.env$con, sql)
}

#' @export
dbW_addWeatherData <- function(Site_id = NULL, lat = NULL, long = NULL,
	weatherFolderPath = NULL, weatherData = NULL, label = NULL, Scenario_id = NULL,
	ScenarioName = "Current", weather_tag = "weath", ignore.case = FALSE) {

	stopifnot(dbW_IsValid())

	has_weatherFolderPath <- !is.null(weatherFolderPath) && file.exists(weatherFolderPath)
	has_weatherData <- !is.null(weatherData) && is.list(weatherData) &&
		inherits(weatherData[[1]], "swWeatherData")
	if (!has_weatherFolderPath && !has_weatherData) {
		stop("'dbW_addWeatherData' requires either a folder path or weatherData.")
	}

	label <- if (!is.null(weatherFolderPath) && is.null(label)) {
			basename(weatherFolderPath)
		} else label
	has_siteID <- (!is.null(Site_id) && is.numeric(Site_id)) || !is.null(label)
	has_coords <- is.numeric(lat) && is.numeric(long)
	if (!has_siteID && !has_coords) {
		stop("'dbW_addWeatherData' has not enough info to identify/locate site.")
	}
	Site_id <- dbW_addSite(Site_id = Site_id, lat = lat, long = long, Label = label)

	has_scenario_id <- !is.null(Scenario_id) && is.numeric(Scenario_id) && Scenario_id > 0
	has_scenario_name <- inherits(ScenarioName, "character") && nchar(ScenarioName) > 0
	if (!has_scenario_id && !has_scenario_name) {
		stop("'dbW_addWeatherData' has not enough info to identify/locate scenario.")
	}
	if (has_scenario_id && !dbW_has_scenarioIDs(Scenario_id)) {
		stop("'dbW_addWeatherData': 'Scenario_id' does not exist.")
	}
	if (!has_scenario_id && has_scenario_name) {
		stopifnot(dbW_addScenarios(ScenarioName, ignore.case = ignore.case))
		sql <- paste0("SELECT id FROM Scenarios WHERE Scenario=", shQuote(ScenarioName),
				if (ignore.case) " COLLATE NOCASE")
		Scenario_id <- as.integer(DBI::dbGetQuery(con.env$con, sql))
	}

	if (is.null(weatherData)) {
		weatherData <- getWeatherData_folders(LookupWeatherFolder = weatherFolderPath,
			filebasename = weather_tag)
	}

	years <- get_years_from_weatherData(weatherData)
	dbW_addWeatherDataNoCheck(Site_id, Scenario_id, years[1], years[length(years)],
		weather_blob = dbW_weatherData_to_blob(weatherData, con.env$blob_compression_type))

	invisible(TRUE)
}


.create_dbW <- function(site_data, scenarios, scen_ambient) {
	sql <- "CREATE TABLE \"Meta\" (\"Desc\" TEXT PRIMARY KEY, \"Value\" TEXT)"
	DBI::dbExecute(con.env$con, sql)

	sql <- "INSERT INTO Meta VALUES(:Desc, :Value)"
	rs <- DBI::dbSendStatement(con.env$con, sql)
	DBI::dbBind(rs, param = list(
		Desc = c("Version", "Compression_type"),
		Value = c(con.env$dbW_version, con.env$blob_compression_type)))
	DBI::dbClearResult(rs)

	# Table of sites
	sql <- paste0("CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, ",
		"\"Latitude\" REAL, \"Longitude\" REAL, \"Label\" TEXT)")
	DBI::dbExecute(con.env$con, sql)
	# Table for weather data
	sql <- paste0("CREATE TABLE \"WeatherData\" (\"Site_id\" integer, ",
		"\"Scenario\" integer, \"StartYear\" integer, \"EndYear\" integer, \"data\" BLOB, ",
		"PRIMARY KEY (\"Site_id\", \"Scenario\"))")
	DBI::dbExecute(con.env$con, sql)
	# Table of scenario names
	sql <- "CREATE TABLE \"Scenarios\" (\"id\" integer PRIMARY KEY, \"Scenario\" TEXT)"
	DBI::dbExecute(con.env$con, sql)

	#---Add sites
	req_cols <- c("Latitude", "Longitude", "Label")
	temp <- sapply(req_cols, function(x) x %in% colnames(site_data))
	N <- NROW(site_data)
	if (N > 0 && temp) {
		stopifnot(dbW_addSites(dfLatitudeLongitudeLabel = site_data[, req_cols]))
	}

	#---Add scenario names
	scenarios <- c(scen_ambient, scenarios[!(scenarios == scen_ambient)])
	stopifnot(dbW_addScenarios(dfScenario = scenarios, ignore.case = FALSE))

	invisible(TRUE)
}


#' Create a weather database
#'
#' @section Details: A rSOILWAT2 weather database has the following format: \describe{
#'   \item{Table 'Meta'}{contains two fields 'Desc' and 'Value' which contain \itemize{
#'      \item the records 'Version' and 'Compression_type'}}
#'   \item{Table 'Sites'}{contains four fields 'Site_id', 'Latitude', 'Longitude', and
#'      'Label'}
#'   \item{Table 'WeatherData'}{contains five fields 'Site_id', 'Scenario' (i.e., the ID
#'      of the scenario), 'StartYear', 'EndYear', and 'data'}
#'   \item{Table 'Scenarios'}{contains two fields 'id' and 'Scenario' (i.e., the scenario
#'      name)}
#' }
#'
#' @param dbFilePath A character string. The file path of the weather database. This will
#'  be a file of type \code{sqlite3}. In-memory databases are not supported.
#' @param site_data A data.frame. The site data with column names "Latitude", "Longitude",
#'  and "Label".
#' @param scenarios A vector of character strings. The climate scenarios of which the
#'  first oneis enforced to be \code{scen_ambient}.
#' @param scen_ambient A character string. The first/default climate scenario.
#' @param compression_type A character string. The type of compression for the weather
#'  blob. See \code{\link{memCompress }} for the available choices.
#' @param verbose A logical value.
#'
#' @return \code{TRUE} on success; \code{FALSE} otherwise. If the file \code{dbFilePath}
#'   didn't already exist, but creating it failed, then the attempt will be removed.
#' @export
dbW_createDatabase <- function(dbFilePath = "dbWeatherData.sqlite3", site_data,
	scenarios, scen_ambient = "Current", compression_type = "gzip", verbose = FALSE) {

	if (file.exists(dbFilePath)) {
		if (verbose) {
			print(paste("'dbW_createDatabase': cannot create a new database because the file",
				basename(shQuote(dbFilePath)), "does already exist."))
		}
		return(FALSE)
	}

	temp <- dbW_setConnection(dbFilePath, create_if_missing = TRUE, verbose = verbose)
	if (!temp) {
		if (verbose) {
			print(paste("'dbW_createDatabase': was not able to create a new database and",
				"connect to the file", basename(shQuote(dbFilePath)), "."))
		}
		return(FALSE)
	}

	# Meta information
	temp <- eval(formals(memCompress)[[2]])
	if (missing(compression_type) || !(compression_type %in% temp)) {
		compression_type <- con.env$default_blob_compression_type
	}
	con.env$blob_compression_type <- compression_type

	# Create tables
	temp <- try(.create_dbW(site_data, scenarios, scen_ambient), silent = TRUE)
	res <- !inherits(temp, "try-error")

	if (!res) {
		if (verbose) {
			print(paste("'dbW_createDatabase': was not able to create a new database",
				basename(shQuote(dbFilePath)), "because of errors in the table data.",
				"The file will be deleted."))
		}
		dbW_disconnectConnection()
		unlink(dbFilePath)
	}

	res
}


#dataframe of columns folder, lat, long, label where label can equal folderName
#' @export
dbW_addFromFolders <- function(MetaData = NULL, FoldersPath, ScenarioName = "Current",
	weather_tag = "weath") {

	if (!is.null(MetaData)) {
		temp <- apply(MetaData, MARGIN = 1, function(x)
			dbW_addWeatherData(Site_id = NULL, lat = x[2], long = x[3],
			weatherFolderPath = file.path(FoldersPath, x[1]), weatherData = NULL, label = x[4],
			ScenarioName = ScenarioName, weather_tag = weather_tag))
	} else {
		files <- list.files(path = FoldersPath, pattern = weather_tag)
		temp <- lapply(files, function(x)
			dbW_addWeatherData(Site_id = NULL, lat = NULL, long = NULL,
			weatherFolderPath = file.path(FoldersPath, x), weatherData = NULL,
			ScenarioName = ScenarioName, weather_tag = weather_tag))
	}

	invisible(TRUE)
}

#' @export
dbW_deleteSite <- function(Site_id) {
	stopifnot(dbW_IsValid())

	DBI::dbExecute(con.env$con, paste0("DELETE FROM \"Sites\" WHERE Site_id=", Site_id))
	dbW_deleteSiteData(Site_id, Scenario_id = NULL)
}

#' @export
dbW_deleteSiteData <- function(Site_id, Scenario_id = NULL) {
	stopifnot(dbW_IsValid())

	sql <- if (is.null(Scenario_id)) {
			#Remove all data for this site
			paste0("DELETE FROM \"WeatherData\" WHERE Site_id=", Site_id)
		} else {
			# Remove data for specific scenario
			paste0("DELETE FROM \"WeatherData\" WHERE Site_id=", Site_id, " AND Scenario=",
				shQuote(Scenario_id))
		}
	DBI::dbExecute(con.env$con, sql)

	invisible(TRUE)
}


## ------ Conversion of weather data formats
#' Conversion: (Compressed) raw vector (e.g., SQL-retrieved blob) to (uncompressed) object
#'
#' The rSOILWAT2 SQlite-DB which manages daily weather data (each as a list of elements
#' of class 'swWeatherData'), uses internally (compressed) blobs. This function is used
#' to convert the blob object to the object used by rSOILWAT2's simulation functions.
#'
#' @param data_blob A raw vector
#' @param type A character string. One of c("gzip", "bzip2", "xz", "none").
#'
#' @seealso \code{\link{memDecompress}}, \code{\link{unserialize}}
#' @export
dbW_blob_to_weatherData <- function(data_blob, type = "gzip") {
	# RSQLite versions < 2.0 return a list of 'raw'; starting with v >= 2.0, the class changed
	#	to 'blob'

	if ((inherits(data_blob, "list") || inherits(data_blob, "blob")) &&
		inherits(data_blob[[1]], "raw") && length(data_blob) == 1) {
		data_blob <- data_blob[[1]]
	}

	unserialize(memDecompress(data_blob, type = type))
}

#' Conversion: R object to (compressed) SQL-blob-ready character vector
#'
#' The rSOILWAT2 SQLite-DB which manages daily weather data (each as a list of elements
#' of class 'swWeatherData'), uses internally (compressed) blobs. This function is used
#' to a list of daily weather data used by rSOILWAT2's simulation functions to a blob object
#' which can be inserted into a SQLite DB.
#'
#' @param weatherData A list of elements of class 'swWeatherData' or any suitable object.
#' @param type A character string. One of c("gzip", "bzip2", "xz", "none").
#'
#' @seealso \code{\link{memCompress}}, \code{\link{serialize}}
#' @export
dbW_weatherData_to_blob <- function(weatherData, type = "gzip") {
	paste0("x'", paste0(memCompress(serialize(weatherData, connection = NULL), type = type),
		collapse = ""), "'")
}



####################
# Conversion: reading of SOILWAT input text files to object of class 'swWeatherData'

#' rSOILWAT2 getWeatherData_folders
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
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#'
#' ## ------ Simulation with data prepared beforehand and separate weather data ------------
#' ## Read inputs from files on disk
#' sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files_v31.in")
#'
#' ## Read forcing weather data from files on disk (there are also functions to set up a
#' ##   SQLite database for the weather data)
#' sw_weath3 <- getWeatherData_folders(LookupWeatherFolder=file.path(path_demo, "Input"),
#'    weatherDirName="data_weather", filebasename="weath", startYear=1979, endYear=2010)
#'
#' ## List of the slots of the input objects of class 'swWeatherData'
#' str(sw_weath3, max.level=1)
#'
#' ## Execute the simulation run
#' \dontrun{sw_out3 <- sw_exec(inputData = sw_in3, weatherList = sw_weath3)}
#'
#' @export
getWeatherData_folders <- function(LookupWeatherFolder, weatherDirName = NULL,
	filebasename = "weath", startYear = NULL, endYear = NULL) {

	if (is.null(LookupWeatherFolder) || is.null(filebasename)) {
		stop("Need 'LookupWeatherFolder' and 'filebasename' ",
			"to locate weather data")
	}

	dir_weather <- if (is.null(weatherDirName)) {
			LookupWeatherFolder
		} else {
			file.path(LookupWeatherFolder, weatherDirName)
		}
	fweath <- tryCatch(list.files(dir_weather, pattern = filebasename), warning =
			function(w) stop("Path to weather data bad or filebasename not correct."))

	if (!endsWith(filebasename, ".")) {
		filebasename <- paste0(filebasename, ".")
	}
	years <- as.numeric(sub(pattern = filebasename, replacement = "", fweath))
	stopifnot(!anyNA(years))
	index <- select_years(years, startYear, endYear)

	weathDataList <- list()
	for (k in seq_along(index)) {
		weathDataList[[k]] <- swReadLines(new("swWeatherData", year = years[index[k]]),
			file.path(dir_weather, fweath[index[k]]))
	}
	names(weathDataList) <- as.character(years[index])

	weathDataList
}

# Conversion: object of class 'swWeatherData' to data.frame
#' @export
dbW_weatherData_to_dataframe <- function(weatherData){
	do.call(rbind, lapply(weatherData, FUN=function(x) {
							temp <- x@data
							Year <- rep(x@year, times=nrow(temp))
							cbind(Year, temp)
						}))
}

# Conversion: object of class 'swWeatherData' to matrix of monthly values (mean Tmax, mean Tmin, sum PPT)
#' @export
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
#' @export
dbW_dataframe_to_monthly <- function(dailySW) {
  temp <- apply(dailySW[, c("Year", "DOY")], 1, paste, collapse = "-")
  temp <- as.POSIXlt(temp, format = "%Y-%j", tz = "UTC")
  ytemp <- unique(temp$year)
  year <- rep(1900L + ytemp, each = 12)
  month <- rep(seq_len(12), times = length(ytemp))
  ltemp <- list(1L + temp$mon, dailySW[, "Year"])

  as.matrix(cbind(Year = year, Month = month,
    Tmax_C = as.vector(tapply(dailySW[, "Tmax_C"], INDEX = ltemp, FUN = mean)),
    Tmin_C = as.vector(tapply(dailySW[, "Tmin_C"], INDEX = ltemp, FUN = mean)),
    PPT_cm = as.vector(tapply(dailySW[, "PPT_cm"], INDEX = ltemp, FUN = sum))
  ))
}



#' @export
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
#' @export
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
#' @export
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
