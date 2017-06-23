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

## ------SQLite weather database function: upgrade

#' Weather database upgrade functions
#'
#' @param dbWeatherDataFile A character string. The path to the weather database file.
#' @param fbackup A character string. The path to where the weather database should be
#'  backed up. If \code{NULL}, then '_copy' is appended to \code{dbWeatherDataFile}.
#'
#' @name dbW_upgrade
NULL

#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_to_rSOILWAT2} upgrades a weather database that was
#' created under'Rsoilwat31' to the current package version 'rSOILWAT2'
#'
#' @inheritParams dbW_upgrade
#' @param check_all A logical value. If \code{TRUE}, then every record is checked;
#'  otherwise, only the first record is checked for the package version.
#'
#' @export
dbW_upgrade_to_rSOILWAT2 <- function(dbWeatherDataFile, fbackup = NULL, check_all = FALSE) {
	print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile),
		"to package 'rSOILWAT2'"))

	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
	on.exit(DBI::dbDisconnect(con), add = TRUE)

	# Check database version
	sql <- "SELECT Value FROM Meta WHERE Desc=\'Version\'"
	temp <- DBI::dbGetQuery(con, sql)[1, 1]
	v_dbW <- numeric_version(as.character(temp))
	if (!identical(v_dbW, numeric_version(con.env$dbW_version))) {
		stop("'dbW_upgrade_to_rSOILWAT2': requires database version ",
			con.env$dbW_version, "; this database is version ", v_dbW)
	}

	# Extract compression type
	sql <- "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'"
	type <- DBI::dbGetQuery(con, sql)[1, 1]

	# Backup copy
	backup_copy(dbWeatherDataFile, fbackup)

	# Check what data are there
	print(paste(Sys.time(), ": examine database", basename(dbWeatherDataFile)))

	ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData")
	n_ids <- NROW(ids)
	has_old_notloaded <- FALSE

	if (n_ids > 0) {
	  for (k in seq_len(n_ids)) {
			print(paste(Sys.time(), ": processing", k, "out of", n_ids))

	    # extract old blob
			sql <- paste("SELECT * FROM WeatherData WHERE Site_id =", ids[k, 1],
				"AND Scenario =", ids[k, 2])
			res <- DBI::dbGetQuery(con, sql)
			wd <- rSOILWAT2::dbW_blob_to_weatherData(res$data, type)

			# Check that the old package is available and load it
      if (k == 1L || check_all) {
        wd_class <- if (inherits(wd, "list")) class(wd[[1]]) else class(wd)
        if (!(wd_class == "swWeatherData")) {
		      stop("'dbW_upgrade_to_rSOILWAT2': cannot update a weather database with ",
			      "data of class ", shQuote(wd_class), "; instead class 'swWeatherData' is ",
            "required.")
        }

        wd_pkg <- attr(wd_class, "package")
        if (!has_old_notloaded) {
          if (wd_pkg == "rSOILWAT2") {
            if (!check_all) {
              print(paste("Class of weather database data is already from package",
                "'rSOILWAT2'; nothing to upgrade."))
              return(invisible(NULL))
            }

          } else {
            has_old_notloaded <- !suppressPackageStartupMessages(requireNamespace(wd_pkg))
            if (!has_old_notloaded) {
              warning("The package ", shQuote(wd_pkg), " which created the weather data, is",
                " not available on this system.")
            }
          }
        }
      }

			if (!(wd_pkg == "rSOILWAT2")) {
        # convert weather data to class of new package
        wd_new <- lapply(wd, function(x) {
          x_data <- slot(x, "data")
          colnames(x_data) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
          new("swWeatherData", year = slot(x, "year"), data = x_data)})
        names(wd_new) <- sapply(wd, slot, "year")

        # update data in weather database
        blob_new <- dbW_weatherData_to_blob(wd_new, type)
        sql <- paste("UPDATE WeatherData SET data =", blob_new, "WHERE Site_id =",
          ids[k, 1], " AND Scenario =", ids[k, 2])
        DBI::dbExecute(con, sql)
      }
    }
	}

	# Checks and clean-up
	check_updatedDB(con)

	invisible(0)
}



#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v3to31} upgrades a weather database from version
#'  '3.0.x' to '3.1.0'
#'
#' @inheritParams dbW_upgrade
#' @param type_new The type of compression used to compress the weather blobs. See
#'  \code{\link{memCompress}}.
#'
#' @export
dbW_upgrade_v3to31 <- function(dbWeatherDataFile, fbackup = NULL, type_new = "gzip") {
	print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile), "to version 3.1.0"))

	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
	v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con, "SELECT Value FROM Meta WHERE Desc=\'Version\'")[1, 1])), silent = TRUE)

	if (inherits(v_dbW, "try-error") || v_dbW >= "3.1.0" && v_dbW < "3.0.0") {
		warning("The function 'dbW_upgrade_v3to31' upgrades weather databases from version 3.0.z to 3.1.0; this database is version ", v_dbW)
		return(invisible(0))
	}

	type_old <- as.character(DBI::dbGetQuery(con, "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'")[1, 1])
	if (!(type_new %in% eval(formals(memCompress)[[2]]))) {
		warning("The upgraded weather database cannot store BLOBs with compression type: ", type_new)
		warning("Instead, the previous compression type: ", type_old, " will be used")
	}

	# Backup copy
	backup_copy(dbWeatherDataFile, fbackup)

	# Update weather blobs
	ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData;")
	n_ids <- NROW(ids)

	blob_to_weatherData_old <- function(StartYear, EndYear, data_blob, type = "gzip") {
		if (inherits(data_blob, "list") || inherits(data_blob, "blob")) {
			data_blob <- data_blob[[1]]
		}
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


	if (n_ids > 0) {
		for (i in seq_len(n_ids)) {
			print(paste(Sys.time(), ":", i, "out of", n_ids))

			result <- DBI::dbGetQuery(con, paste0("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id =", ids[i, 1], " AND Scenario =", ids[i, 2], ";"))
			weatherData <- blob_to_weatherData_old(result$StartYear, result$EndYear, result$data, type_old)
			if (inherits(weatherData, "try-error") || length(weatherData) == 0) {
				warning("Weather data for Site_id = ", ids[i, 1], " and Scenario = ", ids[i, 2], " is missing or is corrupted")

			} else {
				blob_new <- paste0("x'", paste0(memCompress(serialize(weatherData, connection = NULL), type = type_new), collapse = ""), "'")
				DBI::dbExecute(con, paste0("UPDATE WeatherData SET data =", blob_new, "WHERE Site_id =", ids[i, 1], " AND Scenario =", ids[i, 2], ";"))
				rm(blob_new)

			}
		}
	}


	# Update version number
	DBI::dbExecute(con, "UPDATE Meta SET Value = \'3.1.0\' WHERE Desc = \'Version\';")
	if (type_new != type_old)
		DBI::dbExecute(con, paste0("UPDATE Meta SET Value = \'", type_new, "\' WHERE Desc = \'Compression_type\';"))

	# Checks and clean-up
	check_updatedDB(con)
	DBI::dbDisconnect(con)

	invisible(0)
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v2to3} upgrades a weather database from version
#'  '2.x.y' to '3.0.0'
#'
#' @inheritParams dbW_upgrade
#'
#' @export
dbW_upgrade_v2to3 <- function(dbWeatherDataFile, fbackup = NULL) {
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
	v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con, "SELECT Version FROM Version;"))), silent = TRUE)

	if (inherits(v_dbW, "try-error") || v_dbW >= "3" && v_dbW < "2") {
		warning("The function 'dbW_upgrade_v2to3' upgrades weather databases from version 2.y.z to 3.0.0; this database is version ", v_dbW)
		return(invisible(0))
	}

	print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile), "to version 3.0.0"))

	# Backup copy
	backup_copy(dbWeatherDataFile, fbackup)

	# Add new table 'Meta'
	DBI::dbExecute(con, "CREATE TABLE \"Meta\" (\"Desc\" TEXT PRIMARY KEY, \"Value\" TEXT);")

	rs <- DBI::dbSendStatement(con, "INSERT INTO Meta VALUES(:Desc, :Value)")
	DBI::dbBind(rs, param = list(
		Desc = c("Version", "Compression_type"),
		Value = c("3.0.0", "gzip")))
	DBI::dbClearResult(rs)

	# Delete old table 'Version'
	DBI::dbExecute(con, "DROP TABLE Version;")

	# Checks and clean-up
	check_updatedDB(con)
	DBI::dbDisconnect(con)

	invisible(0)
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v1to2} upgrades a weather database from version
#'  '1.x.y' to '2.0.0'
#'
#' @inheritParams dbW_upgrade
#'
#' @export
dbW_upgrade_v1to2 <- function(dbWeatherDataFile, fbackup = NULL, SWRunInformation) {
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
	v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con, "SELECT Version FROM Version;"))), silent = TRUE)

	if (inherits(v_dbW, "try-error") || v_dbW >= "2" && v_dbW < "1") {
		warning("The function 'dbW_upgrade_v1to2' upgrades weather databases from version 1.y.z to 2.0.0; this database is version ", v_dbW)
		return(invisible(0))
	}

	print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile), "to version 2.0.0; be patient, this may take considerable time depending on the size of the database"))

	# Backup copy
	backup_copy(dbWeatherDataFile, fbackup)

	runIDs_sites <- which(SWRunInformation[, "Include_YN"] > 0)

	# Rename table 'Sites' as 'Sites_old'
	DBI::dbExecute(con, "ALTER TABLE Sites RENAME TO Sites_old;")


	# Add new table 'Sites'
	DBI::dbExecute(con, "CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL, \"Longitude\" REAL, \"Label\" TEXT);")

	site_data <- data.frame(Site_id = SWRunInformation$site_id,
					Latitude = SWRunInformation$Y_WGS84,
					Longitude = SWRunInformation$X_WGS84,
					Label = SWRunInformation$WeatherFolder,
					stringsAsFactors = FALSE)

	if (NROW(site_data) > 0 && sapply(c("Site_id", "Latitude", "Longitude", "Label"), function(x) x %in% colnames(site_data))) {
		# Default values
		MetaData <- data.frame(Site_id = seq_len(max(site_data[, "Site_id"])),
								Latitude = -999, Longitude = -999,
								Label = NA, stringsAsFactors = FALSE)
		# Fill in data
		im <- match(site_data[runIDs_sites, "Site_id"], MetaData[, "Site_id"])
		MetaData[im, c("Latitude", "Longitude", "Label")] <- site_data[runIDs_sites, c("Latitude", "Longitude", "Label")]

		rs <- DBI::dbSendStatement(con, "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)")
		DBI::dbBind(rs, param = as.list(MetaData[, c("Latitude", "Longitude", "Label")]))
		DBI::dbClearResult(rs)
	}


	# Create step by step a new WeatherData table: Update Site_id based on matching labels of tables 'Sites' and 'Sites_old'
	old_ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData;")
	index_old <- sort.int(unique(old_ids[, "Site_id"]), decreasing = TRUE) # this assumes that all new Site_id are larger than the old ones

	if (length(old_ids) > 0) {
		for (iold in index_old) {
			print(paste(Sys.time(), ":", iold, "out of", length(index_old)))

			site_old <- DBI::dbGetQuery(con, paste("SELECT Latitude, Longitude, Label FROM Sites_old WHERE Site_id =", iold, ";"))

			if (nrow(site_old) == 1) {
				site_new <- DBI::dbGetQuery(con, paste0("SELECT * FROM Sites WHERE Label = \'", site_old$Label, "\' AND Latitude = ", site_old$Latitude, " AND Longitude = ", site_old$Longitude, ";"))

				if (nrow(site_new) == 1) {
					#DBI::dbGetQuery(con, paste("SELECT Site_id, Scenario, StartYear, EndYear FROM WeatherData WHERE Site_id =", iold, ";"))
					DBI::dbExecute(con, paste("UPDATE WeatherData SET Site_id =", site_new$Site_id, "WHERE Site_id =", iold, ";"))
				} else {
					str(site_new)
					warning("No updated record for old site_id = ", iold)
				}
			} else {
				str(site_old)
				warning("No record for old site_id = ", iold)
			}
		}

		cur_ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData;")
	}
	DBI::dbExecute(con, "DROP TABLE Sites_old;")

	# Update version number
	DBI::dbExecute(con, "DROP TABLE Version;")
	DBI::dbExecute(con, "CREATE TABLE \"Version\" (\"Version\" TEXT);")
	DBI::dbExecute(con, "INSERT INTO Version (Version) VALUES (\"2.0.0\");")

	# Checks and clean-up
	check_updatedDB(con)
	DBI::dbDisconnect(con)

	invisible(0)
}


#' @export
check_updatedDB <- function(con) {
	print(paste0(Sys.time(), ": check database integrity"))
	print(DBI::dbExecute(con, "PRAGMA integrity_check;"))

	print(DBI::dbExecute(con, "PRAGMA index_list(WeatherData);"))
	print(DBI::dbExecute(con, "PRAGMA index_info(sqlite_autoindex_WeatherData_1);"))
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{backup_copy} creates a backup copy of a weather database file
#'  if not already present
#'
#' @inheritParams dbW_upgrade
#'
#' @export
backup_copy <- function(dbWeatherDataFile, fbackup = NULL) {
	print(paste(Sys.time(), ": backup database", basename(dbWeatherDataFile)))

	dbWeatherDataFile <- normalizePath(dbWeatherDataFile)

	dir_old <- getwd()
	on.exit(setwd(dir_old))
	setwd(dirname(dbWeatherDataFile))

	if (is.null(fbackup)) {
		fbackup <- strsplit(basename(dbWeatherDataFile), split = ".", fixed = TRUE)[[1]]
		fbackup <- paste0(paste(fbackup[1], collapse = ""), "_copy.", fbackup[length(fbackup)])
	} else {
		fbackup <- basename(fbackup)
	}

	res <- if (file.exists(dbWeatherDataFile) && !file.exists(fbackup)) {
		system2("cp", args = c(basename(dbWeatherDataFile), fbackup))
	} else if (!file.exists(dbWeatherDataFile) && file.exists(fbackup)) {
		system2("cp", args = c(fbackup, basename(dbWeatherDataFile)))
	}

	res
}
