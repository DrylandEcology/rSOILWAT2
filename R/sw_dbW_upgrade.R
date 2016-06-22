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


## ------SQLite weather database function: upgrade

dbW_upgrade_v1to2 <- function(dbWeatherDataFile, fbackup = NULL, SWRunInformation) {
	dbW_setConnection(dbWeatherDataFile)
	v_dbW <- dbW_version()
	
	if (v_dbW >= "1" && v_dbW < "2") {
		warning("The function 'dbW_upgrade_v1to2' upgrades weather databases from version 1.y.z to 2.0.0; this database is version ", v_dbW)
		return(invisible(0))
	}
	
	print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile), "to version 2.0.0; be patient, this may take considerable time depending on the size of the database"))

	dir_old <- getwd()
	on.exit(setwd(dir_old))
	setwd(dirname(dbWeatherDataFile))
	
	# Backup copy
	if (is.null(fbackup)) {
		temp <- strsplit(basename(dbWeatherDataFile), split = ".", fixed = TRUE)[[1]]
		temp <- paste0(paste(temp[1], collapse = ""), "_copy.", temp[length(temp)])
		
		fbackup <- file.path(dirname(dbWeatherDataFile), temp)
	}

	if (file.exists(dbWeatherDataFile) && !file.exists(fbackup)) {
		system2("cp", args = c(dbWeatherDataFile, fbackup))
	} else if (!file.exists(dbWeatherDataFile) && file.exists(fbackup)) {
		system2("cp", args = c(fbackup, dbWeatherDataFile))
	}


	runIDs_sites <- which(SWRunInformation[, "Include_YN"] > 0)

	# Rename table 'Sites' as 'Sites_old'
	dbGetQuery(env.con$con, "ALTER TABLE Sites RENAME TO Sites_old;")


	# Add new table 'Sites'
	dbGetQuery(env.con$con, "CREATE TABLE \"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL, \"Longitude\" REAL, \"Label\" TEXT);")

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
	
		dbGetPreparedQuery(env.con$con, "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)", bind.data = MetaData)
	}


	# Create step by step a new WeatherData table: Update Site_id based on matching labels of tables 'Sites' and 'Sites_old'
	old_ids <- dbGetQuery(env.con$con, "SELECT Site_id, Scenario FROM WeatherData;")
	index_old <- sort.int(unique(old_ids[, "Site_id"]), decreasing = TRUE) # this assumes that all new Site_id are larger than the old ones

	if (length(old_ids) > 0) {
		for (iold in index_old) {
			print(paste(Sys.time(), ":", iold, "out of", length(index_old)))
	
			site_old <- dbGetQuery(env.con$con, paste("SELECT Latitude, Longitude, Label FROM Sites_old WHERE Site_id =", iold, ";"))
	
			if (nrow(site_old) == 1) {
				site_new <- dbGetQuery(env.con$con, paste0("SELECT * FROM Sites WHERE Label = \'", site_old$Label, "\' AND Latitude = ", site_old$Latitude, " AND Longitude = ", site_old$Longitude, ";"))
		
				if (nrow(site_new) == 1) {
					#dbGetQuery(env.con$con, paste("SELECT Site_id, Scenario, StartYear, EndYear FROM WeatherData WHERE Site_id =", iold, ";"))
					dbGetQuery(env.con$con, paste("UPDATE WeatherData SET Site_id =", site_new$Site_id, "WHERE Site_id =", iold, ";"))
				} else {
					str(site_new)
					warning("No updated record for old site_id = ", iold)
				}
			} else {
				str(site_old)
				warning("No record for old site_id = ", iold)
			}
		}
	
		cur_ids <- dbGetQuery(env.con$con, "SELECT Site_id, Scenario FROM WeatherData;")
	}
	dbGetQuery(env.con$con, "DROP TABLE Sites_old;")

	# Update version number
	dbGetQuery(env.con$con, "DROP TABLE Version;")
	dbGetQuery(env.con$con, "CREATE TABLE \"Version\" (\"Version\" TEXT);")
	dbGetQuery(env.con$con, "INSERT INTO Version (Version) VALUES (\"2.0.0\");")

	# Checks and clean-up
	print(paste(Sys.time(), ": check database integrity"))
	print(dbGetQuery(env.con$con, "PRAGMA integrity_check;"))

	print(dbGetQuery(env.con$con, "PRAGMA index_list(WeatherData);"))
	print(dbGetQuery(env.con$con, "PRAGMA index_info(sqlite_autoindex_WeatherData_1);"))

	invisible(0)
}
