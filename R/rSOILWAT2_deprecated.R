###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
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


#' Deprecated functions in package \pkg{rSOILWAT2}
#'
#' Executing a deprecated function will warn and tell you which function
#' replaces them.
#'
#' @inheritParams sw_exec
#' @param object An object of a \code{rSOILWAT2} class.
#' @param value A value to assign to a specific slot of the object.
#'
#' @name rSOILWAT2-deprecated
NULL


# nolint start.

dbW_getWeatherData_old <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	.Deprecated("dbW_getWeatherData")
	stopifnot(dbW_IsValid())

	if(is.null(Site_id) && is.null(Label) && is.null(lat) && is.null(long)) {
		stop("No way to locate weather data from input", call. = FALSE)
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
				stop("Wrong start or end year", call. = FALSE)
			}
		}
	}
	Site_id<-as.integer(Site_id)
	if(length(Site_id) == 0) {
		Site_id <- dbW_getSiteId(lat,long,Label)
	} else {
		if(!DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.", call. = FALSE)
		}
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- dbW_blob_to_weatherData_old(result$StartYear, result$EndYear, result$data, rSW2_glovars$blob_compression_type)
		if(inherits(data, "try-error")) stop(paste("Weather data for Site_id", Site_id, "is corrupted"), call. = FALSE)
	} else {
		stop(paste("Site_id for", Label, "not obtained."), call. = FALSE)
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

dbW_addWeatherData_old <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	.Deprecated("dbW_addWeatherData")
	stopifnot(dbW_IsValid())

  if (
    (
      is.null(weatherFolderPath) |
      ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)
    ) &
    (is.null(weatherData) | !is.list(weatherData) | !inherits(weatherData[[1]], "swWeatherData"))
  ) {
    stop("addWeatherDataToDataBase does not have folder path or weatherData to insert", call. = FALSE)
  }
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.", call. = FALSE)

	Site_id <- dbW_addSite(
		Site_id = Site_id,
		lat = lat,
		long = long,
		Label = if (!is.null(weatherFolderPath) && is.null(label)) basename(weatherFolderPath) else label)

	Scenarios <- DBI::dbReadTable(rSW2_glovars$con,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		temp <- DBI::dbGetQuery(rSW2_glovars$con, "SELECT MAX(id) FROM \"Scenarios\";")[1,1]
		scenarioID <- ifelse(is.na(temp),1,temp+1)
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(",scenarioID,",'",ScenarioName,"');",sep="")
		DBI::dbExecute(rSW2_glovars$con, SQL)
	}

	if(!is.null(weatherData)) {
		data_blob <- dbW_weatherData_to_blob_old(weatherData, rSW2_glovars$blob_compression_type)
		temp <- as.integer(names(weatherData))
		StartYear <- temp[1]
		EndYear <- temp[length(temp)]
		DBI::dbExecute(rSW2_glovars$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,", ",scenarioID,", ",StartYear,", ",EndYear,", ",data_blob,");",sep=""))
		#dbCommit(rSW2_glovars$con)
	} else {
		weath <- list.files(weatherFolderPath)
		years <- as.numeric(sub(pattern="weath.",replacement="",weath))
		weatherData <- list()
		for(j in 1:length(weath)) {
			year <- as.numeric(sub(pattern="weath.",replacement="",weath[j]))
			temp <- utils::read.csv(file.path(weatherFolderPath,weath[j]),header=FALSE,skip=2,sep="\t")
			weatherData[[j]] <- swReadLinesWeather3Vars(new("swWeatherData",year),file.path(weatherFolderPath,weath[j]))
		}
		StartYear <- years[1]
		EndYear <- years[length(years)]
		data_blob <- dbW_weatherData_to_blob_old(weatherData, rSW2_glovars$blob_compression_type)
		DBI::dbExecute(rSW2_glovars$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,", ",scenarioID,", ",StartYear,", ",EndYear,", ",data_blob,");",sep=""))
		#dbCommit(rSW2_glovars$con)
	}
}


swReadLinesWeather3Vars <- function(object, file) {
    .Deprecated("C_rSW2_readAllWeatherFromDisk")
    warning(
      "swReadLines works only with traditional weather data.", call. = FALSE
    )

    object@year <- as.integer(
      strsplit(
        x = basename(file),
        split = ".",
      fixed = TRUE
      )[[1]][2]
    )
    x <- utils::read.table(
      file,
      header = FALSE,
      comment.char = "#",
      blank.lines.skip = TRUE,
      sep = "\t"
    )
    stopifnot(ncol(x) != 4L)
    colnames(x) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    object@data[] <- NA
    object@data[, colnames(x)] <- as.matrix(x)

    object
}


# Conversion: SQL-blob to object of class 'swWeatherData'
dbW_blob_to_weatherData_old <- function(StartYear, EndYear, data_blob, type = "gzip") {
	.Deprecated("dbW_blob_to_weatherData")
	if ((inherits(data_blob, "list") || inherits(data_blob, "blob")) &&
		inherits(data_blob[[1]], "raw") && length(data_blob) == 1) {
		data_blob <- data_blob[[1]]
	}
	data <- strsplit(memDecompress(data_blob, type = type, asChar = TRUE), ";")[[1]]
	years <- StartYear:EndYear

	weatherData <- list()
	for (i in seq_along(years)) {
		zz <- textConnection(data[i])
		ydata <- utils::read.table(zz, header = FALSE, sep = ",", stringsAsFactors = FALSE)
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
	.Deprecated("dbW_weatherData_to_blob")
	dataString <- NULL
	string <- character(length = length(weatherData))
	for(i in seq_along(weatherData)) {
		rm(dataString)
		zz <- textConnection("dataString", "w")
		utils::write.table(x = weatherData[[i]]@data[, -1], file = zz, col.names = FALSE, sep = "," , row.names = FALSE)
		close(zz)
		string[i] <- paste(dataString, collapse = "\n")
	}
	string <- paste(string, collapse=";")

	paste0("x'", paste0(memCompress(string, type = type), collapse = ""), "'")
}



#' Add a new site description to a weather database
#'
#' @param Site_id An integer value. The identification number of the site.
#' @param lat A numeric value. The latitude of the site.
#' @param long A numeric value. The longitude of the site.
#' @param Label A character string. The name of the site.
#' @export
dbW_addSite <- function(Site_id = NULL, lat = NULL, long = NULL, Label = NULL) {
	.Deprecated("dbW_addSites")
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
		temp <- DBI::dbGetQuery(rSW2_glovars$con, sql)[1,1]
		Site_id <- if (is.na(temp)) 1L else {temp + 1}
		sql <- paste0("INSERT INTO Sites VALUES(", Site_id, ", ", lat, ", ", long, ", ",
			Label, ")")
		DBI::dbExecute(rSW2_glovars$con, sql)

	} else {
		# Site_id exists already
		sql <- paste("SELECT * FROM Sites WHERE Site_id=", Site_id)
		SiteData <- DBI::dbGetQuery(rSW2_glovars$con, sql)

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
				"label(", SiteData[1, "Label"], ":", Label, ").",
				call. = FALSE)
		}
	}

	Site_id
}

#' Calculate variables required to estimate percent C4 species in North America
#'
#' @param dailyTempMin A numeric vector. Time series of daily minimum air
#'   temperature `[C]`.
#' @param dailyTempMean A numeric vector. Time series of daily mean air
#'   temperature `[C]`.
#' @param simTime2 A list with two named elements. The elements are numeric
#'   vectors \var{month_ForEachUsedDay_NSadj} and
#'   \var{year_ForEachUsedDay_NSadj}; they are calculated internally
#'   if \code{NULL}; alternatively, they can be generated by a call to the
#'   function \code{\link[rSW2data]{simTiming_ForEachUsedTimeUnit}}.
#'
#' @return A named numeric vector of length 6.
#'
#' @references Teeri J.A., Stowe L.G. (1976) Climatic patterns and the
#'   distribution of C4 grasses in North America. Oecologia, 23, 1-12.
#'
#' @export
#' @md
sw_dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2) {
  .Deprecated("calc_SiteClimate")
  temp7 <- simTime2$month_ForEachUsedDay_NSadj == 7
  Month7th_MinTemp_C <- tapply(dailyTempMin[temp7],
    simTime2$year_ForEachUsedDay_NSadj[temp7], min)
  FrostFree_Days <- tapply(dailyTempMin, simTime2$year_ForEachUsedDay_NSadj,
    function(x) {
      temp <- rle(x > 0)
      if (any(temp$values)) max(temp$lengths[temp$values], na.rm = TRUE) else 0
    })

  # 18.333 C = 65 F with (65 - 32) * 5 / 9
  temp_base65F <- dailyTempMean - 18.333
  temp_base65F[temp_base65F < 0] <- 0
  DegreeDaysAbove65F_DaysC <- tapply(temp_base65F,
    simTime2$year_ForEachUsedDay_NSadj, sum)

  # if southern Hemisphere, then 7th month of last year is not included
  nyrs <- seq_along(Month7th_MinTemp_C)
  temp <- cbind(Month7th_MinTemp_C[nyrs], FrostFree_Days[nyrs],
    DegreeDaysAbove65F_DaysC[nyrs])
  res <- c(apply(temp, 2, mean), apply(temp, 2, sd))
  temp <- c("Month7th_NSadj_MinTemp_C",
    "LengthFreezeFreeGrowingPeriod_NSadj_Days",
    "DegreeDaysAbove65F_NSadj_DaysC")
  names(res) <- c(temp, paste0(temp, ".sd"))

  res
}

#' Calculate climate variables required to estimate percent cheatgrass cover
#' in North America
#'
#' @section Note: This function does not correct for northern/southern
#'   hemisphere.
#'
#' @param monthlyPPT_cm A numeric matrix of monthly precipitation values in
#'   centimeter. There are 12 rows, one for each month of the year;
#'   and there is one column for each year.
#' @param monthlyTempMean_C A numeric matrix of monthly mean temperature values
#'   in degree Celsius. There are 12 rows, one for each month of the year;
#'   and there is one column for each year.
#' @param monthlyTempMin_C A numeric matrix of monthly minimum temperature
#'   value sin degree Celsius. There are 12 rows, one for each month of the
#'   year; and there is one column for each year.
#'
#' @return A named numeric vector of length 6 with mean and standard deviation
#'   for \var{Month7th_PPT_mm}, \var{MeanTemp_ofDriestQuarter_C}, and
#'   \var{MinTemp_of2ndMonth_C}.
#'
#' @references Brummer, T. J., K. T. Taylor, J. Rotella, B. D. Maxwell,
#'   L. J. Rew, and M. Lavin. 2016. Drivers of Bromus tectorum Abundance in
#'   the Western North American Sagebrush Steppe. Ecosystems 19:986-1000.
#'
#' @export
sw_Cheatgrass_ClimVar <- function(monthlyPPT_cm,
  monthlyTempMean_C = NULL, monthlyTempMin_C = NULL) {
  .Deprecated("calc_SiteClimate")
  # Mean precipitation sum of seventh month of the season (i.e.,
  # July in northern hemisphere)
  Month7th_PPT_mm <- 10 * monthlyPPT_cm[7, ]
  nyrs <- seq_along(Month7th_PPT_mm)

  # Mean temperature of driest quarter (Bioclim variable 9)
  # see \code{link[dismo]{biovars}}
  if (!is.null(monthlyTempMean_C)) {
    wet <- t(apply(monthlyPPT_cm, 2, rSW2utils::moving_function,
      k = 3, win_fun = sum, na.rm = TRUE, circular = TRUE
    ))
    tmp <- t(apply(monthlyTempMean_C, 2, rSW2utils::moving_function,
      k = 3, win_fun = mean, na.rm = TRUE, circular = TRUE
    ))
    dryqrt <- cbind(
      seq_len(ncol(monthlyPPT_cm)),
      as.integer(apply(wet, 1, which.min))
    )
    MeanTemp_ofDriestQuarter_C <- tmp[dryqrt]

  } else {
    MeanTemp_ofDriestQuarter_C <- rep(NA, length(Month7th_PPT_mm))
  }

  # Minimum February temperature
  if (!is.null(monthlyTempMin_C)) {
    MinTemp_of2ndMonth_C <- monthlyTempMin_C[2, , ]
  } else {
    MinTemp_of2ndMonth_C <- rep(NA, length(Month7th_PPT_mm))
  }


  # Aggregate
  temp <- cbind(
    Month7th_PPT_mm[nyrs],
    MeanTemp_ofDriestQuarter_C[nyrs],
    MinTemp_of2ndMonth_C[nyrs]
  )

  res <- c(apply(temp, 2, mean), apply(temp, 2, sd))
  temp <- c("Month7th_PPT_mm", "MeanTemp_ofDriestQuarter_C",
    "MinTemp_of2ndMonth_C")
  names(res) <- c(temp, paste0(temp, "_SD"))

  res
}

#' Old way of calculating climate variables (previous to `v6.0.0`)
#'
#' @examples
#' # Compare new and old function
#' wdata <- rSOILWAT2::get_WeatherHistory(rSOILWAT2::sw_exampleData)
#'
#' fun_clim <- function(fun) {
#'   lapply(
#'     c(-90, 90),
#'     function(latitude) {
#'       fun(
#'         weatherList = wdata,
#'         do_C4vars = TRUE,
#'         do_Cheatgrass_ClimVars = TRUE,
#'         latitude = latitude
#'       )
#'     }
#'   )
#' }
#'
#' clim_old <- fun_clim(rSOILWAT2:::calc_SiteClimate_old)
#' clim_new <- fun_clim(rSOILWAT2::calc_SiteClimate)
#'
#' # Compare values assuming northern hemisphere:
#' all.equal(clim_old[[1]], clim_new[[1]])
#' # MAT_C: Mean relative difference: 2.740629e-05
#'
#' # Compare values assuming southern hemisphere:
#' all.equal(clim_old[[2]], clim_new[[2]])
#' # MAT_C: Mean relative difference: 2.740629e-05
#' # dailyC4vars: Mean relative difference: 0.05932631
#' # Cheatgrass_ClimVars: Mean relative difference: 0.707922
#'
#' # Difference in `MAT`:
#' cat(
#'   "MAT_C(old) = ", clim_old[[2]][["MAT_C"]],
#'   "vs. MAT_C(new) = ", clim_new[[2]][["MAT_C"]],
#'   fill = TRUE
#' )
#' # MAT_C(old) =  4.153896 vs. MAT_C(new) =  4.154009
#'
#' # Reason for differences in mean annual temperature `MAT`:
#' # The new version calculates the mean across years of
#' # means across days within year of mean daily temperature;
#' # previously, it was incorrectly calculated as the mean across all days.
#'
#'
#' # Differences in `dailyC4vars`:
#' print(
#'   cbind(
#'     old = clim_old[[2]][["dailyC4vars"]],
#'     new = clim_new[[2]][["dailyC4vars"]]
#'   )
#' )
#' #                                                    old        new
#' # Month7th_NSadj_MinTemp_C                    -27.243871 -27.199333
#' # LengthFreezeFreeGrowingPeriod_NSadj_Days     68.290323  72.600000
#' # DegreeDaysAbove65F_NSadj_DaysC               20.684935  21.357533
#' # Month7th_NSadj_MinTemp_C.sd                   5.241726   5.325365
#' # LengthFreezeFreeGrowingPeriod_NSadj_Days.sd  13.446669   9.586629
#' # DegreeDaysAbove65F_NSadj_DaysC.sd            19.755513  19.550419
#'
#' Explanation for different values:
#'
#' # Reason for differences in `dailyC4vars`:
#' # The new version adjusts years at locations in the southern hemisphere
#' # to start on July 1 of the previous calendar year;
#' # previously, the adjusted start date varied from July 1 to July 4.
#'
#'
#' # Differences in `Cheatgrass_ClimVars`:
#' print(
#'   cbind(
#'     old = clim_old[[2]][["Cheatgrass_ClimVars"]],
#'     new = clim_new[[2]][["Cheatgrass_ClimVars"]]
#'   )
#' )
#' #                                      old       new
#' # Month7th_PPT_mm                35.729032 65.916667
#' # MeanTemp_ofDriestQuarter_C     11.524859 11.401228
#' # MinTemp_of2ndMonth_C          -13.904600  6.545578
#' # Month7th_PPT_mm_SD             21.598367 35.285409
#' # MeanTemp_ofDriestQuarter_C_SD   7.171922  7.260852
#' # MinTemp_of2ndMonth_C_SD         2.618434  1.639640
#'
#' # Reason for differences in `Cheatgrass_ClimVars`:
#' # The new version now adjusts these variables for location by hemisphere;
#' # previously, they were calculated as if in the northern hemisphere
#' # regardless of actual location.
#'
#'
#' # Benchmarks: new version is about 20x faster
#' bm <- microbenchmark::microbenchmark(
#'   old = fun_clim(rSOILWAT2:::calc_SiteClimate_old),
#'   new = fun_clim(rSOILWAT2::calc_SiteClimate)
#' )
#'
#' # Unit: milliseconds
#' # expr       min         lq       mean     median         uq      max neval
#' # old  136.41207 149.689687 157.494004 154.114424 157.490437 277.3953   100
#' # new    3.07084   3.422651   6.992061   3.694008   4.082199 119.7300   100
#'
#'
#' @noRd
calc_SiteClimate_old <- function(weatherList, year.start = NA, year.end = NA,
  do_C4vars = FALSE, do_Cheatgrass_ClimVars = FALSE, simTime2 = NULL,
  latitude = 90) {
  .Deprecated("calc_SiteClimate")
  x <- dbW_weatherData_to_dataframe(weatherList)

  # Trim to requested years
  if (!is.na(year.start)) {
    x <- x[x[, "Year"] >= year.start, ]
  } else {
    year.start <- x[1, "Year"]
  }

  if (!is.na(year.end)) {
    x <- x[x[, "Year"] <= year.end, ]
  } else {
    year.end <- x[nrow(x), "Year"]
  }

  years <- unique(x[, "Year"])

  if (length(years) == 0) {
    stop("'calc_SiteClimate': no weather data available for ",
      "requested range of years", call. = FALSE)
  }

  # Mean daily temperature
  Tmean_C <- rowMeans(x[, c("Tmax_C", "Tmin_C")])

  # Get time sequence information
  is_simTime2_good <- !is.null(simTime2) &&
    identical(years, simTime2[["useyrs_NSadj"]]) &&
    !is.null(simTime2[["month_ForEachUsedDay"]])

  is_simTime2_good_for_C4vars <- if (do_C4vars) {
      if (is_simTime2_good) {
        !is.null(simTime2[["month_ForEachUsedDay_NSadj"]]) &&
        !is.null(simTime2[["year_ForEachUsedDay_NSadj"]])
      } else {
        FALSE
      }
    } else {
      TRUE
    }

  if (is_simTime2_good && is_simTime2_good_for_C4vars) {
    st2 <- simTime2
  } else {
    st2 <- rSW2data::simTiming_ForEachUsedTimeUnit(
      useyrs = years,
      sim_tscales = "daily",
      latitude = latitude,
      account_NorthSouth = do_C4vars
    )
  }

  # Calculate monthly values
  index <- st2[["month_ForEachUsedDay"]] + 100 * x[, "Year"]

  mon_Temp <- vapply(
    list(Tmean_C, x[, "Tmin_C"], x[, "Tmax_C"]),
    function(data) matrix(tapply(data, index, mean, na.rm = TRUE), nrow = 12),
    FUN.VALUE = matrix(NA_real_, nrow = 12, ncol = length(years))
  )

  mon_PPT <- matrix(tapply(x[, "PPT_cm"], index, sum, na.rm = TRUE), nrow = 12)

  list(
    # Calculate mean monthly values
    meanMonthlyTempC = apply(mon_Temp[, , 1, drop = FALSE], 1, mean,
      na.rm = TRUE),
    minMonthlyTempC = apply(mon_Temp[, , 2, drop = FALSE], 1, mean,
      na.rm = TRUE),
    maxMonthlyTempC = apply(mon_Temp[, , 3, drop = FALSE], 1, mean,
      na.rm = TRUE),
    meanMonthlyPPTcm = apply(mon_PPT, 1, mean, na.rm = TRUE),

    # Calculate mean annual values
    MAP_cm = sum(mon_PPT, na.rm = TRUE) / length(years),
    MAT_C = mean(Tmean_C, na.rm = TRUE),

    # If C4-variables are requested
    dailyTempMin = if (do_C4vars) x[, "Tmin_C"] else NA,
    dailyTempMean = if (do_C4vars) Tmean_C else NA,
    dailyC4vars = if (do_C4vars) {
      sw_dailyC4_TempVar(
        dailyTempMin = x[, "Tmin_C"],
        dailyTempMean = Tmean_C,
        simTime2 = st2
      )
    } else {
      NA
    },

    # If cheatgrass-variables are requested
    Cheatgrass_ClimVars = if (do_Cheatgrass_ClimVars) {
      sw_Cheatgrass_ClimVar(
        monthlyPPT_cm = mon_PPT,
        monthlyTempMean_C = mon_Temp[, , 1, drop = FALSE],
        monthlyTempMin_C = mon_Temp[, , 2, drop = FALSE]
      )
    } else {
      NA
    }
  )
}

#' Old function to estimate natural vegetation cover (previous to `v6.0.0`)
#'
#' @examples
#' # Compare new and old function
#' wdata <- rSOILWAT2::get_WeatherHistory(rSOILWAT2::sw_exampleData)
#' clim1 <- calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
#'
#' fun_pnvcov <- function(fun, clim, fix_issues = FALSE) {
#'   lapply(
#'     c(90, -90),
#'     function(latitude) {
#'       tmp_args <- list(
#'         MAP_mm = 10 * clim[["MAP_cm"]],
#'         MAT_C = clim[["MAT_C"]],
#'         mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
#'         mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
#'         dailyC4vars = clim[["dailyC4vars"]],
#'         isNorth = latitude >= 0
#'       )
#'       if (fix_issues) {
#'         tmp_args[["fix_issue218"]] <- TRUE
#'         tmp_args[["fix_issue219"]] <- TRUE
#'       }
#'       do.call(fun, tmp_args)
#'     }
#'   )
#' }
#'
#' cov_old <- fun_pnvcov(rSOILWAT2:::estimate_PotNatVeg_composition_old, clim1)
#' cov_old2 <- fun_pnvcov(
#'   rSOILWAT2:::estimate_PotNatVeg_composition_old,
#'   clim1,
#'   fix_issues = TRUE
#' )
#' cov_new <- fun_pnvcov(rSOILWAT2::estimate_PotNatVeg_composition, clim1)
#'
#' # Compare values as if northern hemisphere:
#' print(
#'   cbind(
#'     old = cov_old[[1]][["Rel_Abundance_L0"]],
#'     oldfixed = cov_old2[[1]][["Rel_Abundance_L0"]],
#'     new = cov_new[[1]][["Rel_Abundance_L0"]]
#'   )
#' )
#' #                       old  oldfixed       new
#' # Succulents      0.0000000 0.0000000 0.0000000
#' # Forbs           0.2608391 0.2608391 0.2608391
#' # Grasses_C3      0.4307061 0.4307061 0.4307061
#' # Grasses_C4      0.0000000 0.0000000 0.0000000
#' # Grasses_Annuals 0.0000000 0.0000000 0.0000000
#' # Shrubs          0.3084547 0.3084547 0.3084547
#' # Trees           0.0000000 0.0000000 0.0000000
#' # BareGround      0.0000000 0.0000000 0.0000000
#'
#' # Compare values as if southern hemisphere:
#' print(
#'   cbind(
#'     old = cov_old[[2]][["Rel_Abundance_L0"]],
#'     oldfixed = cov_old2[[2]][["Rel_Abundance_L0"]],
#'     new = cov_new[[2]][["Rel_Abundance_L0"]]
#'   )
#' )
#' #                        old  oldfixed       new
#' # Succulents      0.00000000 0.0000000 0.0000000
#' # Forbs           0.22804606 0.2707322 0.2707322
#' # Grasses_C3      0.52575060 0.6241618 0.6241618
#' # Grasses_C4      0.15766932 0.0000000 0.0000000
#' # Grasses_Annuals 0.00000000 0.0000000 0.0000000
#' # Shrubs          0.08853402 0.1051060 0.1051060
#' # Trees           0.00000000 0.0000000 0.0000000
#' # BareGround      0.00000000 0.0000000 0.0000000
#'
#' # Explanation for different values:
#' # `old` and `oldfixes` differ because of issue #218 (correction for
#' # `C4` grass cover was not carried out as documented) and issue #219
#' # (output incorrectly contained negative cover if fixed `SumGrasses_Fraction`
#' # caused that other fixed cover summed > 1);
#' # `oldfixed` and `new` produce identical output.
#'
#'
#' # Benchmarks: new version is about 15x faster
#' bm <- microbenchmark::microbenchmark(
#'   old = fun_pnvcov(rSOILWAT2:::estimate_PotNatVeg_composition_old, clim1),
#'   new = fun_pnvcov(rSOILWAT2::estimate_PotNatVeg_composition, clim1)
#' )
#'
#' # Unit: microseconds
#' # expr     min       lq      mean   median       uq     max neval
#' # old  450.820 467.7365 499.84000 503.8165 515.4235 711.459   100
#' # new   25.467  28.3930  33.95104  31.4155  39.8005  54.414   100
#'
#'
#' # issue 218: correction to C4 grass cover was not carried out as documented
#' for (fix_issue218 in c(FALSE, TRUE)) {
#'   tmp <- rSOILWAT2:::estimate_PotNatVeg_composition_old(
#'     MAP_mm = 10 * clim1[["MAP_cm"]],
#'     MAT_C = 10,
#'     mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'     mean_monthly_Temp_C = 5 + clim1[["meanMonthlyTempC"]],
#'     dailyC4vars = c(
#'       Month7th_NSadj_MinTemp_C = 3,
#'       LengthFreezeFreeGrowingPeriod_NSadj_Days = 150,
#'       DegreeDaysAbove65F_NSadj_DaysC = 110
#'     ),
#'     fix_issue218 = fix_issue218
#'   )
#'   print(tmp[["Grasses"]])
#' }
#' #      Grasses_C3      Grasses_C4 Grasses_Annuals
#' #       0.4522766       0.5477234       0.0000000
#' #      Grasses_C3      Grasses_C4 Grasses_Annuals
#' #               1               0               0
#'
#'
#' # issue 219: output incorrectly contained negative cover
#' # if fixed `SumGrasses_Fraction` caused that other fixed cover summed > 1
#' # expect error with issue 219 fixed
#' for (fix_issue219 in c(FALSE, TRUE)) {
#'   tmp <- try(
#'     rSOILWAT2:::estimate_PotNatVeg_composition_old(
#'       MAP_mm = 10 * clim1[["MAP_cm"]],
#'       MAT_C = clim1[["MAT_C"]],
#'       mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'       mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]],
#'       dailyC4vars = clim1[["dailyC4vars"]],
#'       fix_shrubs = TRUE,
#'       Shrubs_Fraction = 0.5,
#'       fix_sumgrasses = TRUE,
#'       SumGrasses_Fraction = 0.7,
#'       fix_issue219 = fix_issue219
#'     ),
#'     silent = TRUE
#'   )
#'   if (inherits(tmp, "try-error")) {
#'     print(as.character(tmp))
#'   } else {
#'     print(tmp[["Rel_Abundance_L1"]])
#'   }
#' }
#' #      SW_TREES      SW_SHRUB      SW_FORBS      SW_GRASS SW_BAREGROUND
#' #           0.0           0.5          -0.2           0.7           0.0
#' # [1] "Error in rSOILWAT2:::estimate_PotNatVeg_composition_old ..."
#'
#' @noRd
estimate_PotNatVeg_composition_old <- function(MAP_mm, MAT_C,
  mean_monthly_ppt_mm, mean_monthly_Temp_C, dailyC4vars = NULL,
  isNorth = TRUE, shrub_limit = 0.2,
  fix_succulents = FALSE, Succulents_Fraction = NA,
  fix_sumgrasses = FALSE, SumGrasses_Fraction = NA,
  fix_annuals = TRUE, Annuals_Fraction = 0,
  fix_C4grasses = FALSE, C4_Fraction = NA,
  fix_C3grasses = FALSE, C3_Fraction = NA,
  fix_shrubs = FALSE, Shrubs_Fraction = NA,
  fix_forbs = FALSE, Forbs_Fraction = NA,
  fix_trees = TRUE, Trees_Fraction = 0,
  fix_BareGround = TRUE, BareGround_Fraction = 0,
  fill_empty_with_BareGround = TRUE,
  warn_extrapolation = TRUE,
  fix_issue218 = FALSE,
  fix_issue219 = FALSE
) {
  .Deprecated("estimate_PotNatVeg_composition")
  veg_types <- c(
    "Succulents", "Forbs",
    "Grasses_C3", "Grasses_C4", "Grasses_Annuals",
    "Shrubs", "Trees",
    "BareGround"
  )
  Nveg <- length(veg_types)

  isuc <- 1 # succulents
  ifor <- 2 # forbs
  igc3 <- 3 # grasses-C3
  igc4 <- 4 # grasses-C4
  igan <- 5 # grasses-annuals
  ishr <- 6 # shrubs
  itre <- 7 # trees
  ibar <- 8 # bare-ground

  veg_cover <- rep(0, Nveg)

  # groups without climate-equations, i.e., always set to a specific value
  iset <- c(igan, itre, ibar)

  # groups with climate-equations to estimate relative abundance
  iestim <- c(igc4, igc3, ishr, ifor, isuc)
  igrasses <- c(igc3, igc4, igan)


  #--- Get the user specified fractions: input cover fraction values:
  input_cover <- rep(NA, Nveg)

  # Groups that are either fixed or 0, i.e., cannot be NA = not estimated
  input_cover[igan] <- if (fix_annuals) {
    rSW2utils::finite01(Annuals_Fraction)
  } else {
    0
  }
  input_cover[itre] <- if (fix_trees) {
    rSW2utils::finite01(Trees_Fraction)
  } else {
    0
  }
  input_cover[ibar] <- if (fix_BareGround) {
    rSW2utils::finite01(BareGround_Fraction)
  } else {
    0
  }

  # Groups that are either fixed or estimated based on climate-relationships
  input_cover[igc4] <- if (fix_C4grasses) C4_Fraction else NA
  input_cover[igc3] <- if (fix_C3grasses) C3_Fraction else NA
  input_cover[ishr] <- if (fix_shrubs) Shrubs_Fraction else NA
  input_cover[ifor] <- if (fix_forbs) Forbs_Fraction else NA
  input_cover[isuc] <- if (fix_succulents) Succulents_Fraction else NA

  # treat negative input values as if NA
  input_cover <- rSW2utils::cut0Inf(input_cover, val = NA)


  #--- Check individual components if the sum of grasses is fixed
  fix_sumgrasses <- fix_sumgrasses && isTRUE(!is.na(SumGrasses_Fraction))

  if (fix_sumgrasses) {
    SumGrasses_Fraction <- rSW2utils::cut0Inf(SumGrasses_Fraction, val = 0)

    input_sum_grasses <- rSW2utils::replace_NAs_with_val(
      x = sum(input_cover[igrasses], na.rm = TRUE),
      val_replace = 0
    )

    add_sum_grasses <- SumGrasses_Fraction - input_sum_grasses

    if (add_sum_grasses < 0) {
      stop(
        "'estimate_PotNatVeg_composition': ",
        "User defined grass values including C3, C4, and annuals ",
        "sum to more than user defined total grass cover.",
        call. = FALSE
      )

    }

    ids_to_estim_grasses <- is.na(input_cover[igrasses])

    if (add_sum_grasses > 0) {
      if (sum(ids_to_estim_grasses) == 1) {
        # One grass component to estimate: difference from rest
        input_cover[igrasses[ids_to_estim_grasses]] <-
          SumGrasses_Fraction - input_sum_grasses

        add_sum_grasses <- 0
      }

    } else {
      # No grass component to add: set all to zero
      input_cover[igrasses[ids_to_estim_grasses]] <- 0
    }
  }


  #--- Decide if all fractions are sufficiently defined or if they need to be
  # estimated based on climate reltionships
  input_sum <- sum(input_cover, na.rm = TRUE)

  if (isTRUE(fix_issue219)) {
    if (fix_sumgrasses && isTRUE(add_sum_grasses > 0)) {
      input_sum <- input_sum + add_sum_grasses
    }
  }

  ifixed <- unique(c(iset, which(!is.na(input_cover))))

  ids_to_estim <- which(is.na(input_cover))
  n_to_estim <- length(ids_to_estim)

  if (input_sum > 1) {
    stop(
      "'estimate_PotNatVeg_composition': ",
      "User defined relative abundance values sum to more than ",
      "1 = full land cover.",
      call. = FALSE
    )
  }


  #--- Incomplete surface cover
  veg_cover <- input_cover

  if (n_to_estim <= 1) {
    #--- Less than one component to estimate: no need for equations

    if (n_to_estim == 0) {
      #--- All fixed, nothing to estimate
      if (fill_empty_with_BareGround) {
        veg_cover[ibar] <- 1 - sum(veg_cover[-ibar], na.rm = TRUE)

      } else if (input_sum < 1) {
        stop(
          "'estimate_PotNatVeg_composition': ",
          "User defined relative abundance values are all fixed, ",
          "but their sum is smaller than 1 = full land cover.",
          call. = FALSE
        )
      }

    } else if (n_to_estim == 1) {
      #--- One value to estimate: difference from rest
      veg_cover[ids_to_estim] <- 1 - input_sum
    }

  } else {
    #---Potential natural vegetation
    # i.e., (input_sum < 1 && sum(is.na(input_cover)) > 1) is TRUE;
    # thus, estimate relative abundance fractions based on climate relationships

    if (MAP_mm <= 1) {
      # No precipitation ==> no vegetation, only bare-ground
      # TODO: what about fog?
      veg_cover[] <- 0
      veg_cover[ibar] <- 1

    } else {

      estim_cover <- rep(NA, Nveg)

      # Estimate climate variables
      if (isNorth) {
        Months_WinterTF <- c(12, 1:2)
        Months_SummerTF <- 6:8
      } else {
        Months_WinterTF <- 6:8
        Months_SummerTF <- c(12, 1:2)
      }

      # Fraction of precipitation falling during summer/winter months
      ppt.SummerToMAP <- sum(mean_monthly_ppt_mm[Months_SummerTF]) / MAP_mm
      ppt.WinterToMAP <- sum(mean_monthly_ppt_mm[Months_WinterTF]) / MAP_mm

      # Temperature in July minus temperature in January
      therm_amp <- mean_monthly_Temp_C[Months_SummerTF[2]] -
        mean_monthly_Temp_C[Months_WinterTF[2]]

      if (warn_extrapolation) {
        # Adjust climate variables to limits underlying the data used to develop
        # equations Paruelo & Lauenroth (1996): "The selected sites cover a
        #   range of MAT from 2 C to 21.2 C and a range of precipitation (MAP)
        #   from 117 to 1011 mm"

        # MAT limits:
        if (MAT_C < 1) {
          # Note: MAT = 1 C as limit instead of 2 C based on empirical testing;
          # also because log(x) is undefined for x < 0 and results in negative
          # values for x < 1. Hence the threshold of 1.
          warning(
            "Equations used outside supported range (2 - 21.2 C): ",
           "MAT = ", round(MAT_C, 2), " C reset to 1 C.",
           call. = FALSE
          )
          MAT_C <- 1
        }

        if (MAT_C > 21.2) {
          warning(
            "Equations used outside supported range (2 - 21.2 C): ",
            "MAT = ", round(MAT_C, 2), " C.",
            call. = FALSE
          )
        }

        if (MAP_mm < 117 || MAP_mm > 1011) {
          warning(
            "Equations used outside supported range (117-1011 mm): ",
            "MAP = ", round(MAP_mm), " mm.",
            call. = FALSE
          )
        }
      }


      # 1. step: estimate relative abundance based on
      # Paruelo & Lauenroth (1996): shrub climate-relationship:
      if (MAP_mm < 1) {
        estim_cover[ishr] <- 0
      } else {
        # if not enough winter precipitation for a given MAP, then equation
        # results in negative values which we set to 0
        estim_cover[ishr] <- rSW2utils::cut0Inf(
          1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP,
          val = 0
        )
      }

      # Paruelo & Lauenroth (1996): C4-grass climate-relationship:
      if (MAT_C <= 0) {
        estim_cover[igc4] <- 0
      } else {
        # if either MAT < 0 or not enough summer precipitation or
        # too cold for a given MAP, then equation results in negative values
        # which we set to 0
        estim_cover[igc4] <- rSW2utils::cut0Inf(
          -0.9837 + 0.000594 * MAP_mm +
            1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C),
          val = 0
        )

        # 2. step: Teeri JA, Stowe LG (1976)
        # This equations give percent species/vegetation -> use to limit
        # Paruelo's C4 equation, i.e., where no C4 species => C4 abundance == 0
        do_c4_correction <- if (isTRUE(fix_issue218)) {
          !is.null(dailyC4vars)
        } else {
          is.list(dailyC4vars) # always FALSE because `dailyC4vars` is vector
        }

        if (do_c4_correction) {
          if (dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0) {
            grass_c4_species <- 0
          } else {
            x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9 / 5 + 32
            x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9 / 5
            x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
            grass_c4_species <- as.numeric(
              (1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100
            )
          }

          if (grass_c4_species <= rSW2_glovars[["tol"]]) {
            estim_cover[igc4] <- 0
          }
        }
      }

      # Paruelo & Lauenroth (1996): C3-grass climate-relationship:
      if (ppt.WinterToMAP <= 0) {
        c3_in_grassland <- c3_in_shrubland <- NA
      } else {
        # if not enough winter precipitation or too warm for a
        # given MAP, then equation results in negative values which we set to 0
        c3_in_grassland <- rSW2utils::cut0Inf(
          1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1,
          val = 0
        )
        c3_in_shrubland <- rSW2utils::cut0Inf(
          1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2,
          val = 0
        )
      }

      temp <- estim_cover[ishr] >= shrub_limit && !is.na(estim_cover[ishr])
      estim_cover[igc3] <- ifelse(temp, c3_in_shrubland, c3_in_grassland)

      # Paruelo & Lauenroth (1996): forb climate-relationship:
      if (MAP_mm < 1 || MAT_C <= 0) {
        estim_cover[ifor] <- NA
      } else {
        estim_cover[ifor] <- rSW2utils::cut0Inf(
          -0.2035 + 0.07975 * log(MAP_mm) - 0.0623 * log(MAT_C),
          val = 0
        )
      }

      # Paruelo & Lauenroth (1996): succulent climate-relationship:
      if (therm_amp <= 0 || ppt.WinterToMAP <= 0) {
        estim_cover[isuc] <- NA
      } else {
        estim_cover[isuc] <- rSW2utils::cut0Inf(
          -1 + 1.20246 * therm_amp ^ -0.0689 * ppt.WinterToMAP ^ -0.0322,
          val = 0
        )
      }

      # 3. step:
      ngood <- sum(!is.na(estim_cover[iestim]))

      # Any remaining NAs are set to 0
      estim_cover[iestim] <- rSW2utils::replace_NAs_with_val(
        x = estim_cover[iestim],
        val_replace = 0
      )

      if (!fill_empty_with_BareGround && ngood <= 1) {
        #--- Hack if some of the equations produced NAs:
        # [these rules are made up arbitrarily by drs, Nov 2012]:
        # If no or only one successful equation, then add
        #   100% C3 if MAT < 10 C,
        #   100% shrubs if MAP < 600 mm, and
        #   100% C4 if MAT >= 10C & MAP >= 600 mm
        if (MAP_mm < 600) {
          estim_cover[ishr] <- 1 + estim_cover[ishr]
        }

        if (MAT_C < 10) {
          estim_cover[igc3] <- 1 + estim_cover[igc3]
        }

        if (MAT_C >= 10 && MAP_mm >= 600) {
          estim_cover[igc4] <- 1 + estim_cover[igc4]
        }
      }


      # 4. step: put all together:
      # 4-i) groups with set values (iset) and groups with estimable but
      #    fixed values (iestim & !is.na)
      veg_cover[ifixed] <- input_cover[ifixed]

      # 4-ii) rescale grass components to fixed total grass cover
      if (fix_sumgrasses && add_sum_grasses > 0) {
        ids_to_estim_grasses <- intersect(ids_to_estim, igrasses)
        n_to_estim_grasses <- sum(ids_to_estim_grasses)

        estim_grasses_cover_sum <- sum(estim_cover[ids_to_estim_grasses])

        if (estim_grasses_cover_sum > 0) {
          estim_cover[ids_to_estim_grasses] <-
            estim_cover[ids_to_estim_grasses] *
            add_sum_grasses / estim_grasses_cover_sum

        } else if (n_to_estim_grasses > 0) {
          # We estimated zero grass cover, but some was required
          # --> divide requested amount evenly
          estim_cover[ids_to_estim_grasses] <-
            add_sum_grasses / n_to_estim_grasses

          warning(
            "'estimate_PotNatVeg_composition': ",
            "Total grass cover set, but no grass cover estimated; ",
            "requested cover evenly divided among grass types.",
            call. = FALSE
          )
        }
      }

      # 4-iii) groups with values to estimate (iestim & is.na):
      veg_cover[ids_to_estim] <- estim_cover[ids_to_estim]

      if (fix_sumgrasses) {
        # Fix grasses and remove them from estimable
        ifixed <- unique(c(ifixed, igrasses))
        ids_to_estim <- setdiff(ids_to_estim, igrasses)
      }

      # Scale fractions to 0-1 with a sum equal to 1 (if needed)
      tot_veg_cover_sum <- sum(veg_cover)

      if (abs(tot_veg_cover_sum - 1) > rSW2_glovars[["tol"]]) {

        estim_cover_sum <- sum(estim_cover[ids_to_estim])

        if (estim_cover_sum > 0) {
          # Scale estimable fractions so that total sums to 1, but
          # scaling doesn't affect those that are fixed
          veg_cover[ids_to_estim] <- veg_cover[ids_to_estim] *
            (1 - sum(veg_cover[ifixed])) / estim_cover_sum

        } else {
          # cover to estimate is 0 and fixed_cover_sum < 1
          if (fill_empty_with_BareGround && !fix_BareGround) {
            # ==> fill land cover up with bare-ground
            veg_cover[ibar] <- 1 - sum(veg_cover[-ibar])

          } else {
            stop(
              "'estimate_PotNatVeg_composition': ",
              "The estimated vegetation cover values are 0, ",
              "the user fixed relative abundance values sum to less than 1, ",
              "and bare-ground is fixed. ",
              "Thus, the function cannot compute ",
              "complete land cover composition.",
              call. = FALSE
            )
          }
        }
      }

    }
  }

  names(veg_cover) <- veg_types

  # Scale relative grass components to one (or set to 0)
  c3c4ann <- veg_cover[igrasses]
  grass_fraction <- sum(c3c4ann)

  if (grass_fraction > 0) {
    c3c4ann <- c3c4ann / grass_fraction
  }

  # Return values
  temp <- unname(veg_cover)

  list(
    # Full resolution: suitable for STEPWAT2
    Rel_Abundance_L0 = veg_cover,

    # SOILWAT2 land cover types:
    Rel_Abundance_L1 = c(
      SW_TREES = temp[itre],
      SW_SHRUB = temp[ishr],
      SW_FORBS = temp[ifor] + temp[isuc],
      SW_GRASS = grass_fraction,
      SW_BAREGROUND = temp[ibar]
    ),

    # Relative contributions of sub-types to the grass type
    Grasses = c3c4ann
  )
}



## ------ Scanning of SOILWAT input text files ------
readCharacter <- function(text, showWarnings = FALSE) {
  .Deprecated("SOILWAT2's read functionality")
  tmp <- strsplit(x = text, split = "\t")[[1]][1]
  unlist(strsplit(x = tmp, split = " "))[1]
}

readInteger <- function(text,showWarnings=FALSE) {
  .Deprecated("SOILWAT2's read functionality")
  tmp <- suppressWarnings(as.integer(strsplit(x=text,split="\t")[[1]][1]))
  if(is.na(tmp)) {
    if(showWarnings) warning("Line: ", text, call. = FALSE)
    if(showWarnings) {
      warning("Not formatted with \t. Going to try [space].", call. = FALSE)
    }
    tmp <- suppressWarnings(as.integer(strsplit(x=text, split=" ")[[1]][1]))
    if(is.na(tmp)) {
      stop("Bad Line. Or Bad line numbers.", call. = FALSE)
    }
  }
  return(tmp)
}

readLogical <- function(text,showWarnings=FALSE) {
  .Deprecated("SOILWAT2's read functionality")
  tmp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split="\t")[[1]][1])))
  if(is.na(tmp)) {
    if(showWarnings) warning("Line: ",text, call. = FALSE)
    if(showWarnings) {
      warning("Not formatted with \t. Going to try [space].", call. = FALSE)
    }
    tmp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split=" ")[[1]][1])))
    if(is.na(tmp)) {
      stop("Bad Line. Or Bad line numbers.", call. = FALSE)
    }
  }
  return(tmp)
}

readNumeric <- function(text,showWarnings=FALSE) {
  .Deprecated("SOILWAT2's read functionality")
  tmp <- suppressWarnings(as.numeric(strsplit(x=text,split="\t")[[1]][1]))
  if(is.na(tmp)) {
    if(showWarnings) warning("Line: ",text, call. = FALSE)
    if(showWarnings) {
      warning("Not formatted with \t. Going to try [space].", call. = FALSE)
    }
    tmp <- suppressWarnings(as.numeric(strsplit(x=text,split=" ")[[1]][1]))
    if(is.na(tmp)) {
      stop("Bad Line. Or Bad line numbers.", call. = FALSE)
    }
  }
  return(tmp)
}

readNumerics <- function(text,expectedArgs,showWarnings=FALSE) {
  .Deprecated("SOILWAT2's read functionality")
  tmp <- strsplit(x=text,split="\t")[[1]]
  tmp <- tmp[tmp != ""] #get rid of extra spaces
  if(length(tmp) > expectedArgs) tmp <- tmp[1:expectedArgs] #get rid of comment?
  tmp <- suppressWarnings(as.numeric(tmp))
  if(any(is.na(tmp))) {
    if(showWarnings && any(is.na(tmp))) warning("Line: ",text, call. = FALSE)
    if(showWarnings && any(is.na(tmp))) {
      warning("Not formatted with \t. Going to try [space].", call. = FALSE)
    }
    tmp <- strsplit(x=text,split="\t")[[1]][1] #remove comment
    tmp <- strsplit(x=tmp,split=" ")[[1]]
    tmp <- tmp[tmp!=""] #remove extra spaces
    tmp <- suppressWarnings(as.numeric(tmp[1:expectedArgs]))
    if(any(is.na(tmp))) {
      #last try. tried set by \t then by space. Now try both
      tmp <- strsplit(x=text,split=" ",fixed=T)[[1]]
      tmp <- unlist(strsplit(x=tmp,split="\t",fixed=T))
      tmp <- tmp[tmp!=""] #remove extra spaces
      tmp <- suppressWarnings(as.numeric(tmp[1:expectedArgs]))
      if(any(is.na(tmp))) stop("Bad Line. Or Bad line numbers.", call. = FALSE)
    }
  }
  if(length(tmp) != expectedArgs) {
    if(showWarnings) warning("Line: ",text, call. = FALSE)
    stop(paste("Expected ",expectedArgs," Got ",length(tmp),sep=""), call. = FALSE)
  }
  return(tmp)
}



#------ Other deprecated functions ------


#' @rdname rSOILWAT2-deprecated
#' @export
sw_outputData <- function(inputData) {
  .Deprecated("Store return value of `rSOILWAT2::sw_exec()`.")

  dir_prev <- getwd()
  on.exit(setwd(dir_prev), add = TRUE)

  res <- .Call(C_onGetOutputDeprecated, inputData)

  slot(res, "version") <- rSW2_version()
  slot(res, "timestamp") <- rSW2_timestamp()

  res
}


#------ Vegetation-type related methods & functions ------

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_grass",
  function(object) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_grass")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_shrub",
  function(object) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_shrub")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_tree",
  function(object) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_tree")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_forb",
  function(object) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_forb")
  }
)


#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_grass<-",
  function(object, value) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_grass<-")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_shrub<-",
  function(object, value) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_shrub<-")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_tree<-",
  function(object, value) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_tree<-")
  }
)

#' @rdname rSOILWAT2-deprecated
setGeneric(
  "swProd_MonProd_forb<-",
  function(object, value) {
    .Deprecated("swProd_MonProd_veg")
    standardGeneric("swProd_MonProd_forb<-")
  }
)


#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_grass",
  "swProd",
  function(object) {
    object@MonthlyVeg[[
      1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_GRASS"]]
    ]]
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_shrub",
  "swProd",
  function(object) {
    object@MonthlyVeg[[
      1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_SHRUB"]]
    ]]
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_tree",
  "swProd",
  function(object) {
    object@MonthlyVeg[[
      1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_TREES"]]
    ]]
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_forb",
  "swProd",
  function(object) {
    object@MonthlyVeg[[
      1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_FORBS"]]
    ]]
  }
)


#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_grass",
  signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_GRASS"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_shrub",
  signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_SHRUB"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_tree",
  signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_TREES"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_forb",
  signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_FORBS"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
  }
)


#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_grass",
  signature = "swInputData",
  function(object) swProd_MonProd_grass(object@prod)
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_shrub",
  signature = "swInputData",
  function(object) swProd_MonProd_shrub(object@prod)
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_tree",
  signature = "swInputData",
  function(object) swProd_MonProd_tree(object@prod)
)

#' @rdname rSOILWAT2-deprecated
#' @export
setMethod(
  "swProd_MonProd_forb",
  signature = "swInputData",
  function(object) swProd_MonProd_forb(object@prod)
)


#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_grass",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_grass(object@prod) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_shrub",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_shrub(object@prod) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_tree",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_tree(object@prod) <- value
    object
  }
)

#' @rdname rSOILWAT2-deprecated
#' @export
setReplaceMethod(
  "swProd_MonProd_forb",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_forb(object@prod) <- value
    object
  }
)


# nolint end.
