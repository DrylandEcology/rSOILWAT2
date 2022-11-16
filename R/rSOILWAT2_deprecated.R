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


dbW_getWeatherData_old <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	.Deprecated("dbW_getWeatherData")
	stopifnot(dbW_IsValid())

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
		if(!DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.")
		}
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- dbW_blob_to_weatherData_old(result$StartYear, result$EndYear, result$data, rSW2_glovars$blob_compression_type)
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

dbW_addWeatherData_old <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	.Deprecated("dbW_addWeatherData")
	stopifnot(dbW_IsValid())

	if( (is.null(weatherFolderPath) | ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)) & (is.null(weatherData) | !is.list(weatherData) | class(weatherData[[1]]) != "swWeatherData") ) stop("addWeatherDataToDataBase does not have folder path or weatherData to insert")
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.")

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
			weatherData[[j]] <- swReadLines(new("swWeatherData",year),file.path(weatherFolderPath,weath[j]))
		}
		StartYear <- years[1]
		EndYear <- years[length(years)]
		data_blob <- dbW_weatherData_to_blob_old(weatherData, rSW2_glovars$blob_compression_type)
		DBI::dbExecute(rSW2_glovars$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,", ",scenarioID,", ",StartYear,", ",EndYear,", ",data_blob,");",sep=""))
		#dbCommit(rSW2_glovars$con)
	}
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
				"label(", SiteData[1, "Label"], ":", Label, ").")
		}
	}

	Site_id
}

#' Calculate variables required to estimate percent C4 species in North America
#'
#' @return A named numeric vector of length 6.
#' @references Teeri J.A., Stowe L.G. (1976) Climatic patterns and the
#'   distribution of C4 grasses in North America. Oecologia, 23, 1-12.
#'
#' @export
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
#' # Difference in MAT:
#' cat(
#'   "MAT_C(old) = ", clim_old[[2]][["MAT_C"]],
#'   "vs. MAT_C(new) = ", clim_new[[2]][["MAT_C"]],
#'   fill = TRUE
#' )
#' # MAT_C(old) =  4.153896 vs. MAT_C(new) =  4.154009
#'
#' # Differences in dailyC4vars:
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
#'  Within the old versions of rSOILWAT2 (rSOILWAT2 < v7.0.0), a different method
#'  for deciding when a year started and ended for the southern hemisphere was used.
#'  Using the old method, a given year had the chance to start on any day
#'  from July 1st to July 4th ultimately ignoring anywhere from zero to three days
#'  for that year. With the new method, the program now correctly starts on July 1st
#'  and handles a leap year properly. This difference in number of days within the year
#'  is enough to change overall values slightly.
#'
#' # Differences in Cheatgrass_ClimVars:
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
      "requested range of years")
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
