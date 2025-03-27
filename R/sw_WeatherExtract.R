#--- sw_meteo_obtain ------

#' Extract meteorological data from external source and format for `rSOILWAT2`
#'
#' @param x Identifying information for the station/location for which
#' meteorological data is requested, see details.
#' @param start_year A integer value. The first calendar year of the simulation.
#' @param end_year A integer value. The last calendar year of the simulation.
#' @param rawdata A data object retrieved previously.
#' @param ... Additional arguments are ignored.
#'
#' @return A list with the following elements
#'   * `"metadata"`: site metadata (if available)
#'   * `"rawdata"`: data as downloaded (or argument `rawdata` if provided)
#'   * `"weatherDF"`: data frame with weather data formatted for `rSOILWAT2`
#'   * `"vals_missing"`: logical matrix indicating which weather values in
#'     `"weatherDF"` are missing
#'   * `"desc_rsds"`: solar radiation descriptor (if available)
#'   * `"use_cloudCoverMonthly"`: flag indicating need for monthly cloud cover
#'   * `"use_windSpeedMonthly"`: flag indicating need for monthly wind speed
#'   * `"use_humidityMonthly"`: flag indicating need for monthly humidity
#'   * `"dailyInputFlags"`: logical vector indicating which weather variables
#'     contain at least some values (see [calc_dailyInputFlags()])
#'
#' @md
#' @name sw_meteo_obtain
NULL



sw_download_DayMet <- function(longitude, latitude, years) {
  stopifnot(
    requireNamespace("daymetr")
  )

  res <- try(
    daymetr::download_daymet(
      lat = latitude,
      lon = longitude,
      start = years[[1L]],
      end = years[[length(years)]],
      internal = TRUE,
      force = TRUE,
      simplify = FALSE,
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    stop("Download DayMet data failed: ", res, call. = FALSE)
  }

  res
}


#' @rdname sw_meteo_obtain
#'
#' @section Details:
#' `sw_meteo_obtain_DayMet()` uses data from
#' [`DayMet ORNL DAAC`](https://daymet.ornl.gov/single-pixel/)
#' via [daymetr::download_daymet()].
#' The argument `x` is a named vector with `"longitude"` and `"latitude"` in
#' decimal degrees.
#' `"DayMet"` uses a `"noleap"` (`"365_day"`) calendar which is converted
#' to a `"standard"` calendar (i.e., one with leap days); this results in
#' missing values on inserted leap days (see code example).
#' `"DayMet"` does not contain wind speed which is a required input.
#'
#' @examples
#' ## Example: Daymet weather for "Mccracken Mesa" location
#' ##   (see `mm_scan[["metadata"]]`)
#' if (requireNamespace("curl") && curl::has_internet()) {
#'   mm_dm <- rSOILWAT2::sw_meteo_obtain_DayMet(
#'     x = c(longitude = -109.3378, latitude = 37.44671),
#'     start_year = 2015,
#'     end_year = 2023
#'   )
#'
#'   # Fill in missing values on leap days
#'   mm_dm_wdata <- rSOILWAT2::dbW_fixWeather(mm_dm[["weatherDF"]])
#'
#'   # Prepare a SOILWAT2 simulation
#'   swin <- rSOILWAT2::sw_exampleData
#'   rSOILWAT2::swYears_EndYear(swin) <- 2023
#'   rSOILWAT2::swYears_StartYear(swin) <- 2015
#'   swin@weather@desc_rsds <- mm_dm[["desc_rsds"]]
#'   swin@weather@use_cloudCoverMonthly <- mm_dm[["use_cloudCoverMonthly"]]
#'   swin@weather@use_windSpeedMonthly <- mm_dm[["use_windSpeedMonthly"]]
#'   swin@weather@use_humidityMonthly <- mm_dm[["use_humidityMonthly"]]
#'   swin@weather@dailyInputFlags <- mm_dm[["dailyInputFlags"]]
#'
#'   # Set mean monthly climate values to missing
#'   # (except wind speed which is missing in DayMet)
#'   rSOILWAT2::swCloud_Humidity(swin)[] <- NA_real_
#'   rSOILWAT2::swCloud_WindSpeed(swin)[] <- rep(1.5, times = 12L)
#'   rSOILWAT2::swCloud_SkyCover(swin)[] <- NA_real_
#'
#'   # Obtain atmospheric CO2 concentration
#'   rSOILWAT2::swCarbon_Scenario(swin) <- "CMIP6_historical|CMIP6_SSP119"
#'   rSOILWAT2::swCarbon_CO2ppm(swin) <- rSOILWAT2::lookup_annual_CO2a(
#'     start = 2015,
#'     end = 2023,
#'     name_co2 = rSOILWAT2::swCarbon_Scenario(swin)
#'   )
#'
#'   # Run simulation (after providing remaining inputs, e.g., soils)
#'   swout <- try(
#'     rSOILWAT2::sw_exec(
#'       inputData = swin,
#'       weatherList = mm_dm_wdata[["weatherData"]],
#'       quiet = TRUE
#'     ),
#'     silent = TRUE
#'   )
#' }
#'
#' @md
#' @export
sw_meteo_obtain_DayMet <- function(
  x,
  start_year,
  end_year,
  rawdata = NULL,
  ...
) {

  #--- Download NRCS station data (if needed) ------
  if (is.null(rawdata)) {
    rawdata <- try(
      sw_download_DayMet(
        longitude = x[["longitude"]],
        latitude = x[["latitude"]],
        years = start_year:end_year
      ),
      silent = TRUE
    )
  }

  #--- Prepare requested station data ------

  #--- * Daily weather data (update with additional variables) ------
  stopifnot(
    !inherits(rawdata, "try-error"),
    length(rawdata[["data"]][["tmax..deg.c."]]) > 0L
  )

  tmpm <- data.frame(
    Year = rawdata[["data"]][["year"]],
    DOY = rawdata[["data"]][["yday"]],
    Tmax_C = rawdata[["data"]][["tmax..deg.c."]],
    Tmin_C = rawdata[["data"]][["tmin..deg.c."]],
    PPT_cm = rawdata[["data"]][["prcp..mm.day."]] / 10, # convert mm -> cm
    actVP_kPa = 1e-3 * rawdata[["data"]][["vp..Pa."]], # convert Pa -> kPa
    shortWR =
      1e-6 * rawdata[["data"]][["srad..W.m.2."]] *
      rawdata[["data"]][["dayl..s."]] # this will be desc_rsds = 1
  )


  # Add missing variables and missing days to complete full calendar years
  dm_weather <- dbW_convert_to_GregorianYears(
    weatherData = upgrade_weatherDF(tmpm),
    new_startYear = start_year,
    new_endYear = end_year,
    type = "asis"
  )

  vars_meteo <- weather_dataColumns()
  dif <- calc_dailyInputFlags(dm_weather, name_data = vars_meteo)

  list(
    metadata = rawdata[c("site", "tile", "latitude", "longitude", "altitude")],
    rawdata = rawdata,
    weatherDF = dm_weather,
    vals_missing = is_missing_weather(dm_weather[, vars_meteo, drop = FALSE]),
    desc_rsds = 1L,
    use_cloudCoverMonthly = FALSE, # use radiation instead
    use_windSpeedMonthly = TRUE,
    use_humidityMonthly = FALSE, # use vapor pressure instead
    dailyInputFlags = dif
  )
}



sw_download_SCAN <- function(nrcs_site_code, years) {
  stopifnot(requireNamespace("soilDB"))

  res <- try(
    soilDB::fetchSCAN(
      site.code = nrcs_site_code,
      year = years,
      report = "SCAN",
      timeseries = "Daily"
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error") || is.null(res)) {
    stop("Download NRCS station data failed: ", res, call. = FALSE)
  }

  res
}



#' @rdname sw_meteo_obtain
#'
#' @section Details:
#' `sw_meteo_obtain_SCAN()` uses data from
# nolint start: line_length_linter.
#' [`USDA-NRCS` `SCAN/SNOTEL` stations](https://www.nrcs.usda.gov/resources/data-and-reports/soil-climate-analysis-network)
# nolint end: line_length_linter.
#' via [soilDB::fetchSCAN()].
#' The argument `x` takes a `NRCS` `SCAN/SNOTEL` station code.
#'
#' @examples
#' ## Example: SCAN station "Mccracken Mesa"
#' if (requireNamespace("curl") && curl::has_internet()) {
#'   mm_scan <- try(
#'     rSOILWAT2::sw_meteo_obtain_SCAN(
#'       x = 2140, # SCAN station code
#'       start_year = 2015,
#'       end_year = 2023
#'     )
#'   )
#' }
#'
#' if (
#'   exists("mm_scan") && !inherits(mm_scan, "try-error") &&
#'   exists("mm_dm") &&
#'   requireNamespace("graphics")
#' ) {
#'   vars <- c("Tmax_C", "Tmin_C", "PPT_cm")
#'   par_prev <- graphics::par(mfrow = grDevices::n2mfrow(length(vars)))
#'
#'   for (k in seq_along(vars)) {
#'     graphics::plot(
#'       x = mm_scan[["weatherDF"]][[vars[[k]]]],
#'       y = mm_dm[["weatherDF"]][[vars[[k]]]],
#'       xlab = paste("SCAN", vars[[k]]),
#'       ylab = paste("DayMet", vars[[k]])
#'     )
#'     graphics::abline(a = 0, b = 1, col = "red")
#'   }
#'
#'   graphics::par(par_prev)
#' }
#'
#' @md
#' @export
sw_meteo_obtain_SCAN <- function(
  x,
  start_year,
  end_year,
  rawdata = NULL,
  ...
) {

  #--- Download NRCS station data (if needed) ------
  if (is.null(rawdata)) {
    rawdata <- try(
      sw_download_SCAN(x, years = start_year:end_year),
      silent = TRUE
    )
  }

  #--- Prepare requested station data ------

  #--- * Daily weather data (update with additional variables) ------
  stopifnot(
    !inherits(rawdata, "try-error"),
    nrow(rawdata[["TMAX"]]) > 0L,
    nrow(rawdata[["TMIN"]]) > 0L,
    nrow(rawdata[["PRCP"]]) > 0L
  )


  tmp <- merge(
    # Daily air temperature maximum [C]
    rawdata[["TMAX"]][, c("Date", "value"), drop = FALSE],
    # Daily air temperature minimum [C]
    rawdata[["TMIN"]][, c("Date", "value"), drop = FALSE],
    by = "Date",
    suffixes = c("_tmaxC", "_tminC"),
    all = TRUE
  )

  tmp <- merge(
    tmp,
    # Daily precipitation amount [in]
    rawdata[["PRCP"]][, c("Date", "value"), drop = FALSE],
    by = "Date",
    all = TRUE
  )

  tmp_dates <- as.POSIXlt(tmp[["Date"]])

  tmpm <- data.frame(
    Date = tmp[["Date"]],
    Year = 1900L + tmp_dates$year,
    DOY = 1L + tmp_dates$yday,
    Tmax_C = tmp[["value_tmaxC"]],
    Tmin_C = tmp[["value_tminC"]],
    PPT_cm = 1 / 2.54 * tmp[["value"]], # convert [in] -> [cm]
    stringsAsFactors = FALSE
  )


  if (nrow(rawdata[["WSPDV"]]) > 0L) {
    stopifnot("WSPDV is not yet implemented")

    tmp2 <- merge(
      tmpm,
      # Wind speed
      rawdata[["WSPDV"]][, c("Date", "value"), drop = FALSE],
      by = "Date",
      all = TRUE
    )
  }


  # Add missing variables and missing days to complete full calendar years
  scan_weather <- dbW_convert_to_GregorianYears(
    weatherData = upgrade_weatherDF(tmpm),
    new_startYear = start_year,
    new_endYear = end_year,
    type = "asis"
  )

  vars_meteo <- weather_dataColumns()
  dif <- calc_dailyInputFlags(scan_weather, name_data = vars_meteo)

  list(
    metadata = rawdata[["metadata"]],
    rawdata = rawdata,
    weatherDF = scan_weather,
    vals_missing = is_missing_weather(scan_weather[, vars_meteo, drop = FALSE]),
    desc_rsds = NA_integer_,
    use_cloudCoverMonthly = TRUE,
    use_windSpeedMonthly = unname(!dif[which(vars_meteo == "windSpeed_mPERs")]),
    use_humidityMonthly = TRUE,
    dailyInputFlags = dif
  )
}
