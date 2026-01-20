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


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018)
###############################################################################

##############################################################################


#' List names of currently implemented daily weather variables
#' @return A vector of daily weather variable names.
#' @export
weather_dataColumns <- function() {
  c(
    "Tmax_C", "Tmin_C", "PPT_cm",
    "cloudCov_pct",
    "windSpeed_mPERs", "windSpeed_east_mPERs", "windSpeed_north_mPERs",
    "rHavg_pct", "rHmax_pct", "rHmin_pct", "specHavg_gPERkg", "Tdewpoint_C",
    "actVP_kPa",
    "shortWR"
  )
}

#' @return A data frame with four columns:
#'   * `"old"`: the outdated weather data column name
#'   * `"new"`: the new weather data column name
#'   * `"v"`: the `"rSOILWAT2"` version when the name change was introduced
#'   * `"fail"`: error if non-missing values are present
#' @md
#' @noRd
weather_renamedDataColumns <- function() {
  rbind(
    data.frame(
      old = "specHavg_pct", new = "specHavg_gPERkg", v = "6.2.0", fail = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

#' Functions to summarize currently implemented daily weather variables
#' @return A named vector of functions that summarize
#' daily weather variables across time.
#' @export
weather_dataAggFun <- function() {
  c(
    Tmax_C = mean,
    Tmin_C = mean,
    PPT_cm = sum,
    cloudCov_pct = mean,
    windSpeed_mPERs = mean,
    windSpeed_east_mPERs = mean,
    windSpeed_north_mPERs = mean,
    rHavg_pct = mean,
    rHmax_pct = mean,
    rHmin_pct = mean,
    specHavg_pct = mean, # specific humidity: rSOILWAT2 v6.0.0 - v6.1.0
    specHavg_gPERkg = mean, # specific humidity: rSOILWAT2 >= v6.1.1
    Tdewpoint_C = mean,
    actVP_kPa = mean,
    shortWR = mean
  )
}

#' List valid ranges of weather variables
#' @return A named list of length-two numeric vectors.
#' @export
weather_dataBounds <- function() {
  list(
    Tmax_C = c(-Inf, Inf),
    Tmin_C = c(-Inf, Inf),
    PPT_cm = c(0, Inf),
    cloudCov_pct = c(0, 100),
    windSpeed_mPERs = c(0, Inf),
    windSpeed_east_mPERs = c(0, Inf),
    windSpeed_north_mPERs = c(0, Inf),
    rHavg_pct = c(0, 100),
    rHmax_pct = c(0, 100),
    rHmin_pct = c(0, 100),
    specHavg_pct = c(0, 100),
    specHavg_gPERkg = c(0, 1000),
    Tdewpoint_C = c(-Inf, Inf),
    actVP_kPa = c(0, Inf),
    shortWR = c(0, Inf)
  )
}


#' Class \code{"swWeatherData"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param weatherList A list or \code{NULL}. Each element is an object of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}
#'   containing daily weather data of a specific year.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @slot year An integer value. The calendar year of the weather \code{data}
#'   object.
#' @slot data A 365 x 15 or 366 x 15 matrix representing daily weather data for
#'   one calendar \code{year} with columns
#'   \var{DOY},
#'   \var{Tmax_C}, \var{Tmin_C}, \var{PPT_cm},
#'   \var{cloudCov_pct},
#'   \var{windSpeed_mPERs},
#'   \var{windSpeed_east_mPERs}, \var{windSpeed_north_mPERs},
#'   \var{rHavg_pct}, \var{rHmax_pct}, \var{rHmin_pct},
#'   \var{specHavg_gPERkg}, \var{Tdewpoint_C},
#'   \var{actVP_kPa}, and
#'   \var{shortWR}.
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swWeatherData")
#' x <- new("swWeatherData")
#' x <- swWeatherData()
#'
#' @name swWeatherData-class
#' @export
setClass(
  "swWeatherData",
  slots = c(data = "matrix", year = "integer"),
  prototype = list(
    # NOTE: 999 should be rSW2_glovars[["kSOILWAT2"]][["kNUM"]][["SW_MISSING"]]
    # NOTE: 15 must be
    # equal to 1 + rSW2_glovars[["kSOILWAT2"]][["kINT"]][["MAX_INPUT_COLUMNS"]]
    data = array(
      data = c(1:366, rep(NA, 366 * 15L)),
      dim = c(366, 15L),
      dimnames = list(
        NULL,
        c("DOY", weather_dataColumns())
      )
    ),
    year = NA_integer_
  )
)

setValidity(
  "swWeatherData",
  function(object) {
    val <- TRUE
    ref <- new("swWeatherData")

    if (
      !(
        length(object@year) == 1 &&
          (
            isTRUE(is.finite(object@year) && object@year >= 0) ||
              isTRUE(is.na(object@year))
          )
      )
    ) {
      msg <- "@year must be exactly one positive value or NA."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    tmp <- dim(object@data)
    if (tmp[[2L]] != ncol(ref@data)) {
      msg <- paste(
        "@data must have exactly", ncol(ref@data), "columns corresponding to",
        toString(colnames(ref@data))
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    cns <- colnames(object@data)
    validCns <- c("day", colnames(ref@data))
    if (!all(tolower(cns) %in% tolower(validCns))) {
      shouldNot <- setdiff(tolower(cns), tolower(validCns))
      shouldHave <- setdiff(tolower(colnames(ref@data)), tolower(cns))
      msg <- paste(
        "@data has column(s)",
        toString(shQuote(cns[tolower(cns) %in% shouldNot])),
        "instead of",
        toString(shQuote(validCns[tolower(validCns) %in% shouldHave]))
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (!(tmp[1] %in% c(365, 366))) {
      msg <- "@data must 365 or 366 rows corresponding to day of year."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swWeatherData-class
#' @export
swWeatherData <- function(...) {
  # We don't use default values for slots `year` and `data`; this is to prevent
  # simulation runs with accidentally incorrect values
  def <- new("swWeatherData")
  sns <- slotNames(def)
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swWeatherData")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # Guarantee names
  if ("data" %in% dns) {
    dimnames(dots[["data"]]) <- dimnames(slot(def, "data"))
  }

  if ("year" %in% dns) {
    dots[["year"]] <- as.integer(dots[["year"]])
  }

  do.call("new", args = c("swWeatherData", dots[dns %in% sns]))
}


#' @param weatherDF A data frame with weather variables.
#' @param template_weatherColumns A vector with requested weather variables.
#'
#' @return For [upgrade_weatherColumns()]:
#' an updated `weatherDF` with requested column name changes.
#'
#' @examples
#' upgrade_weatherColumns(
#'   data.frame(DOY = 1:2, Tmax_C = runif(2), dummy = runif(2))
#' )
#' upgrade_weatherColumns(
#'   data.frame(DOY = 1:2, Tmax_C = runif(2), specHavg_pct = NA)
#' )
#'
#' @md
#' @rdname sw_upgrade
#' @export
upgrade_weatherColumns <- function(
  weatherDF,
  template_weatherColumns = c("Year", "DOY", weather_dataColumns())
) {
  cns <- colnames(weatherDF)
  if (!all(cns %in% template_weatherColumns)) {
    rds <- weather_renamedDataColumns()
    ids <- match(cns, rds[, "old", drop = TRUE], nomatch = 0L)
    for (k in which(ids > 0L)) {
      if (
        isTRUE(rds[ids[[k]], "fail", drop = TRUE]) &&
          !all(is_missing_weather(weatherDF[, cns[[k]], drop = TRUE]))
      ) {
        stop(
          "Renaming ", shQuote(cns[[k]]), " to ",
          shQuote(as.character(rds[ids[[k]], "new", drop = TRUE])),
          " failed because of non-missing values.",
          call. = FALSE
        )
      }

      cns[[k]] <- as.character(rds[ids[[k]], "new", drop = TRUE])
    }
    colnames(weatherDF) <- cns
  }

  weatherDF
}

#' @return For [upgrade_weatherDF()]:
#' an updated `weatherDF` with requested columns.
#'
#' @examples
#' upgrade_weatherDF(
#'   data.frame(DOY = 1:2, Tmax_C = runif(2), dummy = runif(2))
#' )
#' upgrade_weatherDF(
#'   data.frame(DOY = 1:2, Tmax_C = runif(2), specHavg_pct = NA)
#' )
#'
#' @md
#' @rdname sw_upgrade
#' @export
upgrade_weatherDF <- function(
  weatherDF,
  template_weatherColumns = c("Year", "DOY", weather_dataColumns())
) {
  template_data <- as.data.frame(
    array(
      dim = c(nrow(weatherDF), length(template_weatherColumns)),
      dimnames = list(NULL, template_weatherColumns)
    )
  )

  weatherDF <- upgrade_weatherColumns(weatherDF)

  cns <- intersect(template_weatherColumns, colnames(weatherDF))
  if (length(cns) < 1L) {
    stop("Required weather variables not found.", call. = FALSE)
  }

  template_data[, cns] <- weatherDF[, cns]
  template_data
}

upgrade_swWeatherData <- function(data, year, template = new("swWeatherData")) {
  template@year <- as.integer(year)
  template@data <- data.matrix(
    upgrade_weatherDF(data, c("DOY", weather_dataColumns()))
  )
  template
}


#' @rdname sw_upgrade
#' @export
upgrade_weatherHistory <- function(object, verbose = FALSE) {
  tmp <- try(dbW_check_weatherData(object, check_all = FALSE), silent = TRUE)
  if (inherits(tmp, "try-error") || !isTRUE(tmp)) {
    if (verbose) {
      message("Upgrading `weatherHistory` object.")
    }

    template <- new("swWeatherData")

    object <- lapply(
      object,
      function(old) {
        upgrade_swWeatherData(
          data = old@data,
          year = old@year,
          template = template
        )
      }
    )
  }

  object
}


#' @rdname swWeatherData-class
#' @export
weatherHistory <- function(weatherList = NULL) {
  if (isTRUE(dbW_check_weatherData(weatherList))) {
    weatherList
  } else {
    list(swWeatherData())
  }
}

validObject_weatherHistory <- function(object) {
  res <- lapply(object, validObject)
  has_msg <- vapply(res, is.character, FUN.VALUE = NA)
  if (any(has_msg)) {
    unlist(res[has_msg])
  } else {
    TRUE
  }
}
