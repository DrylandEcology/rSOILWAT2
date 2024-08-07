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
    "rHavg_pct", "rHmax_pct", "rHmin_pct", "specHavg_pct", "Tdewpoint_C",
    "actVP_kPa",
    "shortWR"
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
    specHavg_pct = mean,
    Tdewpoint_C = mean,
    actVP_kPa = mean,
    shortWR = mean
  )
}

#' Class \code{"swWeatherData"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swWeatherData}}.
#' @param file A character string. The file name from which to read.
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
#'   \var{specHavg_pct}, \var{Tdewpoint_C},
#'   \var{actVP_kPa}, and
#'   \var{shortWR}.
#'
#' @seealso
#' \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}}
#' \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}}
#' \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}}
#' \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swSpinup}}
#' \code{\linkS4class{swEstab}}
#' \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swCarbon}}
#' \code{\linkS4class{swSWC}}
#' \code{\linkS4class{swLog}}
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
    if (tmp[2] != ncol(ref@data)) {
      msg <- paste(
        "@data must have exactly", ncol(ref@data), "columns corresponding to",
        toString(colnames(ref@data))
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
#' @return For [upgrade_weatherDF()]:
#' an updated `weatherDF` with requested columns.
#'
#' @examples
#' upgrade_weatherDF(
#'   data.frame(DOY = 1:2, Tmax_C = runif(2), dummy = runif(2))
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

  cns <- intersect(template_weatherColumns, colnames(weatherDF))
  if (length(cns) < 1L) stop("Required weather variables not found.")
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


#' @rdname swWeatherData-class
#' @export
setMethod(
  "swReadLines",
  signature = c(object = "swWeatherData", file = "character"),
  function(object, file) {
    .Deprecated("C_rSW2_readAllWeatherFromDisk")
    warning("swReadLines works only with traditional weather data.")

    object@year <- as.integer(
      strsplit(
        x = basename(file),
        split = ".",
      fixed = TRUE
      )[[1]][2]
    )
    data <- utils::read.table(
      file,
      header = FALSE,
      comment.char = "#",
      blank.lines.skip = TRUE,
      sep = "\t"
    )
    stopifnot(ncol(data) != 4L)
    colnames(data) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    object@data[] <- NA
    object@data[, colnames(data)] <- as.matrix(data)

    object
})
