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

#' Class \code{"swInputData"}
#'
#' This class is a container class to the input file \var{S4} objects. The
#' generic functions listed work on this and the proper \pkg{rSOILWAT2}
#' classes in the container's slots.
#'
#' \code{\linkS4class{swInputData}} consists of slots for each file that is
#' read in. These slots can be accessed via the following functions: \itemize{
#'   \item \code{\link{get_swMarkov}}
#'   \item \code{\link{get_swCloud}}
#'   \item \code{\link{get_swFiles}}
#'   \item \code{\link{get_swOUT}}
#'   \item \code{\link{get_swProd}}
#'   \item \code{\link{get_swSite}}
#'   \item \code{\link{get_swSoils}}
#'   \item \code{\link{get_swSpinup}}
#'   \item \code{\link{get_swSWC}}
#'   \item \code{\link{get_swWeather}}
#'   \item \code{\link{get_swWeatherData}}
#'   \item \code{\link{get_swYears}}
#'   \item \code{\link{get_WeatherHistory}}
#' }
#'
#' Generic methods to get/set individual elements follow a format:
#' \code{"sw" + filename + "_" + option}, e.g.
#' \itemize{
#'   \item \code{\link{swFiles_Cloud}}
#'   \item \code{\link{swProd_Albedo}}
#' }
#'
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @param year An integer value. The calendar year of the weather or
#'   \var{SWC} \code{data} object.
#' @param vegtype The name or index of the vegetation type.
#'
#' @seealso
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
#' showClass("swInputData")
#' x <- new("swInputData") # prototype
#' x <- swInputData() # constructor helper
#'
#' @name swInputData-class
#' @export
setClass(
  "swInputData",
  slots = c(
    version = "character",
    timestamp = "numeric",
    files = "swFiles",
    years = "swYears",
    weather = "swWeather",
    cloud = "swCloud",
    weatherHistory = "list",
    markov = "swMarkov",
    prod = "swProd",
    site = "swSite",
    soils = "swSoils",
    spinup = "swSpinup",
    estab = "swEstab",
    carbon = "swCarbon",
    output = "swOUT",
    swc = "swSWC",
    log = "swLog"
  )
  # Note: we cannot set prototypes for `swInputData` because
  # that calls each slot's class constructor; the constructors call eventually
  # `new()` which in turn calls setValidity()` which use `rSW2_glovars`.
  # However, this all occurs before `rSW2_glovars` is defined, i.e.,
  # validity functions are erroring out if they utilize `rSW2_glovars`.
  # Calling `validObject()` at run time is not a problem because
  # `rSW2_glovars` will be defined by then (see `.onLoad()`).
)


#' @rdname swInputData-class
#' @export
swInputData <- function(...) {
  # Call helper constructor for each slot class
  dots <- list(...)

  if (length(dots) == 1 && inherits(dots[[1]], "swInputData")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }

  dns <- names(dots)

  object <- new("swInputData")
  object@version <- rSW2_version()
  object@timestamp <- rSW2_timestamp()


  object@files <- if ("files" %in% dns) {
    swFiles(dots[["files"]])
  } else {
    do.call(swFiles, dots)
  }

  object@years <- if ("years" %in% dns) {
    swYears(dots[["years"]])
  } else {
    do.call(swYears, dots)
  }

  object@weather <- if ("weather" %in% dns) {
    swWeather(dots[["weather"]])
  } else {
    do.call(swWeather, dots)
  }

  object@cloud <- if ("cloud" %in% dns) {
    swCloud(dots[["cloud"]])
  } else {
    do.call(swCloud, dots)
  }

  object@weatherHistory <- weatherHistory(dots[["weatherHistory"]])

  object@markov <- if ("markov" %in% dns) {
    swMarkov(dots[["markov"]])
  } else {
    do.call(swMarkov, dots)
  }

  object@prod <- if ("prod" %in% dns) {
    swProd(dots[["prod"]])
  } else {
    do.call(swProd, dots)
  }

  object@site <- if ("site" %in% dns) {
    swSite(dots[["site"]])
  } else {
    do.call(swSite, dots)
  }

  object@soils <- if ("soils" %in% dns) {
    swSoils(dots[["soils"]])
  } else {
    do.call(swSoils, dots)
  }

  object@estab <- if ("estab" %in% dns) {
    swEstab(dots[["estab"]])
  } else {
    do.call(swEstab, dots)
  }

  object@carbon <- if ("carbon" %in% dns) {
    swCarbon(dots[["carbon"]])
  } else {
    do.call(swCarbon, dots)
  }

  object@output <- if ("output" %in% dns) {
    swOUT(dots[["output"]])
  } else {
    do.call(swOUT, dots)
  }

  object@swc <- if ("swc" %in% dns) {
    swSWC(dots[["swc"]])
  } else {
    do.call(swSWC, dots)
  }

  object@spinup <- if ("spinup" %in% dns) {
    swSpinup(dots[["spinup"]])
  } else {
    do.call(swSpinup, dots)
  }

  object@log <- if ("log" %in% dns) {
    swLog(dots[["log"]])
  } else {
    do.call(swLog, dots)
  }

  object
}



setValidity(
  "swInputData",
  function(object) {
    res <- lapply(
      slotNames(object),
      function(sn) {
        if (identical(sn, "weatherHistory")) {
          validObject_weatherHistory(slot(object, sn))
        } else {
          validObject(slot(object, sn))
        }
      }
    )

    has_msg <- sapply(res, is.character)
    if (any(has_msg)) {
      unlist(res[has_msg])
    } else {
      TRUE
    }
  }
)


#' @rdname sw_upgrade
#' @export
setMethod(
  "sw_upgrade",
  signature = "swInputData",
  definition = function(object, verbose = FALSE) {

    msg_upgrades <- NULL

    # Suppress warnings in case `object` is indeed invalid (outdated)
    if (!suppressWarnings(check_version(object))) {

      # Upgrade slots of swInputData
      for (sn in slotNames(object)) {
        if (identical(sn, "weatherHistory")) {
          if (!dbW_check_weatherData(slot(object, sn), check_all = FALSE)) {
            slot(object, sn) <- suppressWarnings(
              upgrade_weatherHistory(slot(object, sn))
            )
            msg_upgrades <- c(msg_upgrades, sn)
          }

        } else {
          tmp <- try(validObject(slot(object, sn)), silent = TRUE)

          if (inherits(tmp, "try-error")) {
            if (grepl("invalid class", tmp, fixed = TRUE)) {
              # Upgrade existing but invalid slots
              slot(object, sn) <- suppressWarnings(
                sw_upgrade(slot(object, sn), verbose = FALSE)
              )

            } else if (grepl("no slot of name", tmp, fixed = TRUE)) {
              # Add new slot
              if (identical(sn, "spinup")) {
                object@spinup <- swSpinup()
              }

            } else {
              stop("Failed to upgrade 'swInputData' object slot ", shQuote(sn))
            }

            msg_upgrades <- c(msg_upgrades, sn)
          }
        }
      }

      if (length(msg_upgrades) > 0) {
        if (verbose) {
          message(
            "Upgrading object of class `swInputData`: ",
            toString(shQuote(msg_upgrades))
          )
        }

        #--- Update version/timestamp
        object@version <- rSW2_version()
        object@timestamp <- rSW2_timestamp()

        #--- Check validity and return
        validObject(object)
      }
    }

    object
  }
)




# Methods for slot \code{files}
#' @rdname swInputData-class
#' @export
setMethod("get_swFiles", "swInputData", function(object) object@files)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_ProjDir",
  signature = "swInputData",
  function(object) swFiles_ProjDir(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_filesIn",
  signature = "swInputData",
  function(object) swFiles_filesIn(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Years",
  signature = "swInputData",
  function(object) swFiles_Years(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_LogFile",
  signature = "swInputData",
  function(object) swFiles_LogFile(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_SiteParams",
  signature = "swInputData",
  function(object) swFiles_SiteParams(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Soils",
  signature = "swInputData",
  function(object) swFiles_Soils(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_SWRCp",
  signature = "swInputData",
  function(object) swFiles_SWRCp(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_WeatherSetup",
  signature = "swInputData",
  function(object) swFiles_WeatherSetup(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_MarkovProbs",
  signature = "swInputData",
  function(object) swFiles_MarkovProbs(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_MarkovCov",
  signature = "swInputData",
  function(object) swFiles_MarkovCov(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Cloud",
  signature = "swInputData",
  function(object) swFiles_Cloud(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Prod",
  signature = "swInputData",
  function(object) swFiles_Prod(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Estab",
  signature = "swInputData",
  function(object) swFiles_Estab(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Carbon",
  signature = "swInputData",
  function(object) swFiles_Carbon(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_SWCsetup",
  signature = "swInputData",
  function(object) swFiles_SWCsetup(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_Output",
  signature = "swInputData",
  function(object) swFiles_Output(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_WeatherPrefix",
  signature = "swInputData",
  function(object) swFiles_WeatherPrefix(object@files)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swFiles_OutputPrefix",
  signature = "swInputData",
  function(object) swFiles_OutputPrefix(object@files)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swFiles",
  signature = "swInputData",
  function(object, value) {
    object@files <- value
    validObject(object@files)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_ProjDir",
  signature = "swInputData",
  function(object, value) {
    swFiles_ProjDir(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_filesIn",
  signature = "swInputData",
  function(object, value) {
    swFiles_filesIn(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Years",
  signature = "swInputData",
  function(object, value) {
    swFiles_Years(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_LogFile",
  signature = "swInputData",
  function(object, value) {
    swFiles_LogFile(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_SiteParams",
  signature = "swInputData",
  function(object, value) {
    swFiles_SiteParams(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Soils",
  signature = "swInputData",
  function(object, value) {
    swFiles_Soils(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_SWRCp",
  signature = "swInputData",
  function(object, value) {
    swFiles_SWRCp(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_WeatherSetup",
  signature = "swInputData",
  function(object, value) {
    swFiles_WeatherSetup(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_MarkovProbs",
  signature = "swInputData",
  function(object, value) {
    swFiles_MarkovProbs(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_MarkovCov",
  signature = "swInputData",
  function(object, value) {
    swFiles_MarkovCov(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Cloud",
  signature = "swInputData",
  function(object, value) {
    swFiles_Cloud(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Prod",
  signature = "swInputData",
  function(object, value) {
    swFiles_Prod(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Estab",
  signature = "swInputData",
  function(object, value) {
    swFiles_Estab(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Carbon",
  signature = "swInputData",
  function(object, value) {
    swFiles_Carbon(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_SWCsetup",
  signature = "swInputData",
  function(object, value) {
    swFiles_SWCsetup(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_Output",
  signature = "swInputData",
  function(object, value) {
    swFiles_Output(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_WeatherPrefix",
  signature = "swInputData",
  function(object, value) {
    swFiles_WeatherPrefix(object@files) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swFiles_OutputPrefix",
  signature = "swInputData",
  function(object, value) {
    swFiles_OutputPrefix(object@files) <- value
    object
  }
)


# Methods for slot \code{years}
#' @rdname swInputData-class
#' @export
setMethod("get_swYears", "swInputData", function(object) object@years)

#' @rdname swInputData-class
#' @export
setMethod(
  "swYears_StartYear",
  signature = "swInputData",
  function(object) swYears_StartYear(object@years)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swYears_EndYear",
  signature = "swInputData",
  function(object) swYears_EndYear(object@years)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swYears_FDOFY",
  signature = "swInputData",
  function(object) swYears_FDOFY(object@years)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swYears_EDOEY",
  signature = "swInputData",
  function(object) swYears_EDOEY(object@years)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swYears_isNorth",
  signature = "swInputData",
  function(object) swYears_isNorth(object@years)
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swYears",
  signature = "swInputData",
  function(object, value) {
    object@years <- value
    validObject(object@years)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swYears_StartYear",
  signature = "swInputData",
  function(object, value) {
    swYears_StartYear(object@years) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swYears_EndYear",
  signature = "swInputData",
  function(object, value) {
    swYears_EndYear(object@years) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swYears_FDOFY",
  signature = "swInputData",
  function(object, value) {
    swYears_FDOFY(object@years) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swYears_EDOEY",
  signature = "swInputData",
  function(object, value) {
    swYears_EDOEY(object@years) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swYears_isNorth",
  signature = "swInputData",
  function(object, value) {
    swYears_isNorth(object@years) <- as.logical(value)
    object
  }
)


# Methods for slot \code{cloud}
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud", "swInputData", function(object) object@cloud)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swCloud",
  signature = "swInputData",
  function(object, value) {
    set_swCloud(object@cloud) <- value
    object
  }
)


# Methods for slot \code{weather}
#' @rdname swInputData-class
#' @export
setMethod("get_swWeather", "swInputData", function(object) object@weather)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_DaysRunningAverage",
  signature = "swInputData",
  function(object) swWeather_DaysRunningAverage(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_FirstYearHistorical",
  signature = "swInputData",
  function(object) swWeather_FirstYearHistorical(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_pct_SnowDrift",
  signature = "swInputData",
  function(object) swWeather_DaysRunningAverage(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_pct_SnowRunoff",
  signature = "swInputData",
  function(object) swWeather_pct_SnowDrift(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_UseMarkov",
  signature = "swInputData",
  function(object) swWeather_UseMarkov(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_UseMarkovOnly",
  signature = "swInputData",
  function(object) swWeather_UseMarkovOnly(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_UseSnow",
  signature = "swInputData",
  function(object) swWeather_UseSnow(object@weather)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swWeather_MonScalingParams",
  signature = "swInputData",
  function(object) swWeather_MonScalingParams(object@weather)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swWeather",
  signature = "swInputData",
  function(object, value) {
    slot(object, "weather") <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_DaysRunningAverage",
  signature = "swInputData",
  function(object, value) {
    swWeather_DaysRunningAverage(object@weather) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_FirstYearHistorical",
  signature = "swInputData",
  function(object, value) {
    swWeather_FirstYearHistorical(object@weather) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_pct_SnowDrift",
  signature = "swInputData",
  function(object, value) {
    swWeather_pct_SnowDrift(object@weather) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_pct_SnowRunoff",
  signature = "swInputData",
  function(object, value) {
    swWeather_pct_SnowRunoff(object@weather) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_UseMarkov",
  signature = "swInputData",
  function(object, value) {
    swWeather_UseMarkov(object@weather) <- as.logical(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_UseMarkovOnly",
  signature = "swInputData",
  function(object, value) {
    swWeather_UseMarkovOnly(object@weather) <- as.logical(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_UseSnow",
  signature = "swInputData",
  function(object, value) {
    swWeather_UseSnow(object@weather) <- as.logical(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swWeather_MonScalingParams",
  signature = "swInputData",
  function(object, value) {
    swWeather_MonScalingParams(object@weather) <- value
    object
  }
)



# Methods for slot \code{cloud}
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud", "swInputData", function(object) object@cloud)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCloud_SkyCover",
  signature = "swInputData",
  function(object) swCloud_SkyCover(object@cloud)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCloud_WindSpeed",
  signature = "swInputData",
  function(object) swCloud_WindSpeed(object@cloud)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCloud_Humidity",
  signature = "swInputData",
  function(object) swCloud_Humidity(object@cloud)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCloud_SnowDensity",
  signature = "swInputData",
  function(object) swCloud_SnowDensity(object@cloud)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCloud_RainEvents",
  signature = "swInputData",
  function(object) swCloud_RainEvents(object@cloud)
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swCloud",
  signature = "swInputData",
  function(object, value) {
    set_swCloud(object@cloud) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCloud_SkyCover",
  signature = "swInputData",
  function(object, value) {
    swCloud_SkyCover(object@cloud) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCloud_WindSpeed",
  signature = "swInputData",
  function(object, value) {
    swCloud_WindSpeed(object@cloud) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCloud_Humidity",
  signature = "swInputData",
  function(object, value) {
    swCloud_Humidity(object@cloud) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCloud_SnowDensity",
  signature = "swInputData",
  function(object, value) {
    swCloud_SnowDensity(object@cloud) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCloud_RainEvents",
  signature = "swInputData",
  function(object, value) {
    swCloud_RainEvents(object@cloud) <- value
    object
  }
)


# Methods for slot \code{markov}
# use `get_swMarkov()`, `get_Markov()` is a legacy name
#' @rdname swInputData-class
#' @export
setMethod("get_Markov", "swInputData", function(object) object@markov)

#' @rdname swInputData-class
#' @export
setMethod("get_swMarkov", "swInputData", function(object) object@markov)

#' @rdname swInputData-class
#' @export
setMethod(
  "swMarkov_Prob",
  signature = "swInputData",
  function(object) swMarkov_Prob(object@markov)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swMarkov_Conv",
  signature = "swInputData",
  function(object) swMarkov_Conv(object@markov)
)


# use `set_swMarkov()`; `set_Markov()` is a legacy name
#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_Markov",
  signature = "swInputData",
  function(object, value) {
    set_swMarkov(object@markov) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swMarkov",
  signature = "swInputData",
  function(object, value) {
    set_swMarkov(object@markov) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swMarkov_Prob",
  signature = "swInputData",
  function(object, value) {
    swMarkov_Prob(object@markov) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swMarkov_Conv",
  signature = "swInputData",
  function(object, value) {
    swMarkov_Conv(object@markov) <- value
    object
  }
)


# Methods for slot \code{weatherHistory}
#' @rdname swInputData-class
#' @export
setMethod(
  "get_WeatherHistory",
  signature = "swInputData",
  function(object) object@weatherHistory
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_WeatherHistory",
  signature = c(object = "swInputData", value = "list"),
  function(object, value) {
    object@weatherHistory <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setMethod(
  "get_swWeatherData",
  signature = "swInputData",
  function(object, year) {
    index <- which(names(object@weatherHistory) == as.character(year))
    if (length(index) != 1) {
      stop("Index has wrong length.")
    }
    if (object@weatherHistory[[index]]@year != as.integer(year)) {
      print("Somethings wrong with the weather data.")
    }

    object@weatherHistory[[index]]
  }
)

#' @rdname swInputData-class
#' @export
setMethod(
  "get_swWeatherData",
  signature = "list",
  function(object, year) {
    index <- which(names(object) == as.character(year))
    if (length(index) != 1) {
      stop("Index has wrong length.")
    }
    if (object[[index]]@year != as.integer(year)) {
      print("Somethings wrong with the weather data.")
    }

    object[[index]]
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swWeatherData",
  signature = c(object = "swInputData", value = "swWeatherData"),
  function(object, value) {
    index <- which(names(object@weatherHistory) == as.character(value@year))

    if (length(index) == 0) {
      object@weatherHistory[[length(object@weatherHistory) + 1]] <- value
      years <- vapply(
        object@weatherHistory,
        function(x) x@year,
        FUN.VALUE = NA_integer_
      )
      ids_sorted <- sort.list(years, na.last = TRUE)
      object@weatherHistory <- object@weatherHistory[ids_sorted]
      years <- years[ids_sorted]
      names(object@weatherHistory) <- as.character(years)
      if (!all(years == cummax(years))) {
        print("Weather data is Missing")
      }

    } else if (length(index) == 1) {
      object@weatherHistory[[index]] <- value

    } else {
      print("To many indices. Weather data not set")
    }

    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swWeatherData",
  signature = c(object = "list", value = "swWeatherData"),
  function(object, value) {
    index <- which(names(object) == as.character(value@year))

    if (length(index) == 0) {
      object[[length(object) + 1]] <- value
      years <- vapply(
        object,
        function(x) x@year,
        FUN.VALUE = NA_integer_
      )
      ids_sorted <- sort.list(years, na.last = TRUE)
      object <- object[ids_sorted]
      years <- years[ids_sorted]
      names(object) <- as.character(years)
      if (!all(years == cummax(years))) {
        print("Weather data are missing")
      }

    } else if (length(index) == 1) {
     object[[index]] <- value

    } else {
      print("To many indices. Weather data not set")
    }

    object
  }
)


# Methods for slot \code{prod}
#' @rdname swInputData-class
#' @export
setMethod("get_swProd", "swInputData", function(object) object@prod)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_Composition",
  signature = "swInputData",
  function(object) swProd_Composition(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_Albedo",
  signature = "swInputData",
  function(object) swProd_Albedo(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_CanopyHeight",
  signature = "swInputData",
  function(object) swProd_CanopyHeight(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_VegInterParam",
  signature = "swInputData",
  function(object) swProd_VegInterParam(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_LitterInterParam",
  signature = "swInputData",
  function(object) swProd_LitterInterParam(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_EsTpartitioning_param",
  signature = "swInputData",
  function(object) swProd_EsTpartitioning_param(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_Es_param_limit",
  signature = "swInputData",
  function(object) swProd_Es_param_limit(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_Shade",
  signature = "swInputData",
  function(object) swProd_Shade(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_HydrRedstro_use",
  signature = "swInputData",
  function(object) swProd_HydrRedstro_use(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_HydrRedstro",
  signature = "swInputData",
  function(object) swProd_HydrRedstro(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_CritSoilWaterPotential",
  signature = "swInputData",
  function(object) swProd_CritSoilWaterPotential(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_CO2Coefficients",
  signature = "swInputData",
  function(object) swProd_CO2Coefficients(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_MonProd_veg",
  signature = "swInputData",
  function(object, vegtype) swProd_MonProd_veg(object@prod, vegtype)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_MonProd_grass",
  signature = "swInputData",
  function(object) swProd_MonProd_grass(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_MonProd_shrub",
  signature = "swInputData",
  function(object) swProd_MonProd_shrub(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_MonProd_tree",
  signature = "swInputData",
  function(object) swProd_MonProd_tree(object@prod)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swProd_MonProd_forb",
  signature = "swInputData",
  function(object) swProd_MonProd_forb(object@prod)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swProd",
  signature = "swInputData",
  function(object, value) {
    set_swProd(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_Composition",
  signature = "swInputData",
  function(object, value) {
    swProd_Composition(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_Albedo",
  signature = "swInputData",
  function(object, value) {
    swProd_Albedo(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_CanopyHeight",
  signature = "swInputData",
  function(object, value) {
    swProd_CanopyHeight(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_VegInterParam",
  signature = "swInputData",
  function(object, value) {
    swProd_VegInterParam(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_LitterInterParam",
  signature = "swInputData",
  function(object, value) {
    swProd_LitterInterParam(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_EsTpartitioning_param",
  signature = "swInputData",
  function(object, value) {
    swProd_EsTpartitioning_param(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_Es_param_limit",
  signature = "swInputData",
  function(object, value) {
    swProd_Es_param_limit(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_Shade",
  signature = "swInputData",
  function(object, value) {
    swProd_Shade(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro_use",
  signature = "swInputData",
  function(object, value) {
    swProd_HydrRedstro_use(object@prod) <- as.logical(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro",
  signature = "swInputData",
  function(object, value) {
    swProd_HydrRedstro(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_CritSoilWaterPotential",
  signature = "swInputData",
  function(object, value) {
    swProd_CritSoilWaterPotential(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_CO2Coefficients",
  signature = "swInputData",
  function(object, value) {
    swProd_CO2Coefficients(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_MonProd_veg",
  signature = "swInputData",
  function(object, vegtype, value) {
    swProd_MonProd_veg(object@prod, vegtype) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_MonProd_grass",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_grass(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_MonProd_shrub",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_shrub(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_MonProd_tree",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_tree(object@prod) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swProd_MonProd_forb",
  signature = "swInputData",
  function(object, value) {
    swProd_MonProd_forb(object@prod) <- value
    object
  }
)


# Methods for slot \code{estab}
#' @rdname swInputData-class
#' @export
setMethod("get_swEstab", "swInputData", function(object) object@estab)


# Methods for slot \code{site}
#' @rdname swInputData-class
#' @export
setMethod("get_swSite", "swInputData", function(object) slot(object, "site"))

#' @rdname swSite_SWRCflags
setMethod(
  "swSite_SWRCflags",
  signature = "swInputData",
  function(object) swSite_SWRCflags(object@site)
)

#' @rdname swSite_hasSWRCp
setMethod(
  "swSite_hasSWRCp",
  signature = "swInputData",
  function(object) swSite_hasSWRCp(object@site)
)

#' @rdname swSite_depthSapric
setMethod(
  "swSite_depthSapric",
  signature = "swInputData",
  function(object) swSite_depthSapric(object@site)
)


#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_SWClimits",
  signature = "swInputData",
  function(object) swSite_SWClimits(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_ModelFlags",
  signature = "swInputData",
  function(object) swSite_ModelFlags(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_ModelCoefficients",
  signature = "swInputData",
  function(object) swSite_ModelCoefficients(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_SnowSimulationParams",
  signature = "swInputData",
  function(object) swSite_SnowSimulationParams(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_DrainageCoefficient",
  signature = "swInputData",
  function(object) swSite_DrainageCoefficient(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_EvapCoefficients",
  signature = "swInputData",
  function(object) swSite_EvapCoefficients(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_TranspCoefficients",
  signature = "swInputData",
  function(object) swSite_TranspCoefficients(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_IntrinsicSiteParams",
  signature = "swInputData",
  function(object) swSite_IntrinsicSiteParams(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_SoilTemperatureFlag",
  signature = "swInputData",
  function(object) swSite_SoilTemperatureFlag(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_SoilTemperatureConsts",
  signature = "swInputData",
  function(object) swSite_SoilTemperatureConsts(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_SoilDensityInputType",
  signature = "swInputData",
  function(object) swSite_SoilDensityInputType(object@site)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSite_TranspirationRegions",
  signature = "swInputData",
  function(object) swSite_TranspirationRegions(object@site)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swSite",
  signature = "swInputData",
  function(object, value) {
    set_swSite(object@site) <- value
    object
  }
)

#' @rdname swSite_SWRCflags
#'
#' @section Details:
#' The replacement method [swSite_SWRCflags()] for class [swInputData-class]
#' resets `has_swrcp` to `FALSE` if `"swrc_name"` or `"ptf_name"` are updated.
#' This is to avoid inconsistency between
#' `SWRCp`, `has_swrcp`, and `swrc_flags`.
#'
#' @section Details:
#' The correct sequence for setting values is
#'   1. [swSoils_Layers()],
#'   2. [swSite_SWRCflags()], and
#'   3. [swSoils_SWRCp()] and [swSite_hasSWRCp()]
#'
#' @md
setReplaceMethod(
  "swSite_SWRCflags",
  signature = "swInputData",
  function(object, value) {
    prev <- as.character(swSite_SWRCflags(object@site))
    value <- as.character(value)
    if (!identical(prev, value)) {
      swSite_SWRCflags(object@site) <- value
      # Reset has_swrcp -- avoid inconsistency between SWRCp and swrc_flags
      swSite_hasSWRCp(object) <- FALSE
    }
    object
  }
)

#' @rdname swSite_hasSWRCp
setReplaceMethod(
  "swSite_hasSWRCp",
  signature = "swInputData",
  function(object, value) {
    swSite_hasSWRCp(object@site) <- value
    object
  }
)

#' @rdname swSite_depthSapric
setReplaceMethod(
  "swSite_depthSapric",
  signature = "swInputData",
  function(object, value) {
    swSite_depthSapric(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_SWClimits",
  signature = "swInputData",
  function(object, value) {
    swSite_SWClimits(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_ModelFlags",
  signature = "swInputData",
  function(object, value) {
    swSite_ModelFlags(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_ModelCoefficients",
  signature = "swInputData",
  function(object, value) {
    swSite_ModelCoefficients(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_SnowSimulationParams",
  signature = "swInputData",
  function(object, value) {
    swSite_SnowSimulationParams(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_DrainageCoefficient",
  signature = "swInputData",
  function(object, value) {
    swSite_DrainageCoefficient(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_EvapCoefficients",
  signature = "swInputData",
  function(object, value) {
    swSite_EvapCoefficients(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_TranspCoefficients",
  signature = "swInputData",
  function(object, value) {
    swSite_TranspCoefficients(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_IntrinsicSiteParams",
  signature = "swInputData",
  function(object, value) {
    swSite_IntrinsicSiteParams(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_SoilTemperatureFlag",
  signature = "swInputData",
  function(object, value) {
    swSite_SoilTemperatureFlag(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_SoilTemperatureConsts",
  signature = "swInputData",
  function(object, value) {
    swSite_SoilTemperatureConsts(slot(object, "site")) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_SoilDensityInputType",
  signature = "swInputData",
  function(object, value) {
    swSite_SoilDensityInputType(object@site) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSite_TranspirationRegions",
  signature = "swInputData",
  function(object, value) {
    swSite_TranspirationRegions(object@site) <- value
    object
  }
)


# Methods for slot \code{soils}
#' @rdname swInputData-class
#' @export
setMethod("get_swSoils", "swInputData", function(object) object@soils)

#' @rdname swSoils_Layers
setMethod(
  "swSoils_Layers",
  signature = "swInputData",
  function(object) swSoils_Layers(object@soils)
)

#' @rdname swSoils_SWRCp
setMethod(
 "swSoils_SWRCp",
 signature = "swInputData",
 function(object) swSoils_SWRCp(object@soils)
)

#' @rdname swSoils_omSWRCp
setMethod(
 "swSoils_omSWRCp",
 signature = "swInputData",
 function(object) swSoils_omSWRCp(object@soils)
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swInputData", value = "swSoils"),
  function(object, value) {
    set_swSoils(object@soils) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swInputData", value = "list"),
  function(object, value) {
    set_swSoils(object@soils) <- value
    object
  }
)

#' @rdname swSoils_Layers
#'
#' @section Details:
#' The replacement method `swSoils_Layers<-` for class [swInputData-class]
#' resizes `SWRCp` to match number of new soil layers
#' (and reset `SWRCp` values to `NA`) if `"has_swrcp"` is `FALSE`.
#' This is to avoid inconsistency between
#' soil properties and `SWRCp`.

#'
#' @section Details:
#' The correct sequence for setting values is
#'   1. [swSoils_Layers()],
#'   2. [swSite_SWRCflags()], and
#'   3. [swSoils_SWRCp()] and [swSite_hasSWRCp()]
#'   4. [swSoils_omSWRCp()]
#'
#' @md
setReplaceMethod(
  "swSoils_Layers",
  signature = "swInputData",
  function(object, value) {

    if (!swSite_hasSWRCp(object@site)) {
      # --> assigning new soil layers fails `swSoils` validity checks
      # if number of soil layers disagrees with the SWRC parameter object.
      object@soils@SWRCp <- reset_SWRCp(
        SWRCp = object@soils@SWRCp,
        new_nrow = nrow(value)
      )
    }

    swSoils_Layers(object@soils) <- value
    object
  }
)


#' @rdname swSoils_SWRCp
#'
#' @section Details:
#' The correct sequence for setting values is
#'   1. [swSoils_Layers()],
#'   2. [swSite_SWRCflags()], and
#'   3. [swSoils_SWRCp()] and [swSite_hasSWRCp()]
#'   4. [swSoils_omSWRCp()]
#'
#' @md
setReplaceMethod(
  "swSoils_SWRCp",
  signature = "swInputData",
  function(object, value) {
    swSoils_SWRCp(object@soils) <- value
    object
  }
)

#' @rdname swSoils_omSWRCp
#'
#' @section Details:
#' The correct sequence for setting values is
#'   1. [swSoils_Layers()],
#'   2. [swSite_SWRCflags()], and
#'   3. [swSoils_SWRCp()] and [swSite_hasSWRCp()]
#'   4. [swSoils_omSWRCp()]
#'
#' @md
setReplaceMethod(
  "swSoils_omSWRCp",
  signature = "swInputData",
  function(object, value) {
    swSoils_omSWRCp(object@soils) <- value
    object
  }
)


# Methods for slot \code{swc}
#' @rdname swInputData-class
#' @export
setMethod("get_swSWC", "swInputData", function(object) object@swc)

#' @rdname swInputData-class
#' @export
setMethod("swSWC_use", "swInputData", function(object) swSWC_use(object@swc))

#' @rdname swInputData-class
#' @export
setMethod(
  "swSWC_prefix",
  signature = "swInputData",
  function(object) swSWC_prefix(object@swc)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSWC_FirstYear",
  signature = "swInputData",
  function(object) swSWC_FirstYear(object@swc)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSWC_Method",
  signature = "swInputData",
  function(object) swSWC_Method(object@swc)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSWC_HistoricList",
  signature = "swInputData",
  function(object) swSWC_HistoricList(object@swc)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swSWC_HistoricData",
  signature = "swInputData",
  function(object, year) swSWC_HistoricData(object@swc, year)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swSWC",
  signature = c(object = "swInputData", value = "swSWC"),
  function(object, value) {
    object@swc <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_use",
  signature = c(object = "swInputData", value = "logical"),
  function(object, value) {
    swSWC_use(object@swc) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_prefix",
  signature = c(object = "swInputData", value = "character"),
  function(object, value) {
    swSWC_prefix(object@swc) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_FirstYear",
  signature = c(object = "swInputData", value = "integer"),
  function(object, value) {
    swSWC_FirstYear(object@swc) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_Method",
  signature = c(object = "swInputData", value = "integer"),
  function(object, value) {
    swSWC_Method(object@swc) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_HistoricList",
  signature = c(object = "swInputData", value = "list"),
  function(object, value) {
    swSWC_HistoricList(object@swc) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSWC_HistoricData",
  signature = c(object = "swInputData", value = "swSWC_hist"),
  function(object, value) {
    index <- which(names(object@swc@History) == as.character(value@year))

    if (length(index) == 0) {
      object@swc@History[[length(object@swc@History) + 1]] <- value
      years <- vapply(
        object@swc@History,
        function(x) x@year,
        FUN.VALUE = NA_integer_
      )
      ids_sorted <- sort.list(years, na.last = TRUE)
      object@swc@History <- object@swc@History[ids_sorted]
      years <- years[ids_sorted]
      names(object@swc@History) <- as.character(years)
      if (!all(years == cummax(years))) {
        print("SWC data is Missing")
      }

    } else if (length(index) == 1) {
      object@swc@History[[index]] <- value

    } else {
     print("To many index. Not set")
    }

    object
  }
)


# Methods for slot \code{spinup}
#' @rdname swInputData-class
#' @export
setMethod("get_swSpinup", "swInputData", function(object) object@spinup)

#' @rdname swInputData-class
#' @export
setMethod("swSpinup_SpinupActive", "swInputData",
  function(object) swSpinup_SpinupActive(object@spinup))

#' @rdname swInputData-class
#' @export
setMethod("swSpinup_SpinupMode", "swInputData",
  function(object) swSpinup_SpinupMode(object@spinup))

#' @rdname swInputData-class
#' @export
setMethod("swSpinup_SpinupScope", "swInputData",
  function(object) swSpinup_SpinupScope(object@spinup))

#' @rdname swInputData-class
#' @export
setMethod("swSpinup_SpinupDuration", "swInputData",
  function(object) swSpinup_SpinupDuration(object@spinup))

#' @rdname swInputData-class
#' @export
setMethod("swSpinup_SpinupSeed", "swInputData",
  function(object) swSpinup_SpinupSeed(object@spinup))


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupActive",
  signature = "swInputData",
  function(object, value) {
    swSpinup_SpinupActive(object@spinup) <- as.logical(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupMode",
  signature = "swInputData",
  function(object, value) {
    swSpinup_SpinupMode(object@spinup) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupScope",
  signature = "swInputData",
  function(object, value) {
    swSpinup_SpinupScope(object@spinup) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupDuration",
  signature = "swInputData",
  function(object, value) {
    swSpinup_SpinupDuration(object@spinup) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swSpinup_SpinupSeed",
  signature = "swInputData",
  function(object, value) {
    swSpinup_SpinupSeed(object@spinup) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swSpinup",
  signature = "swInputData",
  function(object, value) {
    set_swSpinup(object@spinup) <- value
    object
  }
)




# Methods for slot \code{carbon}
#' @rdname swInputData-class
#' @export
setMethod("get_swCarbon", "swInputData", function(object) object@carbon)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCarbon_Use_Bio",
  signature = "swInputData",
  function(object) swCarbon_Use_Bio(object@carbon)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCarbon_Use_WUE",
  signature = "swInputData",
  function(object) swCarbon_Use_WUE(object@carbon)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCarbon_Scenario",
  signature = "swInputData",
  function(object) swCarbon_Scenario(object@carbon)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCarbon_DeltaYear",
  signature = "swInputData",
  function(object) swCarbon_DeltaYear(object@carbon)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swCarbon_CO2ppm",
  signature = "swInputData",
  function(object) swCarbon_CO2ppm(object@carbon)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swCarbon",
  signature = "swInputData",
  function(object, value) {
    set_swCarbon(object@carbon) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCarbon_Use_Bio",
  signature = "swInputData",
  function(object, value) {
    swCarbon_Use_Bio(object@carbon) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCarbon_Use_WUE",
  signature = "swInputData",
  function(object, value) {
    swCarbon_Use_WUE(object@carbon) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCarbon_Scenario",
  signature = "swInputData",
  function(object, value) {
    swCarbon_Scenario(object@carbon) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCarbon_DeltaYear",
  signature = "swInputData",
  function(object, value) {
    swCarbon_DeltaYear(object@carbon) <- as.integer(value)
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swCarbon_CO2ppm",
  signature = "swInputData",
  function(object, value) {
    swCarbon_CO2ppm(object@carbon) <- value
    object
  }
)


# Methods for slot \code{output}
#' @rdname swInputData-class
#' @export
setMethod("get_swOUT", "swInputData", function(object) object@output)

#' @rdname swInputData-class
#' @export
setMethod(
  "swOUT_TimeStep",
  signature = "swInputData",
  function(object) swOUT_TimeStep(object@output)
)

#' @rdname swInputData-class
#' @export
setMethod(
  "swOUT_OutputSeparator",
  signature = "swInputData",
  function(object) swOUT_OutputSeparator(object@output)
)


#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "set_swOUT",
  signature = "swInputData",
  function(object, value) {
    set_swOUT(object@output) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swOUT_TimeStep",
  signature = "swInputData",
  function(object, value) {
    swOUT_TimeStep(object@output) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swOUT_TimeStepsForEveryKey",
  signature = "swInputData",
  function(object, value) {
    swOUT_TimeStepsForEveryKey(object@output) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swOUT_OutputSeparator",
  signature = "swInputData",
  function(object, value) {
    swOUT_OutputSeparator(object@output) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "activate_swOUT_OutKey",
  signature = "swInputData",
  function(object, value) {
    activate_swOUT_OutKey(object@output) <- value
    object
  }
)

#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "deactivate_swOUT_OutKey",
  signature = "swInputData",
  function(object, value) {
    deactivate_swOUT_OutKey(object@output) <- value
    object
  }
)


# Methods for slot \code{log}
#' @rdname swInputData-class
#' @export
setReplaceMethod(
  "swLog_setLine",
  signature = "swInputData",
  function(object, value) {
    if (object@log@UsedLines <= object@log@MaxLines) {
      object@log@LogData[object@log@UsedLines] <- value
      object@log@UsedLines <- object@log@UsedLines + 1
    }
    validObject(object)
    object
  }
)



#' @rdname swInputData-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object="swInputData",file="character"),
  function(object,file) {
    print("TODO: method 'swReadLines' for class 'swInputData' is not up-to-date; hard-coded indices are incorrect")

    object@files <- swReadLines(object@files,file)
    object@files@ProjDir <- dirname(file)
    object@years <- swReadLines(object@years,file.path(object@files@ProjDir, object@files@InFiles[2]))
    object@weather <- swReadLines(object@weather,file.path(object@files@ProjDir, object@files@InFiles[6]))
    weatherFiles <- list.files(path=file.path(object@files@ProjDir,dirname(object@files@WeatherPrefix)), pattern=basename(object@files@WeatherPrefix), include.dirs=F, recursive=F, full.names=T)
    object@weatherHistory <- list()
    if(length(weatherFiles) > 0) {
      for(i in 1:length(weatherFiles)) {
        wd <- new("swWeatherData",year=0)
        wd <- swReadLines(wd, weatherFiles[i])
        object@weatherHistory[[i]] <- wd
      }
    }

    object@cloud <- swReadLines(object@cloud,file.path(object@files@ProjDir, object@files@InFiles[9]))
    if(all(file.exists(file.path(object@files@ProjDir, object@files@InFiles[7:8]))))
      object@markov <- swReadLines(object@markov,file.path(object@files@ProjDir, object@files@InFiles[7:8]))
    object@prod <- swReadLines(object@prod,file.path(object@files@ProjDir, object@files@InFiles[10]))
    object@site <- swReadLines(object@site,file.path(object@files@ProjDir, object@files@InFiles[4]))
    object@soils <- swReadLines(object@soils,file.path(object@files@ProjDir, object@files@InFiles[5]))
    if(file.exists(file.path(object@files@ProjDir, object@files@InFiles[11]))) {#Optional File
      object@estab <- swReadLines(object@estab,c(file.path(object@files@ProjDir, object@files@InFiles[11]),object@files@ProjDir))
    }
    object@output <- swReadLines(object@output,file.path(object@files@ProjDir, object@files@InFiles[14]))
    object@swc <- swReadLines(object@swc,file.path(object@files@ProjDir, object@files@InFiles[13]))
    object@carbon <- swReadLines(object@carbon, file.path(object@files@ProjDir, object@files@InFiles[12]))
    return(object)
  }
)
# nolint end
