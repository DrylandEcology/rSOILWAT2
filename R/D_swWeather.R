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

#######################Monthly Scaling Params#################################

#' Class \code{"swMonthlyScalingParams"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swMonthlyScalingParams")
#' x <- new("swMonthlyScalingParams")
#' x <- swMonthlyScalingParams()
#'
#' @name swMonthlyScalingParams-class
#' @export
setClass(
  "swMonthlyScalingParams",
  slots = c(MonthlyScalingParams = "matrix"),
  prototype = list(
    MonthlyScalingParams = array(
      NA_real_,
      dim = c(12, 8),
      dimnames = list(
        NULL,
        c("PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "actVP", "shortWR")
      )
    )
  )
)

setValidity(
  "swMonthlyScalingParams",
  function(object) {
    val <- TRUE
    temp <- dim(object@MonthlyScalingParams)

    if (temp[2] != 6) {
      msg <- paste(
        "@MonthlyScalingParams must have exactly 6 columns ",
        "corresponding to PPT, MaxT, MinT, SkyCover, Wind, rH"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (temp[1] != 12) {
      msg <- paste(
        "@MonthlyScalingParams must have exactly 12 rows",
        "corresponding months."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
   }
)

#' @rdname swMonthlyScalingParams-class
#' @export
swMonthlyScalingParams <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "weather")
  sns <- slotNames("swMonthlyScalingParams")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swMonthlyScalingParams")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  if ("MonthlyScalingParams" %in% dns) {
    # Guarantee names
    dimnames(dots[["MonthlyScalingParams"]]) <- dimnames(
      def@MonthlyScalingParams
    )
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swMonthlyScalingParams", tmp))
}



#####################WEATHERSETUP.IN###################################

#' Class \code{"swWeather"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swWeather}}.
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
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swWeather")
#' x <- new("swWeather")
#' x <- swWeather()
#'
#' @name swWeather-class
#' @export
setClass(
  "swWeather",
  slots = c(
    UseSnow = "logical",
    pct_SnowDrift = "numeric",
    pct_SnowRunoff = "numeric",
    use_weathergenerator = "logical",
    use_weathergenerator_only = "logical",
    FirstYear_Historical = "integer",
    use_cloudCoverMonthly = "logical",
    use_windSpeedMonthly = "logical",
    use_relHumidityMonthly = "logical",
    has_temp2 = "logical",
    has_ppt = "logical",
    has_cloudCover = "logical",
    has_sfcWind = "logical",
    has_windComp = "logical",
    has_hurs = "logical",
    has_hurs2 = "logical",
    has_huss = "logical",
    has_tdps = "logical",
    has_vp = "logical",
    has_rsds = "logical"
  ),
  # TODO: this class should not contain `swMonthlyScalingParams` but
  # instead be a composition, i.e., have a slot of that class
  contains = "swMonthlyScalingParams",
  prototype = list(
    UseSnow = NA,
    pct_SnowDrift = NA_real_,
    pct_SnowRunoff = NA_real_,
    use_weathergenerator = NA,
    use_weathergenerator_only = NA,
    FirstYear_Historical = NA_integer_,
    use_cloudCoverMonthly = NA,
    use_windSpeedMonthly = NA,
    use_relHumidityMonthly = NA,
    has_temp2 = NA,
    has_ppt = NA,
    has_cloudCover = NA,
    has_sfcWind = NA,
    has_windComp = NA,
    has_hurs = NA,
    has_hurs2 = NA,
    has_huss = NA,
    has_tdps = NA,
    has_vp = NA,
    has_rsds = NA
  )
)

setValidity(
  "swWeather",
  function(object) {
    val <- TRUE
    sns <- setdiff(slotNames("swWeather"), inheritedSlotNames("swWeather"))

    for (sn in sns) {
      if (length(slot(object, sn)) != 1) {
        msg <- paste0("@", sn, " must have exactly one value.")
        val <- if (isTRUE(val)) msg else c(val, msg)
      }
    }

    val
  }
)


#' @rdname swWeather-class
#' @export
swWeather <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "weather")
  sns <- setdiff(slotNames("swWeather"), inheritedSlotNames("swWeather"))
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swWeather")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- setdiff(names(dots), inheritedSlotNames("swWeather"))

  # Fix "FirstYear_Historical"
  def@FirstYear_Historical <- -1L

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call(
    "new",
    args = c(
      "swWeather",
      if ("MonthlyScalingParams" %in% dns) {
        swMonthlyScalingParams(dots[["MonthlyScalingParams"]])
      } else {
        do.call(swMonthlyScalingParams, dots)
      },
      tmp
    )
  )
}


#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_DaysRunningAverage",
  "swWeather",
  function(object) object@DaysRunningAverage
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_FirstYearHistorical",
  "swWeather",
  function(object) {
    .Deprecated() # `FirstYear_Historical` is no longer used by SOILWAT2.
    object@FirstYear_Historical
  }
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_pct_SnowDrift",
  "swWeather",
  function(object) object@pct_SnowDrift
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_pct_SnowRunoff",
  "swWeather",
  function(object) object@pct_SnowRunoff
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_UseMarkov",
  "swWeather",
  function(object) object@use_weathergenerator
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_UseMarkovOnly",
  "swWeather",
  function(object) object@use_weathergenerator_only
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_UseSnow",
  "swWeather",
  function(object) object@UseSnow
)

#' @rdname swWeather-class
#' @export
setMethod(
  "swWeather_MonScalingParams",
  "swWeather",
  function(object) object@MonthlyScalingParams
)

#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_DaysRunningAverage",
  signature = "swWeather",
  function(object, value) {
    object@DaysRunningAverage <- as.integer(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_FirstYearHistorical",
  signature = "swWeather",
  function(object, value) {
    .Deprecated() # `FirstYear_Historical` is no longer used by SOILWAT2.
    object@FirstYear_Historical <- as.integer(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_pct_SnowDrift",
  signature = "swWeather",
  function(object, value) {
    object@pct_SnowDrift <- as.numeric(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_pct_SnowRunoff",
  signature = "swWeather",
  function(object, value) {
    object@pct_SnowRunoff <- as.numeric(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_UseMarkov",
  signature = "swWeather",
  function(object, value) {
    object@use_weathergenerator <- as.logical(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_UseMarkovOnly",
  signature = "swWeather",
  function(object, value) {
    object@use_weathergenerator_only <- as.logical(value)
    if (object@use_weathergenerator_only) {
      object@use_weathergenerator <- TRUE
    }
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_UseSnow",
  signature = "swWeather",
  function(object, value) {
    object@UseSnow <- as.logical(value)
    validObject(object)
    object
  }
)


#' @rdname swWeather-class
#' @export
setReplaceMethod(
  "swWeather_MonScalingParams",
  signature = "swWeather",
  function(object, value) {
    object@MonthlyScalingParams[] <- value
    validObject(object)
    object
  }
)



#' @rdname swWeather-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swWeather", file = "character"),
  function(object, file) {
    print(paste(
      "TODO: method 'swReadLines' for class 'swWeather' is not up-to-date;",
      "hard-coded indices are incorrect"
    ))
    infiletext <- readLines(con = file)

    object@UseSnow <- readLogical(infiletext[4])
    object@pct_SnowDrift <- readNumeric(infiletext[5])
    object@pct_SnowRunoff <- readNumeric(infiletext[6])
    object@use_weathergenerator <- readLogical(infiletext[7])
    object@FirstYear_Historical <- readInteger(infiletext[8])
    object@use_cloudCoverMonthly <- readLogical(infiletext[9])
    object@use_windSpeedMonthly <- readLogical(infiletext[10])
    object@use_relHumidityMonthly <- readLogical(infiletext[11])
    object@has_temp2 <- readLogical(infiletext[12])
    object@has_ppt <- readLogical(infiletext[13])
    object@has_cloudCover <- readLogical(infiletext[14])
    object@has_sfcWind <- readLogical(infiletext[15])
    object@has_windComp <- readLogical(infiletext[16])
    object@has_hurs <- readLogical(infiletext[17])
    object@has_hurs2 <- readLogical(infiletext[18])
    object@has_huss <- readLogical(infiletext[19])
    object@has_tdps <- readLogical(infiletext[20])
    object@has_vp <- readLogical(infiletext[21])
    object@has_rsds <- readLogical(infiletext[22])

    data <- matrix(data = c(rep(1, 12), rep(NA, 12 * 5)), nrow = 12, ncol = 8)
    colnames(data) <- c("PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "actVP", "shortWR")
    rownames(data) <- c("January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", "November", "December")

    for (i in 21:32) {
      data[i - 20, ] <- readNumerics(infiletext[i], 8)[2:8]
    }
    object@MonthlyScalingParams <- data

    object
  }
)
# nolint end
