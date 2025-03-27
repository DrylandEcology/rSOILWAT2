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


#' Class \code{"swSWC_hist"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#'
#' @param object An object of class \code{\linkS4class{swSWC_hist}}.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @slot year An integer value. The calendar year of the \var{SWC}
#'   \code{data} object.
#' @slot data A 365 x 4 or 366 x 4 matrix representing daily \var{SWC}
#'   data for one calendar \code{year} with columns \var{doy}, \var{lyr},
#'   \var{swc}, \var{st_err}.
#'
#' @name swSWC_hist-class
#' @export
setClass(
  "swSWC_hist",
  slot = c(data = "matrix", year = "integer"),
  prototype = list(
    data = array(
      NA_real_,
      dim = c(366, 4),
      dimnames = list(NULL, c("doy", "lyr", "swc", "st_err"))
    ),
    year = NA_integer_
  )
)


#' @rdname swSWC_hist-class
#' @export
swSWC_hist <- function(...) {
  def <- new("swSWC_hist")
  sns <- slotNames("swSWC_hist")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSWC_hist")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values; this is to prevent simulation runs with
  # accidentally incorrect values

  # Use prototype "def", but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSWC_hist", tmp))
}


#' @rdname swSWC_hist-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swSWC_hist", file = "character"),
  function(object, file) {
    stop("swReadLines is defunct.", call. = FALSE)
    object@year <- as.integer(strsplit(x = file, split = ".",
      fixed = TRUE)[[1]][2])
    infiletext <- readLines(con = file)
    #should be no empty lines
    infiletext <- infiletext[infiletext != ""]
    days <- length(infiletext) - 2
    data <- matrix(data = NA, nrow = days, ncol = 4)
    colnames(data) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    for (i in 3:length(infiletext)) {
      data[i - 2, ] <- readNumerics(infiletext[i], 4)
    }
    object@data <- data
    object
})
# nolint end

##########################swcsetup.in#########################################

#' Class \code{"swSWC"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swSWC}}.
#' @param file A character string. The file name from which to read.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @param year An integer value. The calendar year of the \var{SWC}
#'   \code{data} object.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swSWC")
#' x <- new("swSWC")
#' x <- swSWC()
#'
#' @name swSWC-class
#' @export
setClass(
  "swSWC",
  slot = c(
    UseSWCHistoricData = "logical",
    DataFilePrefix = "character",
    FirstYear = "integer",
    Method = "integer",
    History = "list"
  ),
  prototype = list(
    UseSWCHistoricData = NA,
    DataFilePrefix = NA_character_,
    FirstYear = NA_integer_,
    Method = NA_integer_,
    History = list()
  )
)

#' @rdname swSWC-class
#' @export
swSWC <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "swc")
  sns <- slotNames("swSWC")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSWC")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)


  # We don't set values for slot `History` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("History" %in% dns)) {
    def@History <- list()
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSWC", tmp))
}


#' @rdname swSWC-class
#' @export
setMethod("swSWC_use", "swSWC", function(object) object@UseSWCHistoricData)

#' @rdname swSWC-class
#' @export
setMethod("swSWC_prefix", "swSWC", function(object) object@DataFilePrefix)

#' @rdname swSWC-class
#' @export
setMethod("swSWC_FirstYear", "swSWC", function(object) object@FirstYear)

#' @rdname swSWC-class
#' @export
setMethod("swSWC_Method", "swSWC", function(object) object@Method)

#' @rdname swSWC-class
#' @export
setMethod("swSWC_HistoricList", "swSWC", function(object) object@History)

#' @rdname swSWC-class
#' @export
setMethod(
  "swSWC_HistoricData",
  "swSWC",
  function(object, year) {
    index <- which(names(object@History) == as.character(year))
    if (length(index) != 1) {
      warning("swc historic data Index has wrong length.", call. = FALSE)
      return(NULL)
    }
    if (object@History[[index]]@year != as.integer(year)) {
      warning(
        "Somethings wrong with the historical soil moisture data.",
        call. = FALSE
      )
    }

    object@History[[index]]
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_use",
  signature = c(object = "swSWC", value = "logical"),
  function(object, value) {
    object@UseSWCHistoricData[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_prefix",
  signature = c(object = "swSWC", value = "character"),
  function(object, value) {
    object@DataFilePrefix <- as.character(value)
    validObject(object)
    object
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_FirstYear",
  signature = c(object = "swSWC", value = "integer"),
  function(object, value) {
    object@FirstYear <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_Method",
  signature = c(object = "swSWC", value = "integer"),
  function(object, value) {
    object@Method <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_HistoricList",
  signature = c(object = "swSWC", value = "list"),
  function(object, value) {
    object@History <- value
    validObject(object)
    object
  }
)

#' @rdname swSWC-class
#' @export
setReplaceMethod(
  "swSWC_HistoricData",
  signature = c(object = "swSWC", value = "swSWC_hist"),
  function(object, value) {
    index <- which(names(object@History) == as.character(value@year))
    if (length(index) == 0) {
      object@History[[length(object@History) + 1]] <- value
      years <- unlist(lapply(object@History, function(x) x@year))
      ids_sorted <- sort.list(years, na.last = TRUE)
      object@History <- object@History[ids_sorted]
      years <- years[ids_sorted]
      names(object@History) <- as.character(years)
      if (!all(years == cummax(years))) {
        stop("SWC data is missing", call. = FALSE)
      }

    } else if (length(index) == 1) {
      object@History[[index]] <- value

    } else {
      stop("To many index. Not set", call. = FALSE)
    }

    object
  }
)


#' @rdname swSWC-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swSWC", file = "character"),
  function(object, file) {
    stop("swReadLines is defunct", call. = FALSE)
    infiletext <- readLines(con = file)
    #should be no empty lines
    infiletext <- infiletext[infiletext != ""]
    object@UseSWCHistoricData <- readLogical(infiletext[4])
    object@DataFilePrefix <- readCharacter(infiletext[5])
    object@FirstYear <- readInteger(infiletext[6])
    object@Method <- readInteger(infiletext[7])
    return(object)
  }
)
# nolint end
