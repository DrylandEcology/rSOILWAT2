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


#' Class \code{"swEstabSpecies"}
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
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swEstabSpecies")
#' x <- new("swEstabSpecies")
#' x <- swEstabSpecies()
#'
#' @name swEstabSpecies-class
#' @export
setClass(
  "swEstabSpecies",
  slot = c(
    fileName = "character",
    Name = "character",
    vegType = "integer",
    estab_lyrs = "integer",
    barsGERM = "numeric",
    barsESTAB = "numeric",
    min_pregerm_days = "integer",
    max_pregerm_days = "integer",
    min_wetdays_for_germ = "integer",
    max_drydays_postgerm = "integer",
    min_wetdays_for_estab = "integer",
    min_days_germ2estab = "integer",
    max_days_germ2estab = "integer",
    min_temp_germ = "numeric",
    max_temp_germ = "numeric",
    min_temp_estab = "numeric",
    max_temp_estab = "numeric"
  ),
  prototype = list(
    fileName = character(),
    Name = character(),
    vegType = integer(),
    estab_lyrs = integer(),
    barsGERM = numeric(),
    barsESTAB = numeric(),
    min_pregerm_days = integer(),
    max_pregerm_days = integer(),
    min_wetdays_for_germ = integer(),
    max_drydays_postgerm = integer(),
    min_wetdays_for_estab = integer(),
    min_days_germ2estab = integer(),
    max_days_germ2estab = integer(),
    min_temp_germ = numeric(),
    max_temp_germ = numeric(),
    min_temp_estab = numeric(),
    max_temp_estab = numeric()
  )

)

setValidity(
  "swEstabSpecies",
  function(object) TRUE
)

#' @rdname swEstabSpecies-class
#' @export
swEstabSpecies <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "estab")
  sns <- slotNames("swEstabSpecies")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swEstabSpecies")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swEstabSpecies", tmp))
}



#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "swEstabSpecies",
  definition = function(object, verbose = FALSE) {
    tmp <- try(validObject(object), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swEstabSpecies`.")
      }
      object <- suppressWarnings(swEstabSpecies(object))
    }

    object
  }
)



#############################ESTAB.IN#########################################
#' Class \code{"swEstab"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swEstab}}.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
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
#' showClass("swEstab")
#' x <- new("swEstab")
#' x <- swEstab()
#'
#' @name swEstab-class
#' @export
setClass(
  "swEstab",
  slot = c(useEstab = "logical", count = "integer"),
  contains = "swEstabSpecies",
  prototype = list(
    useEstab = NA,
    count = integer()
  )
)

setValidity(
  "swEstab",
  function(object) TRUE
)


#' @rdname swEstab-class
#' @export
swEstab <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "estab")
  sns <- setdiff(slotNames("swEstab"), inheritedSlotNames("swEstab"))
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swEstab")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- setdiff(names(dots), inheritedSlotNames("swEstab"))

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call(
    "new",
    args = c(
      "swEstab",
      if ("swEstabSpecies" %in% dns) {
        swEstabSpecies(dots[["swEstabSpecies"]])
      } else {
        do.call(swEstabSpecies, dots)
      },
      tmp
    )
  )
}

#' @rdname swEstab-class
#' @export
setMethod("swEstab_useEstab", "swEstab", function(object) object@useEstab)

#' @rdname swEstab-class
#' @export
setReplaceMethod(
  "swEstab_useEstab",
  signature = "swEstab",
  function(object, value) {
    object@useEstab <- as.logical(value)
    validObject(object)
    object
  }
)



#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "swEstab",
  definition = function(object, verbose = FALSE) {
    tmp <- try(validObject(object), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swEstab`.")
      }
      object <- suppressWarnings(swEstab(object))
    }

    object
  }
)
