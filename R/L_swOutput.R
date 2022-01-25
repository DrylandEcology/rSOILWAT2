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


# TODO: link this to C code
# Note: the values must match those of rSW2_glovars[["kSOILWAT2"]][["OutKeys"]]
#' Slot names of \linkS4class{swOutput}
#' @return Standardized named vector for easier access to slots of class
#'  \linkS4class{swOutput}.
#' @export
sw_out_flags <- function() {
  c(
    sw_aet = "AET",
    sw_deepdrain = "DEEPSWC",
    sw_estabs = "ESTABL",
    sw_evsoil = "EVAPSOIL",
    sw_evapsurface = "EVAPSURFACE",
    sw_hd = "HYDRED",
    sw_inf_soil = "SOILINFILT",
    sw_interception = "INTERCEPTION",
    sw_percolation = "LYRDRAIN",
    sw_pet = "PET",
    sw_precip = "PRECIP",
    sw_runoff = "RUNOFF",
    sw_snow = "SNOWPACK",
    sw_soiltemp = "SOILTEMP",
    sw_surfaceWater = "SURFACEWATER",
    sw_swp = "SWPMATRIC",
    sw_swabulk = "SWABULK",
    sw_swcbulk = "SWCBULK",
    sw_swa = "SWA",
    sw_temp = "TEMP",
    sw_transp = "TRANSP",
    sw_vwcbulk = "VWCBULK",
    sw_vwcmatric = "VWCMATRIC",
    sw_co2effects = "CO2EFFECTS",
    sw_veg = "BIOMASS",
    sw_wetdays = "WETDAY",
    sw_logfile = "LOG"
  )
}

###################Generic Class to Hold One Output KEY########################
#' Class \code{"swOutput_KEY"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swOutput_KEY}}.
#' @param value A value to assign to a specific slot of the object.
#' @param index An integer value. One of the four possible time steps.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swOutput_KEY")
#' x <- new("swOutput_KEY")
#'
#' @name swOutput_KEY-class
#' @export
setClass(
  "swOutput_KEY",
  slot = c(
    Title = "character",
    TimeStep = "integer",
    Columns = "integer",
    Day = "matrix",
    Week = "matrix",
    Month = "matrix",
    Year = "matrix"
  )
)

setValidity(
  "swOutput_KEY",
  function(object) {
    val <- TRUE
    ntemp <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]

    if (
      length(object@TimeStep) != 1 ||
        object@TimeStep > ntemp ||
        object@TimeStep < 1
    ) {
      msg <- "@TimeStep must have exactly 1 value between 1 and SW_OUTNPERIODS"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swOutput_KEY-class
setMethod(
  "swOutput_KEY_Period",
  signature = "swOutput_KEY",
  function(object, index) {
    slot(object, rSW2_glovars[["sw_TimeSteps"]][index])
  }
)

#' @rdname swOutput_KEY-class
setMethod(
  "swOutput_KEY_TimeStep",
  signature = "swOutput_KEY",
  function(object) object@TimeStep
)

#' @rdname swOutput_KEY-class
setMethod(
  "swOutput_KEY_Columns",
  signature = "swOutput_KEY",
  function(object) object@Columns
)

#' @rdname swOutput_KEY-class
setReplaceMethod(
  "swOutput_KEY_Period",
  signature = "swOutput_KEY",
  function(object, index, value) {
    slot(object, rSW2_glovars[["sw_TimeSteps"]][index]) <- value
    validObject(object)
    object
  }
)


##################Main Storage##################
#' Class \code{"swOutput"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swOutput}}.
#' @param x An object of class \code{\linkS4class{swOutput}}.
#' @param value A value to assign to a specific slot of the object.
#' @param index An integer value. One of the four possible time steps.
#' @param name A character string. The name of the of the slots of this class.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swOutput")
#' x <- new("swOutput")
#'
#' @name swOutput-class
#' @export
setClass(
  "swOutput",
  slot = c(
    version = "character",
    timestamp = "numeric",
    yr_nrow = "integer",
    mo_nrow = "integer",
    wk_nrow = "integer",
    dy_nrow = "integer",
    WTHR = "swOutput_KEY",
    TEMP = "swOutput_KEY",
    PRECIP = "swOutput_KEY",
    SOILINFILT = "swOutput_KEY",
    RUNOFF = "swOutput_KEY",
    ALLH2O = "swOutput_KEY",
    VWCBULK = "swOutput_KEY",
    VWCMATRIC = "swOutput_KEY",
    SWCBULK = "swOutput_KEY",
    SWA = "swOutput_KEY",
    SWABULK = "swOutput_KEY",
    SWAMATRIC = "swOutput_KEY",
    SWPMATRIC = "swOutput_KEY",
    SURFACEWATER = "swOutput_KEY",
    TRANSP = "swOutput_KEY",
    EVAPSOIL = "swOutput_KEY",
    EVAPSURFACE = "swOutput_KEY",
    INTERCEPTION = "swOutput_KEY",
    LYRDRAIN = "swOutput_KEY",
    HYDRED = "swOutput_KEY",
    ET = "swOutput_KEY",
    AET = "swOutput_KEY",
    PET = "swOutput_KEY",
    WETDAY = "swOutput_KEY",
    SNOWPACK = "swOutput_KEY",
    DEEPSWC = "swOutput_KEY",
    SOILTEMP = "swOutput_KEY",
    ALLVEG = "swOutput_KEY",
    ESTABL = "swOutput_KEY",
    CO2EFFECTS = "swOutput_KEY",
    BIOMASS = "swOutput_KEY"
  )
)


#' @rdname swOutput-class
#' @export
setMethod(
  "initialize",
  signature = "swOutput",
  function(.Object) {

    slot(.Object, "version") <- rSW2_version()
    slot(.Object, "timestamp") <- rSW2_timestamp()

    validObject(.Object)

    .Object
  }
)

setValidity(
  "swOutput",
  function(object) {
    TRUE
  }
)


#' @rdname get_version
setMethod(
  f = "get_version",
  signature = "swOutput",
  definition = function(object) {
    tmp <- try(object@version, silent = TRUE)
    if (inherits(tmp, "try-error")) NA else tmp
  }
)

#' @rdname get_timestamp
setMethod(
  f = "get_timestamp",
  signature = "swOutput",
  definition = function(object) {
    tmp <- try(object@timestamp, silent = TRUE)
    if (inherits(tmp, "try-error")) NA else tmp
  }
)


#' @rdname swOutput-class
#' @export
setMethod("$", signature = "swOutput", function(x, name) slot(x, name))

#TODO: use C key2str to access slot
#' @rdname swOutput-class
#' @export
setMethod(
  "swOutput_getKEY",
  signature = "swOutput",
  function(object, index) {
    nid <- seq_len(6)
    slot(object, slotNames("swOutput")[-nid][index])
  }
)

#TODO: use C key2str to access slot
#' @rdname swOutput-class
#' @export
setReplaceMethod(
  "swOutput_setKEY",
  signature = c(object = "swOutput", value = "swOutput_KEY"),
  function(object, index, value) {
    nid <- seq_len(6)
    slot(object, slotNames("swOutput")[-nid][index]) <- value
    object
  }
)
