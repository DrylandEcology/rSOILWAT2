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


#' Slot names of \linkS4class{swOutput}
#' @return Standardized named vector for easier access to slots of class
#'  \linkS4class{swOutput}.
#' @export
sw_out_flags <- function() {
  tmp <- rSW2_glovars[["kSOILWAT2"]][["OutKeys"]]
  res <- c(
    sw_aet = tmp["SW_AET"],
    sw_deepdrain = tmp["SW_DEEPSWC"],
    sw_estabs = tmp["SW_ESTAB"],
    sw_evsoil = tmp["SW_EVAPSOIL"],
    sw_evapsurface = tmp["SW_EVAPSURFACE"],
    sw_hd = tmp["SW_HYDRED"],
    sw_inf_soil = tmp["SW_SOILINF"],
    sw_interception = tmp["SW_INTERCEPTION"],
    sw_percolation = tmp["SW_LYRDRAIN"],
    sw_pet = tmp["SW_PET"],
    sw_precip = tmp["SW_PRECIP"],
    sw_runoff = tmp["SW_RUNOFF"],
    sw_snow = tmp["SW_SNOWPACK"],
    sw_soiltemp = tmp["SW_SOILTEMP"],
    sw_surfaceWater = tmp["SW_SURFACEW"],
    sw_swp = tmp["SW_SWPMATRIC"],
    sw_swabulk = tmp["SW_SWABULK"],
    sw_swcbulk = tmp["SW_SWCBULK"],
    sw_swa = tmp["SW_SWA"],
    sw_temp = tmp["SW_TEMP"],
    sw_transp = tmp["SW_TRANSP"],
    sw_vwcbulk = tmp["SW_VWCBULK"],
    sw_vwcmatric = tmp["SW_VWCMATRIC"],
    sw_co2effects = tmp["SW_CO2EFFECTS"],
    sw_veg = tmp["SW_BIOMASS"],
    sw_wetdays = tmp["SW_WETDAY"]
  )

  # Fix names
  tmp <- sapply(strsplit(names(res), split = ".", fixed = TRUE), `[`, j = 1)
  names(res) <- tmp

  res
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
swOutput_KEY <- setClass(
  "swOutput_KEY",
  slot = c(
    Title = "character",
    TimeStep = "integer",
    Columns = "integer",
    Day = "matrix",
    Week = "matrix",
    Month = "matrix",
    Year = "matrix"
  ),
  prototype = list(
    Title = character(),
    TimeStep = integer(),
    Columns = integer(),
    Day = matrix(NA_real_)[0, 0],
    Week = matrix(NA_real_)[0, 0],
    Month = matrix(NA_real_)[0, 0],
    Year = matrix(NA_real_)[0, 0]
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
    slot(object, rSW2_glovars[["kSOILWAT2"]][["OutPeriods"]][index])
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
    slot(object, rSW2_glovars[["kSOILWAT2"]][["OutPeriods"]][index]) <- value
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
swOutput <- setClass(
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
  ),
  prototype = list(
    version = rSW2_version(),
    timestamp = rSW2_timestamp(),
    yr_nrow = integer(),
    mo_nrow = integer(),
    wk_nrow = integer(),
    dy_nrow = integer(),
    WTHR = swOutput_KEY(),
    TEMP = swOutput_KEY(),
    PRECIP = swOutput_KEY(),
    SOILINFILT = swOutput_KEY(),
    RUNOFF = swOutput_KEY(),
    ALLH2O = swOutput_KEY(),
    VWCBULK = swOutput_KEY(),
    VWCMATRIC = swOutput_KEY(),
    SWCBULK = swOutput_KEY(),
    SWA = swOutput_KEY(),
    SWABULK = swOutput_KEY(),
    SWAMATRIC = swOutput_KEY(),
    SWPMATRIC = swOutput_KEY(),
    SURFACEWATER = swOutput_KEY(),
    TRANSP = swOutput_KEY(),
    EVAPSOIL = swOutput_KEY(),
    EVAPSURFACE = swOutput_KEY(),
    INTERCEPTION = swOutput_KEY(),
    LYRDRAIN = swOutput_KEY(),
    HYDRED = swOutput_KEY(),
    ET = swOutput_KEY(),
    AET = swOutput_KEY(),
    PET = swOutput_KEY(),
    WETDAY = swOutput_KEY(),
    SNOWPACK = swOutput_KEY(),
    DEEPSWC = swOutput_KEY(),
    SOILTEMP = swOutput_KEY(),
    ALLVEG = swOutput_KEY(),
    ESTABL = swOutput_KEY(),
    CO2EFFECTS = swOutput_KEY(),
    BIOMASS = swOutput_KEY()
  )
)


setValidity(
  "swOutput",
  function(object) {
    TRUE
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
