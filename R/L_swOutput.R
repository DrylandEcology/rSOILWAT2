###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2016}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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


# This File is the object that will hold all the output from Soilwat.
#
# Author: Ryan Murphy
###############################################################################


#' Slot names of \linkS4class{swOutput}
#' @return Standardized named vector for easier access to slots of class
#'  \linkS4class{swOutput}.
#' @export
sw_out_flags <- function() {
  c(sw_aet = "AET",
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
    sw_temp = "TEMP",
    sw_transp = "TRANSP",
    sw_vwcbulk = "VWCBULK",
    sw_vwcmatric = "VWCMATRIC",
    sw_veg = "CO2EFFECTS",
    sw_wetdays = "WETDAY",
    sw_logfile = "LOG")
}

###################Generic Class to Hold One Output KEY########################
#' @export
setClass("swOutput_KEY", slot = c(Title = "character", TimeStep = "integer",
  Columns = "integer", Day = "matrix", Week = "matrix", Month = "matrix", Year = "matrix"))

setMethod("swOutput_KEY_Period", signature = "swOutput_KEY", function(object, index) {
  slot(object, slotNames(object)[-(1:3)][index])
})
setMethod("swOutput_KEY_TimeStep", signature = "swOutput_KEY", function(object) {
  if (length(object@TimeStep) == 1 & object@TimeStep <= 4) {
    return(object@TimeStep)
  } else stop("TimeStep to long or out of Range.")
})
setMethod("swOutput_KEY_Columns", signature = "swOutput_KEY", function(object) {
  object@Columns
})

setReplaceMethod("swOutput_KEY_Period", signature = "swOutput_KEY",
  function(object, index, value) {
    slot(object, slotNames(object)[-(1:3)][index]) <- value
    object
  }
)


##################Main Storage##################
#' @export
setClass("swOutput", slot = c(yr_nrow = "integer", mo_nrow = "integer",
  wk_nrow = "integer", dy_nrow = "integer",
  WTHR = "swOutput_KEY", TEMP = "swOutput_KEY", PRECIP = "swOutput_KEY",
  SOILINFILT = "swOutput_KEY", RUNOFF = "swOutput_KEY", ALLH2O = "swOutput_KEY",
  VWCBULK = "swOutput_KEY", VWCMATRIC = "swOutput_KEY", SWCBULK = "swOutput_KEY",
  SWABULK = "swOutput_KEY", SWAMATRIC = "swOutput_KEY", SWPMATRIC = "swOutput_KEY",
  SURFACEWATER = "swOutput_KEY", TRANSP = "swOutput_KEY", EVAPSOIL = "swOutput_KEY",
  EVAPSURFACE = "swOutput_KEY", INTERCEPTION = "swOutput_KEY", LYRDRAIN = "swOutput_KEY",
  HYDRED = "swOutput_KEY", ET = "swOutput_KEY", AET = "swOutput_KEY",
  PET = "swOutput_KEY", WETDAY = "swOutput_KEY", SNOWPACK = "swOutput_KEY",
  DEEPSWC = "swOutput_KEY", SOILTEMP = "swOutput_KEY", ALLVEG = "swOutput_KEY",
  ESTABL = "swOutput_KEY", CO2EFFECTS = "swOutput_KEY"))


setMethod("$", signature = "swOutput", function(x, name) {slot(x, name)})
setMethod("swOutput_getKEY", signature = "swOutput", function(object, index) {
  slot(object, slotNames(object)[-(1:4)][index])
})

setReplaceMethod("swOutput_getKEY", signature = c(object = "swOutput", value = "swOutput_KEY"),
  function(object, index, value) {
    slot(object, slotNames(object)[-(1:4)][index]) <- value
    object
  }
)

setMethod("initialize", signature = "swOutput", function(.Object, ...) {
  # prepare yearly output
  x <- inputData
  swOUT_TimeStep(x) <- 3L
  swOUT_useTimeStep(x) <- TRUE
  .Object <- sw_outputData(x)

  validObject(.Object)
  .Object
})

