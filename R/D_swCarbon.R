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


#############################CARBON DATA#########################################
#' @export
setClass("swCarbon",
  representation(CarbonUseBio = 'integer', CarbonUseWUE = 'integer',
    Scenario = 'character', DeltaYear = 'integer', CO2ppm = 'matrix'),

  prototype = prototype(
    CarbonUseBio = as.integer(0),
    CarbonUseWUE = as.integer(0),
    Scenario = as.character("Default"),  # This is not used in rSOILWAT2, but it's useful to see what scenario was used in the input object
    DeltaYear = as.integer(0),
    CO2ppm = as.matrix(data.frame(Year = 1979:2010, CO2ppm = rep(360, 32)))
  )
)

setMethod(f = "swClear", signature = "swCarbon", definition = function(object) {
  object@CarbonUseBio = as.integer(0)
  object@CarbonUseWUE = as.integer(0)
  object@Scenario = as.character("Default")
  object@DeltaYear = as.integer(0)
  object@CO2ppm = as.matrix(data.frame(Year = 1979:2010, CO2ppm = rep(360, 32)))

  object
})


setValidity("swCarbon", function(object) {
  val <- TRUE

  if (!all(c("Year", "CO2ppm") == colnames(object@CO2ppm)) ||
    length(colnames(object@CO2ppm)) != 2) {
    msg <- "@CO2ppm: column names must be 'Year' and 'CO2ppm'"
    val <- if (isTRUE(val)) msg else c(val, msg)

  } else {
    is_bad <- any(is.na(object@CO2ppm[, "Year"]) |
      round(object@CO2ppm[, "Year"]) != object@CO2ppm[, "Year"])
    if (is_bad) {
      msg <- "@CO2ppm: has missing and/or non-integer-like years"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    is_bad <- !all(diff(object@CO2ppm[, "Year"]) == 1)
    if (is_bad) {
      msg <- "@CO2ppm: years are not consecutive"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    ids_bad <- is.na(object@CO2ppm[, "CO2ppm"]) | object@CO2ppm[, "CO2ppm"] < 0
    if (any(ids_bad)) {
      msg <- "@CO2ppm: has missing and/or negative CO2-concentration values"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
  }

  val
})

