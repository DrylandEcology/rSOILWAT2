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
  
  representation(CarbonUseBio='integer', CarbonUseWUE='integer', Scenario='character', DeltaYear='integer', CO2ppm='vector'),
  
  prototype=prototype(
    CarbonUseBio = as.integer(0),
    CarbonUseWUE = as.integer(0),
    Scenario = as.character("Default"),  # This is not used in rSOILWAT2, but it's useful to see what scenario was used in the input object
    DeltaYear = as.integer(0),
    CO2ppm = c(rep(360.0, 2500))  # Index of value represents the year; we make the implicit assumption that the input biomass is at 360 ppm
  ))

setMethod(f="swClear",
          signature="swCarbon",
          definition=function(object) {
            object@CarbonUseBio = as.integer(0)
            object@CarbonUseWUE = as.integer(0)
            object@Scenario = as.character("Default")
            object@DeltaYear = as.integer(0)
            object@CO2ppm = c(rep(360.0, 2500))
            return(object)
          })
