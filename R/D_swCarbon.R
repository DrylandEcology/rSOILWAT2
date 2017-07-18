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
  
  representation(CarbonUseBio='integer', CarbonUseSto='integer', Scenario='character', DeltaYear='integer'),
  
  prototype=prototype(
    CarbonUseBio = as.integer(0),
    CarbonUseSto = as.integer(0),
    Scenario = as.character("RCP85"),
    DeltaYear = as.integer(0)
  ))

setMethod(f="swClear",
          signature="swCarbon",
          definition=function(object) {
            object@CarbonUseBio = as.integer(0)
            object@CarbonUseSto = as.integer(0)
            object@Scenario = as.character("RCP85")
            object@DeltaYear = as.integer(0)
            return(object)
          })
