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


# TODO: Add comment
#
# Author: ryan
###############################################################################

###############################################################SOILS#####################################################################
#' @export
setClass("swSoils", slots = c(Layers = "matrix"))

swSoilLayers_validity<-function(object){
	if(dim(object@Layers)[1]==0)
		return("@Layers has to have some Layers.")
	if(dim(object@Layers)[2]!=12)
		return("@Layers has to have 12 columns.")
	TRUE
}
setValidity("swSoils",swSoilLayers_validity)

setMethod("initialize", signature = "swSoils", function(.Object, ...) {
  def <- slot(inputData, "soils")

  # We don't set values for slot `Layers`; this is to prevent simulation runs with
  # accidentally incorrect values
  temp <- def@Layers
  temp[] <- NA_real_
  .Object@Layers <- temp[1, ]

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})


setMethod("swReadLines", signature=c(object="swSoils",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			infiletext <- infiletext[infiletext!=""]#get rid of extra spaces
			infiletext <- infiletext[17:length(infiletext)]#get rid of comments
			object@Layers <- matrix(data=NA, nrow=length(1:length(infiletext)),ncol=12)
			colnames(object@Layers)<-c("depth_cm","bulkDensity_g/cm^3","gravel_content","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","transpForb_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			for(i in 1:length(infiletext)) {
				object@Layers[i,] = readNumerics(infiletext[i],12)
			}
			return(object)
		})
