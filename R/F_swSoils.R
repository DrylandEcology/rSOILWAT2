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
setMethod(f="initialize",signature="swSoils",definition=function(.Object,Layers=NULL){
			if(is.null(Layers))
				Layers<-matrix(data=c(5,10,20,30,40,60,80,85,				#depth
								1.43,1.41,1.39,1.39,1.38,1.15,1.31,1.31,				#bulkden
								0,0,0,0,0,0,0,0,										#gravel content
								.8122,.1534,.0344,0,0,0,0,0,							#EvapBarSoil
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran grass
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#tran shrub
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran tree
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#trco forb
								.51,.44,.35,.32,.31,.32,.57,.57,						#sand
								.15,.26,.41,.45,.47,.47,.28,.28,						#clay
								0,0,0,0,0,0,0,0,										#imperm
								0.1860,0.3719,0.7438,1.1158,1.4877,2.2315,2.9754,2.9754), nrow=8,ncol=12)
			colnames(Layers)<-c("depth_cm","bulkDensity_g/cm^3","gravel_content","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","transpForb_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			.Object@Layers<-Layers
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swSoils",
		definition=function(object) {
			Layers<-matrix(data=NA, nrow=8,ncol=12)
			colnames(Layers)<-c("depth_cm","bulkDensity_g/cm^3","gravel_content","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","transpForb_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			object@Layers<-Layers
			return(object)
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
