###############################################################################
#Rsoilwat and Rsoilwat31
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
swSoilLayers <- setClass("swSoils",representation(Layers="matrix"),
		prototype=prototype(Layers=matrix(data=c(5,10,20,30,40,60,80,85,				#depth
								1.43,1.41,1.39,1.39,1.38,1.15,1.31,1.31,				#bulkden
								0,0,0,0,0,0,0,0,										#gravel content
								.8122,.1534,.0344,0,0,0,0,0,							#EvapBarSoil
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#trco grass
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#trco shrub
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#trco tree
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#trco forb
								.51,.44,.35,.32,.31,.32,.57,.57,						#sand
								.15,.26,.41,.45,.47,.47,.28,.28,						#clay
								0,0,0,0,0,0,0,0,										#imperm
								0.1860,0.3719,0.7438,1.1158,1.4877,2.2315,2.9754,2.9754), nrow=8,ncol=12,dimnames=list(NULL,c("depth_cm","bulkDensity_g/cm^3",
										"gravel_content","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","transpForb_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")))))

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
setMethod("swWriteLines", signature=c(object="swSoils", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(17+nrow(object@Layers))
			
			infiletext[1] <- "# Soil layer definitions"
			infiletext[2] <- "# Location: "
			infiletext[3] <- "#"
			infiletext[4] <- "# depth = (cm) lower limit of layer; layers must be in order of depth."
			infiletext[5] <- "# matricd = (g/cm^3) density of soil matric component of this layer (will be converted to bulk density with eq. 20 from from Saxton and Rawls 2006 SSAJ)."
			infiletext[6] <- "# fieldc = (cm^3/cm^3) field capacity soil water volume/volume soil."
			infiletext[7] <- "# wiltpt = (cm^3/cm^3) wilting point water volume/volume soil."
			infiletext[8] <- "# evco = (frac) proportion of total baresoil evap from this layer."
			infiletext[9] <- "# trco = (frac) proportion of total transpiration from this layer for each vegetation type (tree, shrub, grass)"
			infiletext[10] <- "# %sand = (frac) proportion of sand in layer (0-1.0)."
			infiletext[11] <- "# %clay = (frac) proportion of clay in layer (0-1.0)."
			infiletext[12] <- "# imperm = (frac) proportion of 'impermeability' to water percolation(/infiltration/drainage) in layer (0-1.0)"
			infiletext[13] <- "# soiltemp = the initial temperature of each soil layer (in celcius), from the day before the simulation starts"
			infiletext[14] <- "# Note that the evco and trco columns must sum to 1.0 or they will"
			infiletext[15] <- "# be normalized."
			infiletext[16] <- "#"
			infiletext[17] <- "# depth matricd	gravel_content	evco	trco_grass	trco_shrub	trco_tree	trco_forb	%sand	%clay	imperm	soiltemp"
			for(i in 1:nrow(object@Layers)) {
				infiletext[i+17] <- paste(format(object@Layers[i,1]),format(object@Layers[i,2]),format(object@Layers[i,3]),format(object@Layers[i,4]),format(object@Layers[i,5]),format(object@Layers[i,6]),
						format(object@Layers[i,7]),format(object@Layers[i,8]),format(object@Layers[i,9]),format(object@Layers[i,10]),format(object@Layers[i,11]),format(object@Layers[i,12]),sep="\t")
			}
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
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
