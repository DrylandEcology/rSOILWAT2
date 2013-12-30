# TODO: Add comment
# 
# Author: ryan
###############################################################################

###############################################################SOILS#####################################################################
swSoilLayers <- setClass("swSoils",representation(Layers="matrix"),
		prototype=prototype(Layers=matrix(data=c(5,10,20,30,40,60,80,85,				#depth
								1.43,1.41,1.39,1.39,1.38,1.15,1.31,1.31,				#bulkden
								0.2472,0.2908,0.3308,0.3407,0.3442,0.3418,0.2634,0.2634,#field
								.1203,.1693,.2207,.2332,.2383,.2367,.1565,.1565,			#Wilting
								.8122,.1534,.0344,0,0,0,0,0,							#EvapBarSoil
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran grass
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#tran shrub
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran tree
								.51,.44,.35,.32,.31,.32,.57,.57,						#sand
								.15,.26,.41,.45,.47,.47,.28,.28,						#clay
								1,0,0,0,0,0,0,0,										#imperm
								0.1860,0.3719,0.7438,1.1158,1.4877,2.2315,2.9754,2.9754), nrow=8,ncol=12,dimnames=list(NULL,c("depth_cm","bulkDensity_g/cm^3",
										"fieldCapacity_cm^3/cm^3","WiltingPoint_cm^3/cm^3","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")))))

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
								0.2472,0.2908,0.3308,0.3407,0.3442,0.3418,0.2634,0.2634,#field
								.1203,.1693,.2207,.2332,.2383,.2367,.1565,.1565,			#Wilting
								.8122,.1534,.0344,0,0,0,0,0,							#EvapBarSoil
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran grass
								.1336,.0936,.1762,.1746,.1098,.1787,.1011,.1011,		#tran shrub
								.0333,.0333,.0667,.0667,.0667,.1333,.1333,.1333,		#tran tree
								.51,.44,.35,.32,.31,.32,.57,.57,						#sand
								.15,.26,.41,.45,.47,.47,.28,.28,						#clay
								1,0,0,0,0,0,0,0,										#imperm
								0.1860,0.3719,0.7438,1.1158,1.4877,2.2315,2.9754,2.9754), nrow=8,ncol=12)
			colnames(Layers)<-c("depth_cm","bulkDensity_g/cm^3","fieldCapacity_cm^3/cm^3","WiltingPoint_cm^3/cm^3","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			.Object@Layers<-Layers
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swSoils",
		definition=function(object) {
			Layers<-matrix(data=NA, nrow=8,ncol=12)
			colnames(Layers)<-c("depth_cm","bulkDensity_g/cm^3","fieldCapacity_cm^3/cm^3","WiltingPoint_cm^3/cm^3","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")
			object@Layers<-Layers
			return(object)
		})
setMethod("swWriteLines", signature=c(object="swSoils", file="character"), definition=function(object, file) {
			infilename <- file.path(file)
			infiletext <- character(17+nrow(object@Layers))
			
			infiletext[1] <- "# Soil layer definitions"
			infiletext[2] <- "# Location: "
			infiletext[3] <- "#"
			infiletext[4] <- "# depth = (cm) lower limit of layer; layers must be in order of depth."
			infiletext[5] <- "# bulkd = (g/cm^3) bulk density of soil in this layer."
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
			infiletext[17] <- "# depth bulkd   fieldc   wiltpt  evco  trco_grass  	trco_shrub  trco_tree  	%sand  %clay imperm soiltemp"
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
			infiletext <- sapply(infiletext, function(x) unlist(strsplit(x,'#'))[1],USE.NAMES=FALSE)#get rid of comments
			infiletext <- infiletext[infiletext!=""]#get rid of extra spaces
			if(length(infiletext) < 1) stop(paste(basename(file), ": No rows available to read."))
			#Reset Layers to proper size
			object@Layers <- matrix(data=NA, nrow=length(infiletext),ncol=12,dimnames=list(NULL,c("depth_cm","bulkDensity_g/cm^3","fieldCapacity_cm^3/cm^3","WiltingPoint_cm^3/cm^3","EvapBareSoil_frac","transpGrass_frac","transpShrub_frac","transpTree_frac","sand_frac","clay_frac","impermeability_frac","soilTemp_c")))
			#Fill in data
			for(i in 1:length(infiletext)) {
				object@Layers[i,] = readNumerics(infiletext[i],12)
			}
			#remove NA rows
			if(length(infiletext) > 1) {
				ind<-apply(object@Layers, 1, function(x) all(is.na(x)))
				object@Layers <- object@Layers[!ind,]
				if(any(ind)) warning(paste(basename(file), ": NA rows removed from Data."))
			} else {
				if(all(is.na(x))) stop(paste(basename(file), ": 1 row availiable but NA values."))
			}
			return(object)
		})
