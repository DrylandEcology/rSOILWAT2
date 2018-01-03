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
# Author: Ryan J. Murphy (2013)
# Updated for v31 Forbs and Bare Ground
###############################################################################


#' @export
setClass("swProd", slots = c(Composition = "numeric", Albedo = "numeric",
  Cover_stcr = "numeric", CanopyHeight = "matrix",
  VegetationInterceptionParameters = "matrix",
  LitterInterceptionParameters = "matrix",
  EsTpartitioning_param = "numeric", Es_param_limit = "numeric", Shade = "matrix",
  HydraulicRedistribution_use = "logical", HydraulicRedistribution = "matrix",
  CriticalSoilWaterPotential = "numeric", CO2Coefficients = "matrix",
  MonthlyProductionValues_grass = "matrix", MonthlyProductionValues_shrub = "matrix",
  MonthlyProductionValues_tree = "matrix", MonthlyProductionValues_forb = "matrix"))

swProd_validity<-function(object){
	if(length(object@Composition)!=5)
		return("@Composition needs length of 5.")
	if(length(object@Albedo)!=5)
		return("@Albedo needs length of 5.")
	if(length(object@Cover_stcr)!=4)
		return("@Cover_stcr needs length of 4.")
	if(dim(object@CanopyHeight)[1] != 5 | dim(object@CanopyHeight)[2] != 4)
		return("@CanopyHeight needs dim of c(5,4).")
	if(dim(object@VegetationInterceptionParameters)[1] != 4 | dim(object@VegetationInterceptionParameters)[2] != 4)
		return("@VegetationInterceptionParameters dim of c(4,4) needed.")
	if(dim(object@LitterInterceptionParameters)[1] != 4 | dim(object@LitterInterceptionParameters)[2] != 4)
		return("@LitterInterceptionParameters dim of c(4,4) needed.")
	if(length(object@EsTpartitioning_param)!=4)
		return("@EsTpartitioning_param needs length of 4.")
	if(length(object@Es_param_limit)!=4)
		return("@Es_param_limit needs length of 4.")
	if(length(object@HydraulicRedistribution_use)!=4)
		return("@HydraulicRedistribution_use needs length of 4.")
	if(dim(object@Shade)[1] != 6 | dim(object@Shade)[2] != 4)
		return("@Shade dim of c(6,4) needed.")
	if(dim(object@HydraulicRedistribution)[1] != 3 | dim(object@HydraulicRedistribution)[2] != 4)
		return("@HydraulicRedistribution dim of c(3,4) needed.")
	if(length(object@CriticalSoilWaterPotential)!=4)
		return("@CriticalSoilWaterPotential needs length of 4.")
	if(dim(object@MonthlyProductionValues_grass)[1] != 12 | dim(object@MonthlyProductionValues_grass)[2] != 4)
		return("@MonthlyProductionValues_grass dim of c(12,4) needed.")
	if(dim(object@MonthlyProductionValues_shrub)[1] != 12 | dim(object@MonthlyProductionValues_shrub)[2] != 4)
		return("@MonthlyProductionValues_shrub dim of c(12,4) needed.")
	if(dim(object@MonthlyProductionValues_tree)[1] != 12 | dim(object@MonthlyProductionValues_tree)[2] != 4)
		return("@MonthlyProductionValues_tree dim of c(12,4) needed.")
	if(dim(object@MonthlyProductionValues_forb)[1] != 12 | dim(object@MonthlyProductionValues_forb)[2] != 4)
		return("@MonthlyProductionValues_forb dim of c(12,4) needed.")
  if(dim(object@CO2Coefficients)[1] != 4 | dim(object@CO2Coefficients)[2] != 4)
    return("@CO2Coefficients dim of c(4,4) needed.")
}
setValidity("swProd",swProd_validity)
setMethod(f="initialize",signature="swProd",definition=function(.Object,Composition=c(0,0,0,1,0),Albedo=c(0.167,0.143,0.106,0.167,0.15),Cover_stcr=c(3.0,2.22,5.0,3.0),CanopyHeight=NULL,VegetationInterceptionParameters=NULL,LitterInterceptionParameters=NULL, EsTpartitioning_param=c(1,1,0.41,1),Es_param_limit=c(999,999,2099,999),Shade=NULL,
				HydraulicRedistribution_use=c(TRUE,TRUE,TRUE,TRUE),HydraulicRedistribution=NULL,CriticalSoilWaterPotential=c(-3.5,-3.9,-2.0,-2.0),CO2Coefficients=NULL,MonthlyProductionValues_grass=NULL,MonthlyProductionValues_shrub=NULL,MonthlyProductionValues_tree=NULL,MonthlyProductionValues_forb=NULL) {

			if(is.null(CanopyHeight))
				CanopyHeight<-matrix(data=c(300,29.5,85,0.002,0, 0,5,100,0.003,50, 0,5,3000,0.00008,1200, 300,29.5,85,0.002,0),nrow=5,ncol=4,dimnames=list(c("xinflec","yinflec","range","slope","height_cm"),c("Grasses","Shrubs","Trees","Forbs")))
			if(is.null(VegetationInterceptionParameters))
				VegetationInterceptionParameters=matrix(data=c(0.0182,0.0065,0.0019,0.0054, 0.,0.0026,0.,0.0033, 0.00461,0.01405,0.0383,0.0337, 0.0182,0.0065,0.0019,0.0054),nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs")))
			if(is.null(LitterInterceptionParameters))
				LitterInterceptionParameters=matrix(data=c(0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002),nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs")))
			if(is.null(Shade))
				Shade<-matrix(data=c(0.3,150,300,12,34,0.002,  0.3,150,300,12,34,0.002,  0.3,150,0,0,2,0.0002, 0.3,150,300,12,34,0.002),nrow=6,ncol=4,dimnames=list(c("scale","maxDeadBiomass","tanfuncXinflec","yinflec","range","slope"),c("Grasses","Shrubs","Trees","Forbs")))
			if(is.null(HydraulicRedistribution))
				HydraulicRedistribution<-matrix(data=c(-0.2328,10,3.22, -0.2328,10,3.22, -0.2328,10,3.22, -0.2328,10,3.22),nrow=3,ncol=4,dimnames=list(c("MaxCondRoot","SoilWaterPotential50","ShapeCond"),c("Grasses","Shrubs","Trees","Forbs")))
			if(is.null(CO2Coefficients))
			  CO2Coefficients=matrix(data=c(0.127, 0.127, 0.127, 0.127, 0.3501, 0.3501, 0.3501, 0.3501, 22.464, 22.464, 22.464, 22.464, -0.531, -0.531, -0.531, -0.531), nrow=4, ncol=4, dimnames=list(c("BioCoeff1", "BioCoeff2", "WUECoeff1", "WUECoeff2"), c("Grasses", "Shrubs", "Trees", "Forbs")))
			if(is.null(MonthlyProductionValues_grass))
				MonthlyProductionValues_grass<-matrix(data=c(75,80,85,90,50,50,50,55,60,65,70,75,150,150,150,170,190,220,250,220,190,180,170,160,0.0,0.0,0.10,0.20,0.40,0.60,0.40,0.60,0.40,0.20,0.10,0.0,300,300,300,300,300,300,300,300,300,300,300,300),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			if(is.null(MonthlyProductionValues_shrub))
				MonthlyProductionValues_shrub<-matrix(data=c(85.40,88.20,95.30,100.50,166.40,186.00,177.10,212.20,157.40,124.90,110.40,104.30,210.00,212.00,228.00,272.00,400.00,404.00,381.00,352.00,286.00,235.00,218.00,214.00,0.06,0.08,0.20,0.33,0.57,0.55,0.50,0.46,0.32,0.15,0.08,0.06,372,372,372,372,372,372,372,372,372,372,372,372),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			if(is.null(MonthlyProductionValues_tree))
				MonthlyProductionValues_tree<-matrix(data=c(2000.000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,500,500,500,500,500,500,500,500,500,500,500,500),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			if(is.null(MonthlyProductionValues_forb))
				MonthlyProductionValues_forb<-matrix(data=c(85.40,88.20,95.30,100.50,166.40,186.00,177.10,212.20,157.40,124.90,110.40,104.30,210.00,212.00,228.00,272.00,400.00,404.00,381.00,352.00,286.00,235.00,218.00,214.00,0.06,0.08,0.20,0.33,0.57,0.55,0.50,0.46,0.32,0.15,0.08,0.06,372,372,372,372,372,372,372,372,372,372,372,372),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))

			names(Composition) <- c("Grasses", "Shrubs", "Trees", "Forbs", "BareGround");
			names(Albedo) <- c("Grasses", "Shrubs", "Trees","Forbs","BareGround");
			names(Cover_stcr) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(EsTpartitioning_param) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(Es_param_limit) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(HydraulicRedistribution_use) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(CriticalSoilWaterPotential) <- c("Grasses", "Shrubs", "Trees","Forbs");

			.Object@Composition=Composition
			.Object@Albedo=Albedo
			.Object@Cover_stcr=Cover_stcr
			.Object@CanopyHeight=CanopyHeight
			.Object@VegetationInterceptionParameters=VegetationInterceptionParameters
			.Object@LitterInterceptionParameters=LitterInterceptionParameters
			.Object@EsTpartitioning_param=EsTpartitioning_param
			.Object@Es_param_limit=Es_param_limit
			.Object@Shade=Shade
			.Object@HydraulicRedistribution_use=HydraulicRedistribution_use
			.Object@HydraulicRedistribution=HydraulicRedistribution
			.Object@CriticalSoilWaterPotential=CriticalSoilWaterPotential
			.Object@MonthlyProductionValues_grass=MonthlyProductionValues_grass
			.Object@MonthlyProductionValues_shrub=MonthlyProductionValues_shrub
			.Object@MonthlyProductionValues_tree=MonthlyProductionValues_tree
			.Object@CO2Coefficients

			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swProd",
		definition=function(object) {
			CanopyHeight<-matrix(data=NA,nrow=5,ncol=4,dimnames=list(c("xinflec","yinflec","range","slope","height_cm"),c("Grasses","Shrubs","Trees","Forbs")))
			VegetationInterceptionParameters<-matrix(data=NA,nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs")))
			LitterInterceptionParameters<-matrix(data=NA,nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs")))
			Shade<-matrix(data=NA,nrow=6,ncol=4,dimnames=list(c("scale","maxDeadBiomass","tanfuncXinflec","yinflec","range","slope"),c("Grasses","Shrubs","Trees","Forbs")))
			HydraulicRedistribution<-matrix(data=NA,nrow=3,ncol=4,dimnames=list(c("MaxCondRoot","SoilWaterPotential50","ShapeCond"),c("Grasses","Shrubs","Trees","Forbs")))
			CO2Coefficients<-matrix(data=NA,nrow=4,ncol=4,dimnames=list(c("BioCoeff1", "BioCoeff2", "WUECoeff1", "WUECoeff2"), c("Grasses", "Shrubs", "Trees", "Forbs")))
			MonthlyProductionValues_grass<-matrix(data=NA,nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			MonthlyProductionValues_shrub<-matrix(data=NA,nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			MonthlyProductionValues_tree<-matrix(data=NA,nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			MonthlyProductionValues_forb<-matrix(data=NA,nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))
			Composition=numeric(5)
			Albedo=numeric(5)
			Cover_stcr=numeric(4)
			EsTpartitioning_param=numeric(4)
			Es_param_limit=numeric(4)
			HydraulicRedistribution_use=logical(4)
			CriticalSoilWaterPotential=numeric(4)
			names(Composition) <- c("Grasses", "Shrubs", "Trees", "Forbs", "BareGround");
			names(Albedo) <- c("Grasses", "Shrubs", "Trees","Forbs","BareGround");
			names(Cover_stcr) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(EsTpartitioning_param) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(Es_param_limit) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(HydraulicRedistribution_use) <- c("Grasses", "Shrubs", "Trees","Forbs");
			names(CriticalSoilWaterPotential) <- c("Grasses", "Shrubs", "Trees","Forbs");
			object@Composition=Composition
			object@Albedo=Albedo
			object@Cover_stcr=Cover_stcr
			object@CanopyHeight=CanopyHeight
			object@VegetationInterceptionParameters=VegetationInterceptionParameters
			object@LitterInterceptionParameters=LitterInterceptionParameters
			object@EsTpartitioning_param=EsTpartitioning_param
			object@Es_param_limit=Es_param_limit
			object@Shade=Shade
			object@HydraulicRedistribution_use=HydraulicRedistribution_use
			object@HydraulicRedistribution=HydraulicRedistribution
			object@CriticalSoilWaterPotential=CriticalSoilWaterPotential
			object@CO2Coefficients
			object@MonthlyProductionValues_grass=MonthlyProductionValues_grass
			object@MonthlyProductionValues_shrub=MonthlyProductionValues_shrub
			object@MonthlyProductionValues_tree=MonthlyProductionValues_tree
			object@MonthlyProductionValues_forb=MonthlyProductionValues_forb
			return(object)
		})
setMethod("swProd_Composition", "swProd", function(object) {return(object@Composition)})
setMethod("swProd_Albedo", "swProd", function(object) {return(object@Albedo)})
setMethod("swProd_Cover_stcr", "swProd", function(object) {return(object@Cover_stcr)})
setMethod("swProd_CanopyHeight", "swProd", function(object) {return(object@CanopyHeight)})
setMethod("swProd_VegInterParam", "swProd", function(object) {return(object@VegetationInterceptionParameters)})
setMethod("swProd_LitterInterParam", "swProd", function(object) {return(object@LitterInterceptionParameters)})
setMethod("swProd_EsTpartitioning_param", "swProd", function(object) {return(object@EsTpartitioning_param)})
setMethod("swProd_Es_param_limit", "swProd", function(object) {return(object@Es_param_limit)})
setMethod("swProd_Shade", "swProd", function(object) {return(object@Shade)})
setMethod("swProd_HydrRedstro_use", "swProd", function(object) {return(object@HydraulicRedistribution_use)})
setMethod("swProd_HydrRedstro", "swProd", function(object) {return(object@HydraulicRedistribution)})
setMethod("swProd_CritSoilWaterPotential", "swProd", function(object) {return(object@CriticalSoilWaterPotential)})
setMethod("swProd_CO2Coefficients", "swProd", function(object) {return(object@CO2Coefficients)})
setMethod("swProd_MonProd_grass", "swProd", function(object) {return(object@MonthlyProductionValues_grass)})
setMethod("swProd_MonProd_shrub", "swProd", function(object) {return(object@MonthlyProductionValues_shrub)})
setMethod("swProd_MonProd_tree", "swProd", function(object) {return(object@MonthlyProductionValues_tree)})
setMethod("swProd_MonProd_forb", "swProd", function(object) {return(object@MonthlyProductionValues_forb)})

setReplaceMethod(f="swProd_Composition", signature="swProd", definition=function(object,value) {object@Composition <- value; return(object)})
setReplaceMethod(f="swProd_Albedo", signature="swProd", definition=function(object,value) {object@Albedo <- value; return(object)})
setReplaceMethod(f="swProd_Cover_stcr", signature="swProd", definition=function(object,value) {object@Cover_stcr <- value; return(object)})
setReplaceMethod(f="swProd_CanopyHeight", signature="swProd", definition=function(object,value) {object@CanopyHeight <- value; return(object)})
setReplaceMethod(f="swProd_VegInterParam", signature="swProd", definition=function(object,value) {object@VegetationInterceptionParameters <- value; return(object)})
setReplaceMethod(f="swProd_LitterInterParam", signature="swProd", definition=function(object,value) {object@LitterInterceptionParameters <- value; return(object)})
setReplaceMethod(f="swProd_EsTpartitioning_param", signature="swProd", definition=function(object,value) {object@EsTpartitioning_param <- value; return(object)})
setReplaceMethod(f="swProd_Es_param_limit", signature="swProd", definition=function(object,value) {object@Es_param_limit <- value; return(object)})
setReplaceMethod(f="swProd_Shade", signature="swProd", definition=function(object,value) {object@Shade <- value; return(object)})
setReplaceMethod(f="swProd_HydrRedstro_use", signature="swProd", definition=function(object,value) {object@HydraulicRedistribution_use <- value; return(object)})
setReplaceMethod(f="swProd_HydrRedstro", signature="swProd", definition=function(object,value) {object@HydraulicRedistribution <- value; return(object)})
setReplaceMethod(f="swProd_CritSoilWaterPotential", signature="swProd", definition=function(object,value) {object@CriticalSoilWaterPotential <- value; return(object)})
setReplaceMethod(f="swProd_CO2Coefficients", signature="swProd", definition=function(object, value) {object@CO2Coefficients <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_grass", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_grass <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_shrub", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_shrub <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_tree", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_tree <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_forb", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_forb <- value; return(object)})


setMethod("swReadLines", signature=c(object="swProd",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			object@Composition = readNumerics(infiletext[6],5)
			object@Albedo = readNumerics(infiletext[11],5)
			object@Cover_stcr = readNumerics(infiletext[16],4)
			object@CanopyHeight[1,] = readNumerics(infiletext[21],4)
			object@CanopyHeight[2,] = readNumerics(infiletext[22],4)
			object@CanopyHeight[3,] = readNumerics(infiletext[23],4)
			object@CanopyHeight[4,] = readNumerics(infiletext[24],4)
			object@CanopyHeight[5,] = readNumerics(infiletext[25],4)
			object@VegetationInterceptionParameters[1,] = readNumerics(infiletext[30],4)
			object@VegetationInterceptionParameters[2,] = readNumerics(infiletext[31],4)
			object@VegetationInterceptionParameters[3,] = readNumerics(infiletext[32],4)
			object@VegetationInterceptionParameters[4,] = readNumerics(infiletext[33],4)
			object@LitterInterceptionParameters[1,] = readNumerics(infiletext[38],4)
			object@LitterInterceptionParameters[2,] = readNumerics(infiletext[39],4)
			object@LitterInterceptionParameters[3,] = readNumerics(infiletext[40],4)
			object@LitterInterceptionParameters[4,] = readNumerics(infiletext[41],4)
			object@EsTpartitioning_param = readNumerics(infiletext[46],4)
			object@Es_param_limit = readNumerics(infiletext[51],4)
			object@Shade[1,] = readNumerics(infiletext[56],4)
			object@Shade[2,] = readNumerics(infiletext[57],4)
			object@Shade[3,] = readNumerics(infiletext[58],4)
			object@Shade[4,] = readNumerics(infiletext[59],4)
			object@Shade[5,] = readNumerics(infiletext[60],4)
			object@Shade[6,] = readNumerics(infiletext[61],4)
			object@HydraulicRedistribution_use = as.logical(as.integer(readNumerics(infiletext[66],4)))
			object@HydraulicRedistribution[1,] = readNumerics(infiletext[67],4)
			object@HydraulicRedistribution[2,] = readNumerics(infiletext[68],4)
			object@HydraulicRedistribution[3,] = readNumerics(infiletext[69],4)
			object@CriticalSoilWaterPotential = readNumerics(infiletext[74],4)
			for(i in 1:4)  object@CO2Coefficients[i, ] = readNumerics(infiletext[79 + i], 4)
			for(i in 1:12) object@MonthlyProductionValues_grass[i,] = readNumerics(infiletext[94+i],4)
			for(i in 1:12) object@MonthlyProductionValues_shrub[i,] = readNumerics(infiletext[109+i],4)
			for(i in 1:12) object@MonthlyProductionValues_tree[i,] = readNumerics(infiletext[124+i],4)
			for(i in 1:12) object@MonthlyProductionValues_forb[i,] = readNumerics(infiletext[139+i],4)
			return(object)
		})
