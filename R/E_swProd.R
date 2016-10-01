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
# Author: Ryan J. Murphy (2013)
# Updated for v31 Forbs and Bare Ground
###############################################################################


print("swProd")
swProd <- setClass("swProd",representation(Composition="numeric",Albedo="numeric",Cover_stcr="numeric",CanopyHeight="matrix",VegetationInterceptionParameters="matrix",LitterInterceptionParameters="matrix", EsTpartitioning_param="numeric",Es_param_limit="numeric",Shade="matrix",HydraulicRedistribution_use="logical",HydraulicRedistribution="matrix",CriticalSoilWaterPotential="numeric",MonthlyProductionValues_grass="matrix",MonthlyProductionValues_shrub="matrix",MonthlyProductionValues_tree="matrix", MonthlyProductionValues_forb="matrix"),
		prototype=prototype(Composition=c(0,0,0,1,0),Albedo=c(0.167,0.143,0.106,0.167,0.15),Cover_stcr=c(3.0,2.22,5.0,3.0),
				CanopyHeight=matrix(data=c(300,29.5,85,0.002,0, 0,5,100,0.003,50, 0,5,3000,0.00008,1200, 300,29.5,85,0.002,0),nrow=5,ncol=4,dimnames=list(c("xinflec","yinflec","range","slope","height_cm"),c("Grasses","Shrubs","Trees","Forbs"))),
				VegetationInterceptionParameters=matrix(data=c(0.0182,0.0065,0.0019,0.0054, 0.,0.0026,0.,0.0033, 0.00461,0.01405,0.0383,0.0337, 0.0182,0.0065,0.0019,0.0054),nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs"))),
				LitterInterceptionParameters=matrix(data=c(0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002, 0.0151,0.00005,0.0116,0.00002),nrow=4,ncol=4,dimnames=list(c("a","b","c","d"),c("Grasses","Shrubs","Trees","Forbs"))),
				EsTpartitioning_param=c(1,1,0.41,1),Es_param_limit=c(999,999,2099,999),
				Shade=matrix(data=c(0.3,150,300,12,34,0.002,  0.3,150,300,12,34,0.002,  0.3,150,0,0,2,0.0002, .3,150,300,12,34,0.002),nrow=6,ncol=4,dimnames=list(c("scale","maxDeadBiomass","tanfuncXinflec","yinflec","range","slope"),c("Grasses","Shrubs","Trees","Forbs"))),
				HydraulicRedistribution_use=c(TRUE,TRUE,TRUE,TRUE),
				HydraulicRedistribution=matrix(data=c(-0.2328,10,3.22, -0.2328,10,3.22, -0.2328,10,3.22, -0.2328,10,3.22),nrow=3,ncol=4,dimnames=list(c("MaxCondRoot","SoilWaterPotential50","ShapeCond"),c("Grasses","Shrubs","Trees","Forbs"))),
				CriticalSoilWaterPotential=c(-3.5,-3.9,-2.0,-2.0),
				MonthlyProductionValues_grass=matrix(data=c(75,80,85,90,50,50,50,55,60,65,70,75,150,150,150,170,190,220,250,220,190,180,170,160,0.0,0.0,0.10,0.20,0.40,0.60,0.40,0.60,0.40,0.20,0.10,0.0,300,300,300,300,300,300,300,300,300,300,300,300),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv"))),
				MonthlyProductionValues_shrub=matrix(data=c(85.40,88.20,95.30,100.50,166.40,186.00,177.10,212.20,157.40,124.90,110.40,104.30,210.00,212.00,228.00,272.00,400.00,404.00,381.00,352.00,286.00,235.00,218.00,214.00,0.06,0.08,0.20,0.33,0.57,0.55,0.50,0.46,0.32,0.15,0.08,0.06,372,372,372,372,372,372,372,372,372,372,372,372),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv"))),
				MonthlyProductionValues_tree=matrix(data=c(2000.000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,15000,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,0.083,500,500,500,500,500,500,500,500,500,500,500,500),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv"))),
				MonthlyProductionValues_forb=matrix(data=c(85.40,88.20,95.30,100.50,166.40,186.00,177.10,212.20,157.40,124.90,110.40,104.30,210.00,212.00,228.00,272.00,400.00,404.00,381.00,352.00,286.00,235.00,218.00,214.00,0.06,0.08,0.20,0.33,0.57,0.55,0.50,0.46,0.32,0.15,0.08,0.06,372,372,372,372,372,372,372,372,372,372,372,372),nrow=12,ncol=4,dimnames=list(c("January","February","March","April","May","June","July","August","September","October","November","December"),c("Litter","Biomass","live_pct","LAI_conv")))))

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
}
setValidity("swProd",swProd_validity)
setMethod(f="initialize",signature="swProd",definition=function(.Object,Composition=c(0,0,0,1,0),Albedo=c(0.167,0.143,0.106,0.167,0.15),Cover_stcr=c(3.0,2.22,5.0,3.0),CanopyHeight=NULL,VegetationInterceptionParameters=NULL,LitterInterceptionParameters=NULL, EsTpartitioning_param=c(1,1,0.41,1),Es_param_limit=c(999,999,2099,999),Shade=NULL,
				HydraulicRedistribution_use=c(TRUE,TRUE,TRUE,TRUE),HydraulicRedistribution=NULL,CriticalSoilWaterPotential=c(-3.5,-3.9,-2.0,-2.0),MonthlyProductionValues_grass=NULL,MonthlyProductionValues_shrub=NULL,MonthlyProductionValues_tree=NULL,MonthlyProductionValues_forb=NULL) {

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
			object@MonthlyProductionValues_grass=MonthlyProductionValues_grass
			object@MonthlyProductionValues_shrub=MonthlyProductionValues_shrub
			object@MonthlyProductionValues_tree=MonthlyProductionValues_tree
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
setReplaceMethod(f="swProd_MonProd_grass", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_grass <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_shrub", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_shrub <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_tree", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_tree <- value; return(object)})
setReplaceMethod(f="swProd_MonProd_forb", signature="swProd", definition=function(object,value) {object@MonthlyProductionValues_forb <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swProd", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(142)

			infiletext[1] <- "# Plant production data file for SOILWAT"
			infiletext[2] <- "# Location: "

			infiletext[4] <- "# ---- Composition of vegetation type components (0-1; must add up to 1)"
			infiletext[5] <- "# Grasses\tShrubs\tTrees\tForbs\tBare Ground"
			infiletext[6] <- paste(format(object@Composition[1]),"\t",format(object@Composition[2]),"\t",format(object@Composition[3]),"\t",format(object@Composition[4]),"\t",format(object@Composition[5]),"\t",sep="")

			infiletext[9] <- "# ---- Albedo"
			infiletext[10] <- "# Grasses\tShrubs\tTrees\tForbs\tBare Ground"
			infiletext[11] <- paste(format(object@Albedo[1]),"\t",format(object@Albedo[2]),"\t",format(object@Albedo[3]),"\t",format(object@Albedo[4]),"\t",format(object@Albedo[5]),"\t# albedo:	(Houldcroft et al. 2009) MODIS snowfree 'grassland', 'open shrub', 'evergreen needle forest' with MODIS albedo aggregated over pure IGBP cells where NDVI is greater than the 98th percentile NDVI",sep="")

			infiletext[14] <- "# ---- % Cover: divide standing LAI by this to get % cover"
			infiletext[15] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[16] <- paste(format(object@Cover_stcr[1]),"\t",format(object@Cover_stcr[2]),"\t",format(object@Cover_stcr[3]),"\t",format(object@Cover_stcr[4]),sep="")

			infiletext[19] <- "# -- Canopy height (cm) parameters either constant through season or as tanfunc with respect to biomass (g/m^2)"
			infiletext[20] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[21] <- paste(format(object@CanopyHeight[1,1]),"\t",format(object@CanopyHeight[1,2]),"\t",format(object@CanopyHeight[1,3]),"\t",format(object@CanopyHeight[1,4]),"\t# xinflec",sep="")
			infiletext[22] <- paste(format(object@CanopyHeight[2,1]),"\t",format(object@CanopyHeight[2,2]),"\t",format(object@CanopyHeight[2,3]),"\t",format(object@CanopyHeight[2,4]),"\t# yinflec",sep="")
			infiletext[23] <- paste(format(object@CanopyHeight[3,1]),"\t",format(object@CanopyHeight[3,2]),"\t",format(object@CanopyHeight[3,3]),"\t",format(object@CanopyHeight[3,4]),"\t# range",sep="")
			infiletext[24] <- paste(format(object@CanopyHeight[4,1]),"\t",format(object@CanopyHeight[4,2]),"\t",format(object@CanopyHeight[4,3]),"\t",format(object@CanopyHeight[4,4]),"\t# slope",sep="")
			infiletext[25] <- paste(format(object@CanopyHeight[5,1]),"\t",format(object@CanopyHeight[5,2]),"\t",format(object@CanopyHeight[5,3]),"\t",format(object@CanopyHeight[5,4]),"\t# if > 0 then constant canopy height (cm)",sep="")

			infiletext[28] <- "# --- Vegetation interception parameters for equation: intercepted rain = (a + b*veg) + (c+d*veg) * ppt; Grasses+Shrubs: veg=vegcov, Trees: veg=LAI"
			infiletext[29] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[30] <- paste(format(object@VegetationInterceptionParameters[1,1]),"\t",format(object@VegetationInterceptionParameters[1,2]),"\t",format(object@VegetationInterceptionParameters[1,3]),"\t",format(object@VegetationInterceptionParameters[1,4]),"\t# a",sep="")
			infiletext[31] <- paste(format(object@VegetationInterceptionParameters[2,1]),"\t",format(object@VegetationInterceptionParameters[2,2]),"\t",format(object@VegetationInterceptionParameters[2,3]),"\t",format(object@VegetationInterceptionParameters[2,4]),"\t# b",sep="")
			infiletext[32] <- paste(format(object@VegetationInterceptionParameters[3,1]),"\t",format(object@VegetationInterceptionParameters[3,2]),"\t",format(object@VegetationInterceptionParameters[3,3]),"\t",format(object@VegetationInterceptionParameters[3,4]),"\t# c",sep="")
			infiletext[33] <- paste(format(object@VegetationInterceptionParameters[4,1]),"\t",format(object@VegetationInterceptionParameters[4,2]),"\t",format(object@VegetationInterceptionParameters[4,3]),"\t",format(object@VegetationInterceptionParameters[4,4]),"\t# d",sep="")

			infiletext[36] <- "# --- Litter interception parameters for equation: intercepted rain = (a + b*litter) + (c+d*litter) * ppt"
			infiletext[37] <- "# Grass-Litter\tShrub-Litter\tTree-Litter\tForbs-Litter"
			infiletext[38] <- paste(format(object@LitterInterceptionParameters[1,1]),"\t",format(object@LitterInterceptionParameters[1,2]),"\t",format(object@LitterInterceptionParameters[1,3]),"\t",format(object@LitterInterceptionParameters[1,4]),"\t# a",sep="")
			infiletext[39] <- paste(format(object@LitterInterceptionParameters[2,1]),"\t",format(object@LitterInterceptionParameters[2,2]),"\t",format(object@LitterInterceptionParameters[2,3]),"\t",format(object@LitterInterceptionParameters[2,4]),"\t# b",sep="")
			infiletext[40] <- paste(format(object@LitterInterceptionParameters[3,1]),"\t",format(object@LitterInterceptionParameters[3,2]),"\t",format(object@LitterInterceptionParameters[3,3]),"\t",format(object@LitterInterceptionParameters[3,4]),"\t# c",sep="")
			infiletext[41] <- paste(format(object@LitterInterceptionParameters[4,1]),"\t",format(object@LitterInterceptionParameters[4,2]),"\t",format(object@LitterInterceptionParameters[4,3]),"\t",format(object@LitterInterceptionParameters[4,4]),"\t# d",sep="")

			infiletext[44] <- "# ---- Parameter for partitioning of bare-soil evaporation and transpiration as in Es = exp(-param*LAI)"
			infiletext[45] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[46] <- paste(format(object@EsTpartitioning_param[1]),"\t",format(object@EsTpartitioning_param[2]),"\t",format(object@EsTpartitioning_param[3]),"\t",format(object@EsTpartitioning_param[4]),"\t# Trees: According to a regression based on a review by Daikoku, K., S. Hattori, A. Deguchi, Y. Aoki, M. Miyashita, K. Matsumoto, J. Akiyama, S. Iida, T. Toba, Y. Fujita, and T. Ohta. 2008. Influence of evaporation from the forest floor on evapotranspiration from the dry canopy. Hydrological Processes 22:4083-4096.",sep="")

			infiletext[49] <- "# ---- Parameter for scaling and limiting bare soil evaporation rate: if totagb (g/m2) > param then no bare-soil evaporation"
			infiletext[50] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[51] <- paste(format(object@Es_param_limit[1]),"\t",format(object@Es_param_limit[2]),"\t",format(object@Es_param_limit[3]),"\t",format(object@Es_param_limit[4]),"\t#",sep="")

			infiletext[54] <- "# --- Shade effects on transpiration based on live and dead biomass"
			infiletext[55] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[56] <- paste(format(object@Shade[1,1]),"\t",format(object@Shade[1,2]),"\t",format(object@Shade[1,3]),"\t",format(object@Shade[1,4]),"\t# shade scale",sep="")
			infiletext[57] <- paste(format(object@Shade[2,1]),"\t",format(object@Shade[2,2]),"\t",format(object@Shade[2,3]),"\t",format(object@Shade[2,4]),"\t# shade maximal dead biomass",sep="")
			infiletext[58] <- paste(format(object@Shade[3,1]),"\t",format(object@Shade[3,2]),"\t",format(object@Shade[3,3]),"\t",format(object@Shade[3,4]),"\t# tanfunc: xinflec",sep="")
			infiletext[59] <- paste(format(object@Shade[4,1]),"\t",format(object@Shade[4,2]),"\t",format(object@Shade[4,3]),"\t",format(object@Shade[4,4]),"\t# yinflec",sep="")
			infiletext[60] <- paste(format(object@Shade[5,1]),"\t",format(object@Shade[5,2]),"\t",format(object@Shade[5,3]),"\t",format(object@Shade[5,4]),"\t# range",sep="")
			infiletext[61] <- paste(format(object@Shade[6,1]),"\t",format(object@Shade[6,2]),"\t",format(object@Shade[6,3]),"\t",format(object@Shade[6,4]),"\t# slope",sep="")

			infiletext[64] <- "# ---- Hydraulic redistribution: Ryel, Ryel R, Caldwell, Caldwell M, Yoder, Yoder C, Or, Or D, Leffler, Leffler A. 2002. Hydraulic redistribution in a stand of Artemisia tridentata: evaluation of benefits to transpiration assessed with a simulation model. Oecologia 130: 173-184."
			infiletext[65] <- "# Grass-Litter\tShrub-Litter\tTree-Litter\tForbs"
			infiletext[66] <- paste(format(as.integer(object@HydraulicRedistribution_use[1])),"\t",format(as.integer(object@HydraulicRedistribution_use[2])),"\t",format(as.integer(object@HydraulicRedistribution_use[3])),"\t",format(as.integer(object@HydraulicRedistribution_use[4])),"\t# flag to turn on/off (1/0) hydraulic redistribution",sep="")
			infiletext[67] <- paste(format(object@HydraulicRedistribution[1,1]),"\t",format(object@HydraulicRedistribution[1,2]),"\t",format(object@HydraulicRedistribution[1,3]),"\t",format(object@HydraulicRedistribution[1,4]),"\t# maxCondroot - maximum radial soil-root conductance of the entire active root system for water (cm/-bar/day) = 0.097 cm/MPa/h",sep="")
			infiletext[68] <- paste(format(object@HydraulicRedistribution[2,1]),"\t",format(object@HydraulicRedistribution[2,2]),"\t",format(object@HydraulicRedistribution[2,3]),"\t",format(object@HydraulicRedistribution[2,4]),"\t# swp50 - soil water potential (-bar) where conductance is reduced by 50% = -1. MPa",sep="")
			infiletext[69] <- paste(format(object@HydraulicRedistribution[3,1]),"\t",format(object@HydraulicRedistribution[3,2]),"\t",format(object@HydraulicRedistribution[3,3]),"\t",format(object@HydraulicRedistribution[3,4]),"\t# shapeCond - shaping parameter for the empirical relationship from van Genuchten to model relative soil-root conductance for water",sep="")

			infiletext[72] <- "# ---- Critical soil water potential (MPa), i.e., when transpiration rates cannot sustained anymore, for instance, for many crop species -1.5 MPa is assumed and called wilting point"
			infiletext[73] <- "# Grasses\tShrubs\tTrees\tForbs"
			infiletext[74] <- paste(format(object@CriticalSoilWaterPotential[1]),"\t",format(object@CriticalSoilWaterPotential[2]),"\t",format(object@CriticalSoilWaterPotential[3]),"\t",format(object@CriticalSoilWaterPotential[4]),sep="")

			infiletext[77] <- "# Grasslands component:"
			infiletext[78] <- "# -------------- Monthly production values ------------"
			infiletext[79] <- "# Litter   - dead leafy material on the ground (g/m^2 )."
			infiletext[80] <- "# Biomass  - living and dead/woody aboveground standing biomass (g/m^2)."
			infiletext[81] <- "# %Live    - proportion of Biomass that is actually living (0-1.0)."
			infiletext[82] <- "# LAI_conv - monthly amount of biomass needed to produce LAI=1.0 (g/m^2)."
			infiletext[83] <- "# There should be 12 rows, one for each month, starting with January."
			infiletext[84] <- "#"
			infiletext[85] <- "#Litter\tBiomass\t%Live\tLAI_conv"
			infiletext[86] <- paste(format(object@MonthlyProductionValues_grass[1,1]),"\t",format(object@MonthlyProductionValues_grass[1,2]),"\t",format(object@MonthlyProductionValues_grass[1,3]),"\t",format(object@MonthlyProductionValues_grass[1,4]),"\t# January",sep="")
			infiletext[87] <- paste(format(object@MonthlyProductionValues_grass[2,1]),"\t",format(object@MonthlyProductionValues_grass[2,2]),"\t",format(object@MonthlyProductionValues_grass[2,3]),"\t",format(object@MonthlyProductionValues_grass[2,4]),"\t# February",sep="")
			infiletext[88] <- paste(format(object@MonthlyProductionValues_grass[3,1]),"\t",format(object@MonthlyProductionValues_grass[3,2]),"\t",format(object@MonthlyProductionValues_grass[3,3]),"\t",format(object@MonthlyProductionValues_grass[3,4]),"\t# March",sep="")
			infiletext[89] <- paste(format(object@MonthlyProductionValues_grass[4,1]),"\t",format(object@MonthlyProductionValues_grass[4,2]),"\t",format(object@MonthlyProductionValues_grass[4,3]),"\t",format(object@MonthlyProductionValues_grass[4,4]),"\t# April",sep="")
			infiletext[90] <- paste(format(object@MonthlyProductionValues_grass[5,1]),"\t",format(object@MonthlyProductionValues_grass[5,2]),"\t",format(object@MonthlyProductionValues_grass[5,3]),"\t",format(object@MonthlyProductionValues_grass[5,4]),"\t# May",sep="")
			infiletext[91] <- paste(format(object@MonthlyProductionValues_grass[6,1]),"\t",format(object@MonthlyProductionValues_grass[6,2]),"\t",format(object@MonthlyProductionValues_grass[6,3]),"\t",format(object@MonthlyProductionValues_grass[6,4]),"\t# June",sep="")
			infiletext[92] <- paste(format(object@MonthlyProductionValues_grass[7,1]),"\t",format(object@MonthlyProductionValues_grass[7,2]),"\t",format(object@MonthlyProductionValues_grass[7,3]),"\t",format(object@MonthlyProductionValues_grass[7,4]),"\t# July",sep="")
			infiletext[93] <- paste(format(object@MonthlyProductionValues_grass[8,1]),"\t",format(object@MonthlyProductionValues_grass[8,2]),"\t",format(object@MonthlyProductionValues_grass[8,3]),"\t",format(object@MonthlyProductionValues_grass[8,4]),"\t# August",sep="")
			infiletext[94] <- paste(format(object@MonthlyProductionValues_grass[9,1]),"\t",format(object@MonthlyProductionValues_grass[9,2]),"\t",format(object@MonthlyProductionValues_grass[9,3]),"\t",format(object@MonthlyProductionValues_grass[9,4]),"\t# September",sep="")
			infiletext[95] <- paste(format(object@MonthlyProductionValues_grass[10,1]),"\t",format(object@MonthlyProductionValues_grass[10,2]),"\t",format(object@MonthlyProductionValues_grass[10,3]),"\t",format(object@MonthlyProductionValues_grass[10,4]),"\t# October",sep="")
			infiletext[96] <- paste(format(object@MonthlyProductionValues_grass[11,1]),"\t",format(object@MonthlyProductionValues_grass[11,2]),"\t",format(object@MonthlyProductionValues_grass[11,3]),"\t",format(object@MonthlyProductionValues_grass[11,4]),"\t# November",sep="")
			infiletext[97] <- paste(format(object@MonthlyProductionValues_grass[12,1]),"\t",format(object@MonthlyProductionValues_grass[12,2]),"\t",format(object@MonthlyProductionValues_grass[12,3]),"\t",format(object@MonthlyProductionValues_grass[12,4]),"\t# December",sep="")

			infiletext[99] <- "# Shrublands component:"
			infiletext[100] <- "#Litter\tBiomass\t%Live\tLAI_conv"
			infiletext[101] <- paste(format(object@MonthlyProductionValues_shrub[1,1]),"\t",format(object@MonthlyProductionValues_shrub[1,2]),"\t",format(object@MonthlyProductionValues_shrub[1,3]),"\t",format(object@MonthlyProductionValues_shrub[1,4]),"\t# January",sep="")
			infiletext[102] <- paste(format(object@MonthlyProductionValues_shrub[2,1]),"\t",format(object@MonthlyProductionValues_shrub[2,2]),"\t",format(object@MonthlyProductionValues_shrub[2,3]),"\t",format(object@MonthlyProductionValues_shrub[2,4]),"\t# February",sep="")
			infiletext[103] <- paste(format(object@MonthlyProductionValues_shrub[3,1]),"\t",format(object@MonthlyProductionValues_shrub[3,2]),"\t",format(object@MonthlyProductionValues_shrub[3,3]),"\t",format(object@MonthlyProductionValues_shrub[3,4]),"\t# March",sep="")
			infiletext[104] <- paste(format(object@MonthlyProductionValues_shrub[4,1]),"\t",format(object@MonthlyProductionValues_shrub[4,2]),"\t",format(object@MonthlyProductionValues_shrub[4,3]),"\t",format(object@MonthlyProductionValues_shrub[4,4]),"\t# April",sep="")
			infiletext[105] <- paste(format(object@MonthlyProductionValues_shrub[5,1]),"\t",format(object@MonthlyProductionValues_shrub[5,2]),"\t",format(object@MonthlyProductionValues_shrub[5,3]),"\t",format(object@MonthlyProductionValues_shrub[5,4]),"\t# May",sep="")
			infiletext[106] <- paste(format(object@MonthlyProductionValues_shrub[6,1]),"\t",format(object@MonthlyProductionValues_shrub[6,2]),"\t",format(object@MonthlyProductionValues_shrub[6,3]),"\t",format(object@MonthlyProductionValues_shrub[6,4]),"\t# June",sep="")
			infiletext[107] <- paste(format(object@MonthlyProductionValues_shrub[7,1]),"\t",format(object@MonthlyProductionValues_shrub[7,2]),"\t",format(object@MonthlyProductionValues_shrub[7,3]),"\t",format(object@MonthlyProductionValues_shrub[7,4]),"\t# July",sep="")
			infiletext[108] <- paste(format(object@MonthlyProductionValues_shrub[8,1]),"\t",format(object@MonthlyProductionValues_shrub[8,2]),"\t",format(object@MonthlyProductionValues_shrub[8,3]),"\t",format(object@MonthlyProductionValues_shrub[8,4]),"\t# August",sep="")
			infiletext[109] <- paste(format(object@MonthlyProductionValues_shrub[9,1]),"\t",format(object@MonthlyProductionValues_shrub[9,2]),"\t",format(object@MonthlyProductionValues_shrub[9,3]),"\t",format(object@MonthlyProductionValues_shrub[9,4]),"\t# September",sep="")
			infiletext[110] <- paste(format(object@MonthlyProductionValues_shrub[10,1]),"\t",format(object@MonthlyProductionValues_shrub[10,2]),"\t",format(object@MonthlyProductionValues_shrub[10,3]),"\t",format(object@MonthlyProductionValues_shrub[10,4]),"\t# October",sep="")
			infiletext[111] <- paste(format(object@MonthlyProductionValues_shrub[11,1]),"\t",format(object@MonthlyProductionValues_shrub[11,2]),"\t",format(object@MonthlyProductionValues_shrub[11,3]),"\t",format(object@MonthlyProductionValues_shrub[11,4]),"\t# November",sep="")
			infiletext[112] <- paste(format(object@MonthlyProductionValues_shrub[12,1]),"\t",format(object@MonthlyProductionValues_shrub[12,2]),"\t",format(object@MonthlyProductionValues_shrub[12,3]),"\t",format(object@MonthlyProductionValues_shrub[12,4]),"\t# December",sep="")

			infiletext[114] <- "# Forest component:"
			infiletext[115] <- "#Litter\tBiomass\t%Live\tLAI_conv"
			infiletext[116] <- paste(format(object@MonthlyProductionValues_tree[1,1]),"\t",format(object@MonthlyProductionValues_tree[1,2]),"\t",format(object@MonthlyProductionValues_tree[1,3]),"\t",format(object@MonthlyProductionValues_tree[1,4]),"\t# January",sep="")
			infiletext[117] <- paste(format(object@MonthlyProductionValues_tree[2,1]),"\t",format(object@MonthlyProductionValues_tree[2,2]),"\t",format(object@MonthlyProductionValues_tree[2,3]),"\t",format(object@MonthlyProductionValues_tree[2,4]),"\t# February",sep="")
			infiletext[118] <- paste(format(object@MonthlyProductionValues_tree[3,1]),"\t",format(object@MonthlyProductionValues_tree[3,2]),"\t",format(object@MonthlyProductionValues_tree[3,3]),"\t",format(object@MonthlyProductionValues_tree[3,4]),"\t# March",sep="")
			infiletext[119] <- paste(format(object@MonthlyProductionValues_tree[4,1]),"\t",format(object@MonthlyProductionValues_tree[4,2]),"\t",format(object@MonthlyProductionValues_tree[4,3]),"\t",format(object@MonthlyProductionValues_tree[4,4]),"\t# April",sep="")
			infiletext[120] <- paste(format(object@MonthlyProductionValues_tree[5,1]),"\t",format(object@MonthlyProductionValues_tree[5,2]),"\t",format(object@MonthlyProductionValues_tree[5,3]),"\t",format(object@MonthlyProductionValues_tree[5,4]),"\t# May",sep="")
			infiletext[121] <- paste(format(object@MonthlyProductionValues_tree[6,1]),"\t",format(object@MonthlyProductionValues_tree[6,2]),"\t",format(object@MonthlyProductionValues_tree[6,3]),"\t",format(object@MonthlyProductionValues_tree[6,4]),"\t# June",sep="")
			infiletext[122] <- paste(format(object@MonthlyProductionValues_tree[7,1]),"\t",format(object@MonthlyProductionValues_tree[7,2]),"\t",format(object@MonthlyProductionValues_tree[7,3]),"\t",format(object@MonthlyProductionValues_tree[7,4]),"\t# July",sep="")
			infiletext[123] <- paste(format(object@MonthlyProductionValues_tree[8,1]),"\t",format(object@MonthlyProductionValues_tree[8,2]),"\t",format(object@MonthlyProductionValues_tree[8,3]),"\t",format(object@MonthlyProductionValues_tree[8,4]),"\t# August",sep="")
			infiletext[124] <- paste(format(object@MonthlyProductionValues_tree[9,1]),"\t",format(object@MonthlyProductionValues_tree[9,2]),"\t",format(object@MonthlyProductionValues_tree[9,3]),"\t",format(object@MonthlyProductionValues_tree[9,4]),"\t# September",sep="")
			infiletext[125] <- paste(format(object@MonthlyProductionValues_tree[10,1]),"\t",format(object@MonthlyProductionValues_tree[10,2]),"\t",format(object@MonthlyProductionValues_tree[10,3]),"\t",format(object@MonthlyProductionValues_tree[10,4]),"\t# October",sep="")
			infiletext[126] <- paste(format(object@MonthlyProductionValues_tree[11,1]),"\t",format(object@MonthlyProductionValues_tree[11,2]),"\t",format(object@MonthlyProductionValues_tree[11,3]),"\t",format(object@MonthlyProductionValues_tree[11,4]),"\t# November",sep="")
			infiletext[127] <- paste(format(object@MonthlyProductionValues_tree[12,1]),"\t",format(object@MonthlyProductionValues_tree[12,2]),"\t",format(object@MonthlyProductionValues_tree[12,3]),"\t",format(object@MonthlyProductionValues_tree[12,4]),"\t# December",sep="")

			infiletext[129] <- "# FORB component:"
			infiletext[130] <- "#Litter\tBiomass\t%Live\tLAI_conv"
			infiletext[131] <- paste(format(object@MonthlyProductionValues_forb[1,1]),"\t",format(object@MonthlyProductionValues_forb[1,2]),"\t",format(object@MonthlyProductionValues_forb[1,3]),"\t",format(object@MonthlyProductionValues_forb[1,4]),"\t# January",sep="")
			infiletext[132] <- paste(format(object@MonthlyProductionValues_forb[2,1]),"\t",format(object@MonthlyProductionValues_forb[2,2]),"\t",format(object@MonthlyProductionValues_forb[2,3]),"\t",format(object@MonthlyProductionValues_forb[2,4]),"\t# February",sep="")
			infiletext[133] <- paste(format(object@MonthlyProductionValues_forb[3,1]),"\t",format(object@MonthlyProductionValues_forb[3,2]),"\t",format(object@MonthlyProductionValues_forb[3,3]),"\t",format(object@MonthlyProductionValues_forb[3,4]),"\t# March",sep="")
			infiletext[134] <- paste(format(object@MonthlyProductionValues_forb[4,1]),"\t",format(object@MonthlyProductionValues_forb[4,2]),"\t",format(object@MonthlyProductionValues_forb[4,3]),"\t",format(object@MonthlyProductionValues_forb[4,4]),"\t# April",sep="")
			infiletext[135] <- paste(format(object@MonthlyProductionValues_forb[5,1]),"\t",format(object@MonthlyProductionValues_forb[5,2]),"\t",format(object@MonthlyProductionValues_forb[5,3]),"\t",format(object@MonthlyProductionValues_forb[5,4]),"\t# May",sep="")
			infiletext[136] <- paste(format(object@MonthlyProductionValues_forb[6,1]),"\t",format(object@MonthlyProductionValues_forb[6,2]),"\t",format(object@MonthlyProductionValues_forb[6,3]),"\t",format(object@MonthlyProductionValues_forb[6,4]),"\t# June",sep="")
			infiletext[137] <- paste(format(object@MonthlyProductionValues_forb[7,1]),"\t",format(object@MonthlyProductionValues_forb[7,2]),"\t",format(object@MonthlyProductionValues_forb[7,3]),"\t",format(object@MonthlyProductionValues_forb[7,4]),"\t# July",sep="")
			infiletext[138] <- paste(format(object@MonthlyProductionValues_forb[8,1]),"\t",format(object@MonthlyProductionValues_forb[8,2]),"\t",format(object@MonthlyProductionValues_forb[8,3]),"\t",format(object@MonthlyProductionValues_forb[8,4]),"\t# August",sep="")
			infiletext[139] <- paste(format(object@MonthlyProductionValues_forb[9,1]),"\t",format(object@MonthlyProductionValues_forb[9,2]),"\t",format(object@MonthlyProductionValues_forb[9,3]),"\t",format(object@MonthlyProductionValues_forb[9,4]),"\t# September",sep="")
			infiletext[140] <- paste(format(object@MonthlyProductionValues_forb[10,1]),"\t",format(object@MonthlyProductionValues_forb[10,2]),"\t",format(object@MonthlyProductionValues_forb[10,3]),"\t",format(object@MonthlyProductionValues_forb[10,4]),"\t# October",sep="")
			infiletext[141] <- paste(format(object@MonthlyProductionValues_forb[11,1]),"\t",format(object@MonthlyProductionValues_forb[11,2]),"\t",format(object@MonthlyProductionValues_forb[11,3]),"\t",format(object@MonthlyProductionValues_forb[11,4]),"\t# November",sep="")
			infiletext[142] <- paste(format(object@MonthlyProductionValues_forb[12,1]),"\t",format(object@MonthlyProductionValues_forb[12,2]),"\t",format(object@MonthlyProductionValues_forb[12,3]),"\t",format(object@MonthlyProductionValues_forb[12,4]),"\t# December",sep="")

			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
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
			for(i in 1:12) object@MonthlyProductionValues_grass[i,] = readNumerics(infiletext[85+i],4)
			for(i in 1:12) object@MonthlyProductionValues_shrub[i,] = readNumerics(infiletext[100+i],4)
			for(i in 1:12) object@MonthlyProductionValues_tree[i,] = readNumerics(infiletext[115+i],4)
			for(i in 1:12) object@MonthlyProductionValues_forb[i,] = readNumerics(infiletext[130+i],4)
			return(object)
		})
