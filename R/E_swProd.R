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

setMethod("initialize", signature = "swProd", function(.Object, ...) {
  def <- slot(inputData, "prod")

  # We don't set values for slot `Composition`; this is to prevent simulation runs with
  # accidentally incorrect values
  temp <- def@Composition
  temp[] <- NA_real_
  .Object@Composition <- temp

  .Object@Albedo <- def@Albedo
  .Object@Cover_stcr <- def@Cover_stcr
  .Object@CanopyHeight <- def@CanopyHeight
  .Object@VegetationInterceptionParameters <- def@VegetationInterceptionParameters
  .Object@LitterInterceptionParameters <- def@LitterInterceptionParameters
  .Object@EsTpartitioning_param <- def@EsTpartitioning_param
  .Object@Es_param_limit <- def@Es_param_limit
  .Object@Shade <- def@Shade
  .Object@HydraulicRedistribution_use <- def@HydraulicRedistribution_use
  .Object@HydraulicRedistribution <- def@HydraulicRedistribution
  .Object@CriticalSoilWaterPotential <- def@CriticalSoilWaterPotential
  .Object@CO2Coefficients <- def@CO2Coefficients
  .Object@MonthlyProductionValues_grass <- def@MonthlyProductionValues_grass
  .Object@MonthlyProductionValues_shrub <- def@MonthlyProductionValues_shrub
  .Object@MonthlyProductionValues_tree <- def@MonthlyProductionValues_tree
  .Object@MonthlyProductionValues_forb <- def@MonthlyProductionValues_forb

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
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
