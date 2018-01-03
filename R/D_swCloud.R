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


########################CLOUD DATA################################
#' @export
setClass("swCloud", slots = c(Cloud = "matrix"))


swCloud_validity<-function(object){
	if(dim(object@Cloud)[1]!=5)
		return("@data in swCloud to many/few rows.")
	if(dim(object@Cloud)[2]!=12)
		return("@data in swCloud to many/few cols.")
	TRUE
}
setValidity("swCloud",swCloud_validity)

setMethod("initialize", signature = "swCloud", function(.Object, ...) {
  def <- slot(inputData, "cloud")

  # We don't set values for slot `Cloud`; this is to prevent simulation runs with
  # accidentally incorrect values
  temp <- def@Cloud
  temp[] <- NA_real_
  .Object@Cloud <- temp

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})



setMethod("get_swCloud","swCloud",function(object) {return(object)})
setMethod("swCloud_SkyCover","swCloud",function(object) {return(object@Cloud[1,])})
setMethod("swCloud_WindSpeed","swCloud",function(object) {return(object@Cloud[2,])})
setMethod("swCloud_Humidity","swCloud",function(object) {return(object@Cloud[3,])})
setMethod("swCloud_Transmissivity","swCloud",function(object) {return(object@Cloud[4,])})
setMethod("swCloud_SnowDensity","swCloud",function(object) {return(object@Cloud[5,])})

setReplaceMethod(f="set_swCloud",signature="swCloud",function(object,value) {object <- value; return(object)})
setReplaceMethod(f="swCloud_SkyCover",signature="swCloud",function(object,value) {object@Cloud[1,] <- value; return(object)})
setReplaceMethod(f="swCloud_WindSpeed",signature="swCloud",function(object,value) {object@Cloud[2,] <- value; return(object)})
setReplaceMethod(f="swCloud_Humidity",signature="swCloud",function(object,value) {object@Cloud[3,] <- value; return(object)})
setReplaceMethod(f="swCloud_Transmissivity",signature="swCloud",function(object,value) {object@Cloud[4,] <- value; return(object)})
setReplaceMethod(f="swCloud_SnowDensity",signature="swCloud",function(object,value) {object@Cloud[5,] <- value; return(object)})


setMethod("swReadLines", signature=c(object="swCloud",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]

			object@Cloud=matrix(data=NA,nrow=5,ncol=12,byrow=T)
			colnames(object@Cloud)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
			rownames(object@Cloud)<-c("SkyCoverPCT","WindSpeed_m/s","HumidityPCT","Transmissivity","SnowDensity_kg/m^3")

			for(i in 1:length(infiletext)) {
				object@Cloud[i,] <- readNumerics(infiletext[i],12)
			}
			return(object)
		})
