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


# TODO:
#
# Author: Ryan Murphy
###############################################################################

#######################CONSTANTS##############################################

##############################################################################

#' @export
setClass("swWeatherData", slots = c(data = "matrix", year = "integer"))

swWeatherData_validity<-function(object){
	if(length(object@year)!=1|is.na(object@year)|object@year<0)
		return("@year needs to be length 1,not NA and positive.")
	if(!any(dim(object@data)[1]==c(365,366)))
		return("@data in swWeather Data number of days is not right.")
	if(dim(object@data)[2]!=4)
		return("@data in swWeatherData number of columns is not right.")
	TRUE
}
setValidity("swWeatherData",swWeatherData_validity)

setMethod("initialize", signature = "swWeatherData", function(.Object, ...,
  year = 0L, data = NULL) {

  def <- slot(inputData, "weatherHistory")[[1]] # first year of weather data
  # We don't set values for slots `year` and `data`; this is to prevent simulation runs with
  # accidentally incorrect values

  if (is.null(data)) {
    data <- matrix(c(1:366, rep(kSOILWAT2()[["kINT"]][["SW_MISSING"]], 366 * 3)),
      nrow = 366, ncol = 4)
  }
  colnames(data) <- colnames(slot(def, "data"))
  .Object@data <- data

  .Object@year <- as.integer(year)

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

setMethod("swReadLines", signature=c(object="swWeatherData",file="character"), definition=function(object,file) {
			object@year = as.integer(strsplit(x=basename(file),split=".",fixed=TRUE)[[1]][2])
			#data <-read.csv(file,header=FALSE,skip=2,sep="\t")
			data <- read.table(file, header=FALSE, comment.char="#", blank.lines.skip=TRUE, sep="\t")
			colnames(data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			object@data = as.matrix(data)
			return(object)
		})
