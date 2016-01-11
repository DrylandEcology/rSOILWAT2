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


# TODO: 
# 
# Author: Ryan Murphy
###############################################################################

#######################CONSTANTS##############################################

##############################################################################

swWeatherData<-setClass("swWeatherData",representation(data="matrix",year="integer"),prototype=prototype(data=matrix(data=c(1:366,rep(999,366*3)),nrow=366,ncol=4),year=as.integer(1987)))
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
setMethod(f="initialize",signature="swWeatherData",definition=function(.Object,year,data=NULL){
			if(is.null(data))
				data=matrix(data=c(1:366,rep(999,366*3)),nrow=366,ncol=4)
			colnames(data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			.Object@data<-data
			.Object@year<-as.integer(year)
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swWeatherData",
		definition=function(object) {
			object@data=matrix(data=c(1:366,rep(999,366*3)),nrow=366,ncol=4)
			colnames(object@data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			object@year=integer(1)
			return(object)
		})
setMethod("swWriteLines", signature=c(object="swWeatherData", file="character"), definition=function(object, file) {
			dir.create(dirname(file),showWarnings = FALSE, recursive=TRUE)
			infilename <- file.path(dirname(file), paste(basename(file),object@year,sep="."))
			infiletext <- character(dim(object@data)[1]+2)
			infiletext[1] <- paste("# weather for site: year =  ",object@year,sep="")
			infiletext[2] <- paste("# DOY Tmax(C) Tmin(C) PPT(cm)")
			
			for(i in 1:dim(object@data)[1]) {
				infiletext[i + 2] <- paste(format(object@data[i,1]),"\t",format(object@data[i,2]),"\t",format(object@data[i,3]),"\t",format(object@data[i,4]),sep="")
			}
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swWeatherData",file="character"), definition=function(object,file) {
			object@year = as.integer(strsplit(x=basename(file),split=".",fixed=TRUE)[[1]][2])
			#data <-read.csv(file,header=FALSE,skip=2,sep="\t")
			data <- read.table(file, header=FALSE, comment.char="#", blank.lines.skip=TRUE, sep="\t")
			colnames(data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			object@data = as.matrix(data)
			return(object)
		})
