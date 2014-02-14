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
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]
			days <- (length(infiletext)-2)
			data=matrix(data=c(1:days,rep(999,days*3)),nrow=days,ncol=4)
			colnames(data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			for(i in 3:length(infiletext)) {
				data[i-2,] <- readNumerics(infiletext[i],4)
			}
			object@data = data
			return(object)
		})
