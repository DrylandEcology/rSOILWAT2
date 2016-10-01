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
###############################################################################


print("swSWC")
swSWC_hist <- setClass(Class="swSWC_hist", representation(data="matrix", year="integer"), prototype=prototype(data=matrix(data=NA, nrow=366,ncol=4,dimnames=list(NULL,c("DOY","Layer","SWC","st_err"))),year=as.integer(1949)))
setMethod(f="swClear",
		signature="swSWC_hist",
		definition=function(object) {
			object@data=matrix(data=NA, nrow=366,ncol=4,dimnames=list(NULL,c("DOY","Layer","SWC","st_err")))
			object@year=integer(1)
			return(object)
		})
setMethod("swWriteLines", signature=c(object="swSWC_hist", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(dim(object@data)[1]+2)
			infiletext[1] <- paste("# SWC History for site: year =  ",object@year,sep="")
			infiletext[2] <- paste("# DOY Layer SWC st_err")

			for(i in 1:dim(object@data)[1]) {
				infiletext[i + 2] <- paste(format(object@data[i,1]),"\t",format(object@data[i,2]),"\t",format(object@data[i,3]),"\t",format(object@data[i,4]),sep="")
			}

			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)})
setMethod("swReadLines", signature=c(object="swSWC_hist",file="character"), definition=function(object,file) {
			object@year = as.integer(strsplit(x=file,split=".",fixed=TRUE)[[1]][2])
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]
			days <- (length(infiletext)-2)
			data=matrix(data=NA,nrow=days,ncol=4)
			colnames(data)<-c("DOY","Tmax_C","Tmin_C","PPT_cm")
			for(i in 3:length(infiletext)) {
				data[i-2,] <- readNumerics(infiletext[i],4)
			}
			object@data = data
			return(object)
		})
##########################swcsetup.in#########################################

swSWC <- setClass(Class="swSWC", representation(UseSWCHistoricData="logical",DataFilePrefix="character",FirstYear="integer",Method="integer",History="list"),
		prototype=prototype(UseSWCHistoricData=FALSE,DataFilePrefix="swcdata",FirstYear=as.integer(1949),Method=as.integer(2),History=list(swSWC_hist())))
setMethod(f="swClear",
		signature="swSWC",
		definition=function(object) {
			object@UseSWCHistoricData=logical(1)
			object@DataFilePrefix=character(1)
			object@FirstYear=integer(1)
			object@Method=integer(1)
			object@History=list(swClear(new("swSWC_hist")))
			return(object)
		})

setMethod("swSWC_use","swSWC",function(object) {return(object@UseSWCHistoricData)})
setMethod("swSWC_prefix","swSWC",function(object) {return(object@DataFilePrefix)})
setMethod("swSWC_FirstYear","swSWC",function(object) {return(object@FirstYear)})
setMethod("swSWC_Method","swSWC",function(object) {return(object@Method)})
setMethod("swSWC_HistoricList","swSWC",function(object) {return(object@History)})
setMethod("swSWC_HistoricData","swSWC",function(object, year) {
			index<-which(names(object@History) == as.character(year))
			if(length(index) != 1) {
				print("swc historic data Index has wrong length.")
				return(NULL)
			}
			if(object@History[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object@History[[index]])
		})

setReplaceMethod(f="swSWC_use",signature=c(object="swSWC", value="logical"),function(object, value) {object@UseSWCHistoricData <- value; return(object)})
setReplaceMethod(f="swSWC_prefix",signature=c(object="swSWC", value="character"),function(object, value) {object@UseSWCHistoricData <- value; return(object)})
setReplaceMethod(f="swSWC_FirstYear",signature=c(object="swSWC",value="integer"),function(object,value) {object@UseSWCHistoricData <- value; return(object)})
setReplaceMethod(f="swSWC_Method",signature=c(object="swSWC", value="integer"),function(object, value) {object@UseSWCHistoricData <- value; return(object)})
setReplaceMethod(f="swSWC_HistoricList",signature=c(object="swSWC", value="list"),function(object, value) {object@UseSWCHistoricData <- value; return(object)})
setReplaceMethod(f="swSWC_HistoricData",signature=c(object="swSWC", value="swSWC_hist"),function(object, value) {
			index<-which(names(object@History) == as.character(value@year))
			if(length(index) == 0) {
				object@History[[length(object@History)+1]] <- value
				years <- unlist(lapply(object@History, function(x) {return(x@year)}))
				object@History <- object@History[order(years)]
				names(object@History)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("SWC data is Missing")
			} else if(length(index) == 1) {
				object@History[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})

setMethod("swWriteLines", signature=c(object="swSWC", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext<-character(7)
			infiletext[1]<-"# Setup parameters for measured swc"
			infiletext[2]<-"# Location: -"
			infiletext[3]<-"#"
			infiletext[4]<-paste(as.character(as.integer(object@UseSWCHistoricData)),"# 1=use swcdata history data file, 0= don't use",sep="\t")
			infiletext[5]<-paste(object@DataFilePrefix,"# input data file prefix",sep="\t")
			infiletext[6]<-paste(as.character(object@FirstYear),"# first year of measurement data files",sep="\t")
			infiletext[7]<-paste(as.character(object@Method),"# method: 1=average with model; 2=measured+/- stderr",sep="\t")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swSWC",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]
			object@UseSWCHistoricData = readLogical(infiletext[4])
			object@DataFilePrefix = readCharacter(infiletext[5])
			object@FirstYear = readInteger(infiletext[6])
			object@Method = readInteger(infiletext[7])
			return(object)
		})

