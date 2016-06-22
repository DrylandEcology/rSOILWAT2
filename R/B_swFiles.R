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
##############################################

#################################
print("swFiles")
swFiles<-setClass("swFiles",representation(ProjDir="character",InFiles="character",WeatherPrefix="character",OutputPrefix="character"),prototype=prototype(ProjDir="",InFiles=c('files_v30.in','Input/years.in','Output/logfile.log','Input/siteparam_v26.in','Input/soils_v30.in','Input/weathsetup_v20.in','Input/mkv_prob.in','Input/mkv_covar.in','Input/cloud_v20.in','Input/sbe_prod_v31.in','Input/estab.in','Input/swcsetup.in','Input/outsetup_v30.in'),WeatherPrefix="Weather_Test/weath",OutputPrefix="Output/"))

swFiles_validity<-function(object){
	if(length(object@ProjDir)!=1)
		return("ProjectFolder@ProjectFolderneedstobeoflengthone.")
	if(length(object@InFiles)!=13)
		return("InFiles@InFilesneedstobeoflength13.")
	if(length(object@WeatherPrefix)!=1)
		return("WeatherPrefix@WeatherPrefixneedstobeoflength1.")
	if(length(object@OutputPrefix)!=1)
		return("OutputPrefix@OutputPrefixneedstobeoflength1.")
	#ShouldwechecksizelimitretrictionsinC?
	TRUE
}
setValidity("swFiles",swFiles_validity)
setMethod("initialize","swFiles",function(.Object,ProjDir="",InFiles=c('files_v30.in','Input/years.in','Output/logfile.log','Input/siteparam_v26.in','Input/soils_v30.in','Input/weathsetup_v20.in','Input/mkv_prob.in','Input/mkv_covar.in','Input/cloud_v20.in','Input/sbe_prod_v31.in','Input/estab.in','Input/swcsetup.in','Input/outsetup_v30.in'),WeatherPrefix="Weather_Test/weath",OutputPrefix="Output/"){
			names(InFiles) <- c("InputFilesForSimulation","Model_Years","Model_LogFile","Site_Params","Site_Soils","Weather_setup","Markov_precip_probs","Markov_covarianceTable","Weather_atmosphericParams","Vegetation_Productivity","Vegetation_Establishment","SWC_setup","Output_setup")
			.Object@ProjDir=ProjDir
			.Object@InFiles=InFiles
			.Object@WeatherPrefix=WeatherPrefix
			.Object@OutputPrefix=OutputPrefix
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swFiles",
		definition=function(object) {
			object@ProjDir<-character(1)
			object@InFiles<-character(13)
			names(object@InFiles) <- c("InputFilesForSimulation","Model_Years","Model_LogFile","Site_Params","Site_Soils","Weather_setup","Markov_precip_probs","Markov_covarianceTable","Weather_atmosphericParams","Vegetation_Productivity","Vegetation_Establishment","SWC_setup","Output_setup")
			object@WeatherPrefix<-character(1)
			object@OutputPrefix<-character(1)
			return(object)
		})
setMethod("swFiles_ProjDir", "swFiles", function(object) {return(object@ProjDir)})
setMethod("swFiles_filesIn", "swFiles", function(object) {return(object@InFiles[1])})
setMethod("swFiles_Years", "swFiles", function(object) {return(object@InFiles[2])})
setMethod("swFiles_LogFile", "swFiles", function(object) {return(object@InFiles[3])})
setMethod("swFiles_SiteParams", "swFiles", function(object) {return(object@InFiles[4])})
setMethod("swFiles_Soils", "swFiles", function(object) {return(object@InFiles[5])})
setMethod("swFiles_WeatherSetup", "swFiles", function(object) {return(object@InFiles[6])})
setMethod("swFiles_MarkovProbs", "swFiles", function(object) {return(object@InFiles[7])})
setMethod("swFiles_MarkovCov", "swFiles", function(object) {return(object@InFiles[8])})
setMethod("swFiles_Cloud", "swFiles", function(object) {return(object@InFiles[9])})
setMethod("swFiles_Prod", "swFiles", function(object) {return(object@InFiles[10])})
setMethod("swFiles_Estab", "swFiles", function(object) {return(object@InFiles[11])})
setMethod("swFiles_SWCsetup", "swFiles", function(object) {return(object@InFiles[12])})
setMethod("swFiles_Output", "swFiles", function(object) {return(object@InFiles[13])})
setMethod("swFiles_WeatherPrefix", "swFiles", function(object) {return(object@WeatherPrefix)})
setMethod("swFiles_OutputPrefix", "swFiles", function(object) {return(object@OutputPrefix)})
setReplaceMethod(f="swFiles_ProjDir", signature="swFiles", definition=function(object,value) {object@ProjDir <- value; return(object)})
setReplaceMethod(f="swFiles_filesIn", signature="swFiles", definition=function(object,value) {object@InFiles[1] <- value; return(object)})
setReplaceMethod(f="swFiles_Years", signature="swFiles", definition=function(object,value) {object@InFiles[2] <- value; return(object)})
setReplaceMethod(f="swFiles_LogFile", signature="swFiles", definition=function(object,value) {object@InFiles[3] <- value; return(object)})
setReplaceMethod(f="swFiles_SiteParams", signature="swFiles", definition=function(object,value) {object@InFiles[4] <- value; return(object)})
setReplaceMethod(f="swFiles_Soils", signature="swFiles", definition=function(object,value) {object@InFiles[5] <- value; return(object)})
setReplaceMethod(f="swFiles_WeatherSetup", signature="swFiles", definition=function(object,value) {object@InFiles[6] <- value; return(object)})
setReplaceMethod(f="swFiles_MarkovProbs", signature="swFiles", definition=function(object,value) {object@InFiles[7] <- value; return(object)})
setReplaceMethod(f="swFiles_MarkovCov", signature="swFiles", definition=function(object,value) {object@InFiles[8] <- value; return(object)})
setReplaceMethod(f="swFiles_Cloud", signature="swFiles", definition=function(object,value) {object@InFiles[9] <- value; return(object)})
setReplaceMethod(f="swFiles_Prod", signature="swFiles", definition=function(object,value) {object@InFiles[10] <- value; return(object)})
setReplaceMethod(f="swFiles_Estab", signature="swFiles", definition=function(object,value) {object@InFiles[11] <- value; return(object)})
setReplaceMethod(f="swFiles_SWCsetup", signature="swFiles", definition=function(object,value) {object@InFiles[12] <- value; return(object)})
setReplaceMethod(f="swFiles_Output", signature="swFiles", definition=function(object,value) {object@InFiles[13] <- value; return(object)})
setReplaceMethod(f="swFiles_WeatherPrefix", signature="swFiles", definition=function(object,value) {object@WeatherPrefix <- value; return(object)})
setReplaceMethod(f="swFiles_OutputPrefix", signature="swFiles", definition=function(object,value) {object@OutputPrefix <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swFiles", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(28)
			infiletext[1] <- "# List of input files for SOILWAT v32"
			infiletext[2] <- paste("# This is the first file read. Simulation information = ")
			
			infiletext[4] <- "#Model"
			infiletext[5] <- paste(object@InFiles[2], "\t# years for model operation", sep="")   
			infiletext[6] <- paste(object@InFiles[3], "\t# errors or important info (can also be stdout)", sep="")   
			
			infiletext[8] <- "#Site"
			infiletext[9] <- paste(object@InFiles[4], "\t# site parameters", sep="")   
			infiletext[10] <- paste(object@InFiles[5], "\t# soil layer definitions", sep="")
			
			infiletext[12] <- "#Weather"
			infiletext[13] <- paste(object@InFiles[6], "\t# weather parameters", sep="")
			infiletext[14] <- paste(object@WeatherPrefix, "\t# data file containing historical weather (can include path)", sep="")
			infiletext[15] <- paste(object@InFiles[7], "\t# precip probs; required for markov weather", sep="")
			infiletext[16] <- paste(object@InFiles[8], "\t# covariance table required for markov weather", sep="")
			infiletext[17] <- paste(object@InFiles[9], "\t# general atmospheric params", sep="")
			
			infiletext[19] <- "#Vegetation"
			infiletext[20] <- paste(object@InFiles[10], "\t# productivity values", sep="")
			infiletext[21] <- paste(object@InFiles[11], "\t# plant establishment start file", sep="")
			
			infiletext[23] <- "#SWC measurements"

			infiletext[24] <- paste(object@InFiles[12], "\t# params for handling measured swc", sep="")
			
			infiletext[26] <- "#Output"
			infiletext[27] <- paste(object@OutputPrefix, "\t# 'relative' path for output files: / for same directory, or e.g., Output/", sep="")   
			infiletext[28] <- paste(object@InFiles[13], "\t# define output quantities", sep="")
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swFiles",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			object@InFiles[1] = file
			object@InFiles[2] = strsplit(x=infiletext[5],split="\t")[[1]][1]
			object@InFiles[3] = strsplit(x=infiletext[6],split="\t")[[1]][1]
			object@InFiles[4] = strsplit(x=infiletext[9],split="\t")[[1]][1]
			object@InFiles[5] = strsplit(x=infiletext[10],split="\t")[[1]][1]
			object@InFiles[6] = strsplit(x=infiletext[13],split="\t")[[1]][1]
			object@WeatherPrefix = strsplit(x=infiletext[14],split="\t")[[1]][1]
			object@InFiles[7] = strsplit(x=infiletext[15],split="\t")[[1]][1]
			object@InFiles[8] = strsplit(x=infiletext[16],split="\t")[[1]][1]
			object@InFiles[9] = strsplit(x=infiletext[17],split="\t")[[1]][1]
			object@InFiles[10] = strsplit(x=infiletext[20],split="\t")[[1]][1]
			object@InFiles[11] = strsplit(x=infiletext[21],split="\t")[[1]][1]
			object@InFiles[12] = strsplit(x=infiletext[24],split="\t")[[1]][1]
			object@OutputPrefix = strsplit(x=infiletext[27],split="\t")[[1]][1]
			object@InFiles[13] = strsplit(x=infiletext[28],split="\t")[[1]][1]
			names(object@InFiles) <- c("InputFilesForSimulation","Model_Years","Model_LogFile","Site_Params","Site_Soils","Weather_setup","Markov_precip_probs","Markov_covarianceTable","Weather_atmosphericParams","Vegetation_Productivity","Vegetation_Establishment","SWC_setup","Output_setup")
			return(object)
		})
