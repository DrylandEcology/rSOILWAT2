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


sw_exec <- function(inputData=NULL,weatherList=NULL,dir="", files.in="files_v31.in", echo=FALSE, quiet=FALSE) {
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(files.in!="")
		input<-c(input,"-f", files.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	if(is.null(inputData)) {
		inputData<-sw_inputDataFromFiles(dir=dir,files.in=files.in)
	}
	#if(colNames)
	#	.Call("onSetNames",data)
	return(.Call("start",input,inputData,weatherList))
}

sw_inputDataFromFiles <- function(dir="", files.in="files_v30.in") {
	echo=FALSE
	quiet=FALSE
	
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(files.in!="")
		input<-c(input,"-f", files.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	data <- .Call("onGetInputDataFromFiles",input)
	
	return(data)
}

sw_outputData <- function(inputData) {
	.Call("onGetOutput",inputData)
}

sw_inputData <- function() {
	temp<-swInputData()
	data(package="Rsoilwat31", weatherData)
	temp@weatherHistory <- weatherData
	return(temp)
}
