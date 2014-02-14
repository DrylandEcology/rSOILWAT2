\name{sw_exec}
\alias{sw_exec}
\title{sw_exec}
\description{
	Run the Simulation and get the Output Data.
	Executes Soil Water simulator and returns Soil Water data via List.
	Uses .Call to pass data to/from C library.
}
\usage{
	sw_exec(data=NULL,weatherList=NULL,dir="", files.in="files_v30.in", echo=FALSE, quiet=FALSE)
}
\arguments{
	\item{data}{S4 swInputData. Object is obtained from sw_inputData or sw_inputDataFromFiles.}
	\item{weatherList}{List. List of weather data obtained via onGetWeatherData_database or onGetWeatherData_folders.}
	\item{dir}{String. that represents the path to the Project Directory. Use with files.in}
	\item{files.in}{String. that represents the partial path, after project dir, and file name to files.in file.}
	\item{echo}{Logical. This option will echo the inputs to the Log Data. Helpful for debugging.}
	\item{quiet}{Logical. quiet mode, don't print message to check logfile.}
}
\details{
	The input data can come from two different locations. It can either
	come from files with the 'dir' option and 'files.in' option set or
	the input can come from memory via S4 object container class. This
	object is obtained via one of the two functions, 'sw_inputData' or
	'sw_inputDataFromFiles'. The weather data can be a part of the input
	data S4 class or separate as a list to reduce the input object size.
	The option 'weatherList' if set will use the weatherList provided
	and not the inputdata object's weather data if it has it. Weather
	data can also be read in when 'dir' and 'files.in' are set.
}
\value{
	A S4 Object Containing 28 outputkey slots.
	Each of those has slots for Yr Mo Wk Dy.
	
	NOTE: WTHR ALLH2O ET ALLVEG do not work.
	
	The output also contains slots for the number of rows
	for each time period. Individual outputKeys also have slots
	for time periods used. Title from outputsetup. They also have
	the number of data columns.
	
	These elements contain the output data from soilwat based on the outputSetup.
	Output columns should all be labeled.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{sw_inputData}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{onGetWeatherData_database}}
	\code{\link{onGetWeatherData_folders}}
}
\examples{
	## R's working directory is in a SoilWat project directory
	## and files.in file is named files_v30.in. No colNames.
	run1<-sw_exec()
	run1[[1]][[1]] #aet yr
	run1[[23]] #logfile
	
	## No column names. From Files. R's working dir is different then Project
	## dir. Echo the inputs.
	run2<-sw_exec(dir="/home/ryan/Documents/Work/SoilWatProject1/", echo=TRUE)
	run2[[1]][[1]] #aet yr
	run2[[23]] #logfile with echoed inputs
	
	## Default data set. Default data set includes weather data. 
	## Column Names are also turned on for output
	inputData3 <- sw_sw_inputData()
	run3<-sw_exec(data=inputData3, colNames=TRUE)
	run3$aet$yr
	run3$logfile
	
	## Input data from Files. Will read in weather data if avail. 
	## Column Names are also turned on for output
	inputData4 <- sw_inputDataFromFiles(dir="/home/ryan/Documents/Work/SoilWatProject1/", file.in="files_v27.in")
	run4<-sw_exec(data=inputData4, colNames=TRUE)
	run4$aet$yr
	run4$logfile
	
	## Input data from Files but separate Weather data.
	## Column Names are also turned on for output
	inputData4 <- sw_inputDataFromFiles(dir="/home/ryan/Documents/Work/SoilWatProject1/", file.in="files_v27.in")
	weatherData <- onGetWeatherData_folders(LookupWeatherFolder="/path/to/weatherfolder/", weatherDirName="blabla", filebasename="weath", startYear=swYears_StartYear(inputData4), endYear=swYears_EndYear(inputData4))
	run4<-sw_exec(data=inputData4, weatherList=weatherData, colNames=TRUE)
	run4$aet$yr
	run4$logfile
}
\keyword{sw_exec}