\name{sw_exec}
\alias{sw_exec}
\title{sw_exec}
\description{
	Run the Simulation and get the Output Data.
	Executes Soil Water simulator and returns Soil Water data via List.
	Uses .Call to pass data to/from C library.
}
\usage{
	sw_exec(data=NULL,weatherList=NULL,dir="", files.in="files_v27.in", echo=FALSE, quiet=FALSE, colNames=FALSE)
}
\arguments{
	\item{data}{S4 swInputData. Object is obtained from sw_inputData or sw_inputDataFromFiles.}
	\item{weatherList}{List. List of weather data obtained via onGetWeatherData_database or onGetWeatherData_folders.}
	\item{dir}{String. that represents the path to the Project Directory. Use with files.in}
	\item{files.in}{String. that represents the partial path, after project dir, and file name to files.in file.}
	\item{echo}{Logical. This option will echo the inputs to the Log Data. Helpful for debugging.}
	\item{quiet}{Logical. quiet mode, don't print message to check logfile.}
	\item{colNames}{Logical. If true, will set the List names and column names on the List.}
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
	A list containing 23 elements.
	sw_aet			<-1;
	sw_deepdrain	<-2;
	sw_estabs		<-3;
	sw_evsoil		<-4;
	sw_evapsurface	<-5;
	sw_hd			<-6;
	sw_inf_soil		<-7;
	sw_interception	<-8;
	sw_percolation	<-9;
	sw_pet			<-10;
	sw_precip		<-11;
	sw_runoff		<-12;
	sw_snow			<-13;
	sw_soiltemp		<-14;
	sw_surfaceWater	<-15;
	sw_swp			<-16;
	sw_swa			<-17;
	sw_swc			<-18;
	sw_temp			<-19;
	sw_transp		<-20;
	sw_vwc			<-21;
	sw_wetdays		<-22;
	sw_logfile		<-23;
	
	Except for the last, logfile 23, each element is another list of 4.
	sw_yr			<-1;
	sw_mo			<-2;
	sw_wk			<-3;
	sw_dy			<-4;
	
	These elements contain the output data from soilwat based on the outputSetup.
	Use the above indexing if column names are turned off. If turned on, they will
	be labeled.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{sw_inputData}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{onGetWeatherData_database}}
	\code{\link{onGetWeatherData_folders}}
}
\examples{
	## No column names. R's working directory is in a SoilWat project directory
	## and files.in file is named files_v27.in. No colNames.
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