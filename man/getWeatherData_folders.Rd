% 
% TODO: Review
% 
\name{getWeatherData_folders}
\alias{getWeatherData_folders}
\title{Rsoilwat onGetWeatherData_folders}
\description{
	Reads weather data from files.
	Returns list of weather data.
}
\usage{
	getWeatherData_folders(LookupWeatherFolder=NULL, weatherDirName=NULL, filebasename=NULL, startYear=NULL, endYear=NULL)
}
\arguments{
	\item{LookupWeatherFolder}{String Path. Path to the LookupWeatherFolder location.}
	\item{weatherDirName}{String. Name of the folder containing weather data files.}
	\item{filebasename}{String. File prefix for weather data. Usually 'weath'.}
	\item{startYear}{Numeric. Extracted weather data will start with this year.}
	\item{endYear}{Numeric. Extracted weather data will end with this year.}
}
\details{
	Weather data for the soil water simulation run can be stored in the input data
	or it can be separate to keep the input data size down for multiple variations
	of the same site. This function is used to return the weather data from folders.
	The other option is to use onGetWeatherData_database.
	
	The output from this function can be passed directly to sw_exec with input data.
}
\value{
	Returns weather data as list. Each element contains data for one year.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{swInputData}}
	\code{\link{sw_exec}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{getWeatherData_database}}
}
\examples{
	## Default data set without weather data.
	## Column Names are also turned on for output
	library(Rsoilwat)
	inputData <- sw_inputDataFromFiles(dir="/path/to/project",files.in="files_v27.in") #Get the data set
	weatherData <- getWeatherData_database(LookupWeatherFolder="/projects/1_data/treatments/LookupWeatherFolder", weatherDirName="Weather_NCEPCFSR_0.156--77.813", filebasename="weath", startYear=1960, endYear=1980)
	#Run the simulation
	run<-sw_exec(data=inputData, weatherList=weatherData, colNames=TRUE)
}
\keyword{sw_inputDataFromFiles}