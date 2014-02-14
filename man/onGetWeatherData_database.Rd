% 
% TODO: Write Documentation 
% 
\name{onGetWeatherData_database}
\alias{onGetWeatherData_database}
\title{Rsoilwat onGetWeatherData_database}
\description{
	Reads weather data from database.
	Returns list of weather data.
}
\usage{
	onGetWeatherData_database(con, Site_id=NULL,lat=NULL,long=NULL,weatherDirName=NULL,startYear=NULL,endYear=NULL)
}
\arguments{
	\item{con}{RSQLite con object. A connection to the weather data database. requires(RSQLite)}
	\item{Site_id}{Numeric. Used to identify site and extract weather data.}
	\item{lat}{Numeric. Latitude used with longitude to identify site id if Site_id is missing.}
	\item{long}{Numeric. Longitude and Latitude are used to identify site if Site_id is missing.}
	\item{weatherDirName}{String. If Site_id, lat, and long is missing then this will be used to identify Site_id by parsing the lat and long out of the name.}
	\item{startYear}{Numeric. Extracted weather data will start with this year.}
	\item{endYear}{Numeric. Extracted weather data will end with this year.}
}
\details{
	Weather data for the soil water simulation run can be stored in the input data
	or it can be separate to keep the input data size down for multiple variations
	of the same site. This function is used to return the weather data from a pre
	defined weather database. Using the database was faster then reading in multiple
	weather files.
	
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
	\code{\link{onGetWeatherData_folders}}
}
\examples{
	## Default data set without weather data.
	## Column Names are also turned on for output
	library(Rsoilwat)
	library(RSQLite)
	drv <- dbDriver("SQLite")
	con <- dbConnect(drv, "/path/to/weather/database/lookupWeatherDB.sqlite3")
	inputData <- sw_inputDataFromFiles(dir="/path/to/project",files.in="files_v27.in") #Get the data set
	weatherData <- onGetWeatherData_database(con, Site_id=200)
	#Run the simulation
	run<-sw_exec(data=inputData, weatherList=weatherData, colNames=TRUE)
}
\keyword{sw_inputDataFromFiles}

