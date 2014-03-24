% 
% TODO: Write Documentation 
% 
\name{addWeatherDataToDataBase}
\alias{addWeatherDataToDataBase}
\title{Rsoilwat addWeatherDataToDataBase}
\description{
	Adds one site 
}
\usage{
	addWeatherDataToDataBase(dbFilePath, Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current")
}
\arguments{
	\item{dbFilePath}{A filepath to the database containing the weather data.}
	\item{Site_id}{Numeric. Used to make a new site or add data to existing site.}
	\item{lat}{Numeric. Latitude used with longitude to identify site id if provided.}
	\item{long}{Numeric. Longitude and Latitude are used to identify site if provided.}
	\item{weatherFolderPath}{String. A folder to read in weather data from. Weather data must have prefix weath.'Year'}
	\item{weatherData}{Numeric. Rsoilwat Weather List of Weatherdata objects to be used instead of weather folder for insertion.}
	\item{label}{Numeric. A discriptive label for the site used with lat & long or without.}
	\item{ScenarioName}{Name of the scenario for that site.}
}
\details{
	This function handles the adding of the data to the weather database. It creates a connection
	to the weather database and depending on the parameters given will insert a new site and/or scenario
	to the database. Then it will create a new row for the data.
	
	Site_id is not required but can be used to insert new site. If Site_id is not provided, then last
	Site_id+1 will be used. {lat,long} are used to lookup a Site_id. If one does not provide {lat,long}
	then Label can be used to identify a site.
	
	The data can come from files by giving path to folder with weatherFolderPath. The data can also come
	from memory via the weatherData parameter. weatherData would take exactly what soilwat takes, a list of
	weatherData objects, one for each year. ScenarioName inserts the weather data for that scenario.
}
\value{
	Returns nothing if pass. Returns Error message if fails.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{swInputData}}
	\code{\link{sw_exec}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{getWeatherData_folders}}
	\code{\link{getWeatherData_database}}
	\code{\link{createDatabase}}
	\code{\link{addToDatabaseFromFolders}}
	\code{\link{deleteSiteData}}
}
\examples{
	#Database must first be created
	createDatabase("/home/ryan/dbWeather.sqlite")
	addWeatherDataToDataBase("/home/ryan/dbWeather.sqlite", Site_id=20167, weatherFolderPath="/home/ryan/Prj01/WeatherData", label="Site1")
}
\keyword{sw_inputDataFromFiles}