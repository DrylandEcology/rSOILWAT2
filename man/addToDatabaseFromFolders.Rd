% 
% TODO: Write Documentation 
% 
\name{addToDatabaseFromFolders}
\alias{addToDatabaseFromFolders}
\title{Rsoilwat addToDatabaseFromFolders}
\description{
	Adds Multiple Sites To Weather Database.
}
\usage{
	addToDatabaseFromFolders(dbFilePath="dbWeatherData.sqlite", MetaData=NULL, FoldersPath, ScenarioName="Current")
}
\arguments{
	\item{dbFilePath}{A filepath to the future database file.}
	\item{MetaData}{Defines what folders are read in and what data is stored in Sites table. See details for details.}
	\item{FoldersPath}{The Path to the folders to be inserted into the database.}
	\item{ScenarioName}{All data is inserted as this scenario}
}
\details{
	This function reads in multiple weather data folders and inserts them into the database.
	MetaData is optional. If it is not provided, then the folders names will be used as a label
	for the sites table no lat or long will be used. MetaData allows you to define the lat and long 
	and label for the sites table. It also allows you to define only the folders you want.
	
	Structure of MetaData: Dataframe with the following columns
	folder, lat, long, label
	
	folder - folder name to be read in.
	lat - Latitude for that folder/site
	long - Longitude for that folder/site
	label - Label for site. Could be folder name 
	
}
\value{
	Returns nothing if pass. If fails then error message.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{swInputData}}
	\code{\link{sw_exec}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{getWeatherData_folders}}
	\code{\link{addWeatherDataToDataBase}}
	\code{\link{createDatabase}}
	\code{\link{getWeatherData_database}}
	\code{\link{deleteSiteData}}
}
\examples{
	addToDatabaseFromFolders(dbFilePath="dbWeatherData.sqlite", MetaData=NULL, "/home/ryan/weatherData", ScenarioName="Current")
}
\keyword{addToDatabaseFromFolders}