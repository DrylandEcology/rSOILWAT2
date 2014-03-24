% 
% TODO: Write Documentation 
% 
\name{createDatabase}
\alias{createDatabase}
\title{Rsoilwat createDatabase}
\description{
	Creates empty database for weather data.
}
\usage{
	createDatabase(dbFilePath="dbWeatherData.sqlite")
}
\arguments{
	\item{dbFilePath}{A filepath to the future database file.}
}
\details{
	This function creates an empty database for storing/accessing weather data.
	It has Three tables.
	
	Table: sites
	--------------------------------------------
	|  Site_id  | Latitude | Longitude | Label |
	--------------------------------------------
	
	Table: scenarios
	-------------------
	|  id  | Scenario |
	-------------------
	
	Table: weatherdata
	-----------------------------
	| Site_id | Scenario | data |
	-----------------------------
	
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
	\code{\link{getWeatherData_database}}
	\code{\link{addToDatabaseFromFolders}}
	\code{\link{deleteSiteData}}
}
\examples{
	library(Rsoilwat)
	createDatabase()
}
\keyword{createDatabase}