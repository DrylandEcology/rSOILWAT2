% 
% TODO: Write Documentation 
% 
\name{deleteSiteData}
\alias{deleteSiteData}
\title{Rsoilwat deleteSiteData}
\description{
	Remove one or more weather Data Scenario(s) from Weather Database.
}
\usage{
	deleteSiteData(dbFilePath, Site_id, Scenario=NULL)
}
\arguments{
	\item{dbFilePath}{A filepath to the existing database file.}
	\item{Site_id}{The Id of the Site to remove data from.}
	\item{Scenario}{Removes given scenario or all if NULL.}
}
\details{
	Removes one or all scenarios for a site. Will also remove site if
	it has no more data.
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
	deleteSiteData(dbFilePath="dbWeatherData.sqlite", Site_id=1))
}
\keyword{deleteSiteData}