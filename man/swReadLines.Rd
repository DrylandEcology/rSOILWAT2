% 
% TODO: Write Documentation 
% 
\name{swReadLines}
\alias{swReadLines}
\title{Rsoilwat swReadLines}
\description{
	Reads input data to file(s) from within R.
	This does not use C code.
}
\usage{
	swReadLines(object)
}
\arguments{
	\item{object}{S4 Rsoilwat inputData class.}
}
\details{
	This will write to hard drive the file represented by the object.
}
\value{
	returns nothing
}
\author{Ryan Murphy}
\seealso{
	\code{\link{swInputData}}
	\code{\link{sw_exec}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{onGetWeatherData_database}}
}
\examples{
	## Default data set without weather data.
	## Column Names are also turned on for output
	inputData <- sw_inputDataFromFiles(dir="/path/to/project",files.in="files_v27.in") #Get the data set
	swReadLines(get_swSite)
}
\keyword{swWriteLines}
