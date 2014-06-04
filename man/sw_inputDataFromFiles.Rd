% 
% TODO: Write Documentation 
% 
\name{sw_inputDataFromFiles}
\alias{sw_inputDataFromFiles}
\title{Rsoilwat sw_inputDataFromFiles}
\description{
	Reads in a SoilWat Project's input data.
	Returns swInputData Object.
}
\usage{
	sw_inputDataFromFiles(dir="", files.in="")
}
\arguments{
	\item{dir}{String. The path to the Project Directory. Use with files.in}
	\item{files.in}{String. The partial path, after project dir path, and file name to files.in file.}
}
\details{
	swInputData Object is a S4 object containing slots for all input data.
	Calling sw_inputDataFromFiles generates a new swInputData object with
	data from the Project files given.
	
	The data returned can be directly used to run the simulation if weather Data
	was included in the Project Directory. If weather data was not in the project
	directory then use onGetWeatherData_database or onGetWeatherData_folders to
	obtain weatherDataList and pass both to sw_exec.
	
	swInputData consists of slots for each file that is read in. These slots
	can be accessed via the following functions.
	get_Markov			#get markov prop and conv.
	get_swCloud			#get cloud
	get_swFiles			#get files.in
	get_swOUT			#get outpusetup.in
	get_swProd			#get prod.in
	get_swSite			#get site.in
	get_swSoils			#get soils.in
	get_swSWC			#get swcSetup.in
	get_swWeather		#get weatherSetup.in
	get_swWeatherData	#get individual year
	get_swYears			#get years.in data object
	get_WeatherHistory	#get S4 of WeatherHistory
	
	generic functions to get/set individual elements follow a format
	sw'then file name'_'option'
	e.g. swFiles_Cloud
		swProd_Albedo
	remember tab complete is your friend.
}
\value{
	Returns a S4 class of type swInputData. This is a container for the input
	S4 objects used to run soilwat. The data for the given project is loaded.
}
\author{Ryan Murphy}
\seealso{
	\code{\link{swInputData}}
	\code{\link{sw_exec}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{getWeatherData_database}}
	\code{\link{getWeatherData_folders}}
}
\examples{
	## Default data set. Default data set includes weather data. 
	## Column Names are also turned on for output
	inputData <- sw_inputDataFromFiles(dir="/path/to/project",files.in="files_v27.in") #Get the data set
	swFiles_Cloud(inputData) #Look at some settings
	swProd_Albedo(inputData)
	#Run the simulation
	run<-sw_exec(data=inputData, colNames=TRUE)
}
\keyword{sw_inputDataFromFiles}

