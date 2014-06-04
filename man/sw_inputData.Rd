% 
% TODO: Review
% 
\name{sw_inputData}
\alias{sw_inputData}
\title{Rsoilwat sw_inputData}
\description{
	Default data Set for SoilWat.
	Use this as a template or for testing the package.
}
\usage{
	sw_inputData()
}
\arguments{
	
}
\details{
	swInputData Object is a S4 object containing slots for all input data.
	Calling sw_inputData() generates a new swInputData object with test
	project data loaded into them. This can be used for testing or as a
	template to build another project off of.
	
	The data returned can be directly used to run the simulation. Use the
	function sw_exec and pass it the output from this function to run SW.
	
	showMethods(class="swInputData") # Will show all generic functions that
	be applied to the inputData. These include getters/setters for slots.
	
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
	get_WeatherHistory	#get S4 of WeatherHistory\
	
	generic functions to get/set individual elements follow a format
	sw'then file name'_'option'
	e.g. swFiles_Cloud
		swProd_Albedo
	remember tab complete is your friend.
}
\value{
	Returns a S4 class of type swInputData. This is a container for the input
	S4 objects used to run soilwat. The data for a test project is loaded.
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
	inputData <- sw_inputData() #Get the default data set
	swFiles_Cloud(inputData) #Look at some settings
	swProd_Albedo(inputData)
	#Run the simulation
	run<-sw_exec(data=inputData, colNames=TRUE)
}
\keyword{sw_inputData}

