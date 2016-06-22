###############################################################################
#Rsoilwat and Rsoilwat31
#    Copyright (C) {2009-2016}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


library(Rsoilwat31)
packageVersion("Rsoilwat31")

##### Some of the important available functions
#	- sw_inputData
#	- sw_inputDataFromFiles
#	- sw_exec
#	- getWeatherData_folders
##### For a complete list of available functions
help(package = "Rsoilwat31")


#obtain example input data
swIn1 <- sw_inputData()
str(swIn1, max.level=2)


#Or read input data from files
setwd("~/Dropbox/Work_Stuff/2_Research/Software/GitHub_Projects/Rsoilwat/demo/IntroductionExample")
dir.sw <- getwd()
swIn2 <- sw_inputDataFromFiles(dir=getwd(), files.in="files_v31.in")
str(swIn2, max.level=2)


#Read in forcing weather data (there are also functions to set up a SQLite database for the weather data)
weatherList <- getWeatherData_folders(LookupWeatherFolder=file.path(getwd(), "Input"), weatherDirName="data_weather", filebasename="weath", startYear=1979, endYear=2010)
str(weatherList, max.level=1)


#Execute a SoilWat run
swOut <- sw_exec(inputData=swIn2, weatherList=weatherList, dir="", files.in="files_v31.in", echo=TRUE, quiet=FALSE)
str(swOut, max.level=2)
swOut@Log

str(swOut$sw_pot, max.level=1)
matplot(x=swOut$vwc$yr[,1], swOut$vwc$yr[, -1], ylim=c(0, 0.4), type="l")


#Change input values, e.g.,
swYears_StartYear(swIn2) <- 1979L
swWeather_FirstYearHistorical(swIn2) <- 1979L
swYears_EndYear(swIn2) <- 2010L

swProd_Composition(swIn2) <- c(0.4, 0.6, 0, 0, 0)	


