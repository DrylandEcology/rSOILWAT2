% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_dbW_WeatherDatabase.R
\name{getWeatherData_folders}
\alias{getWeatherData_folders}
\title{Reads daily weather data from files}
\usage{
getWeatherData_folders(
  LookupWeatherFolder,
  weatherDirName = NULL,
  filebasename = "weath",
  startYear = NULL,
  endYear = NULL,
  dailyInputFlags = c(rep(TRUE, 3L), rep(FALSE, 11L)),
  method = c("R", "C")
)
}
\arguments{
\item{LookupWeatherFolder}{A character string. The path to the parent folder
of \code{weatherDirName}.}

\item{weatherDirName}{String. Name of the folder with the daily weather data
files.}

\item{filebasename}{String. File prefix for weather data. Usually
\var{weath}.}

\item{startYear}{Numeric. Extracted weather data will start with this year.}

\item{endYear}{Numeric. Extracted weather data will end with this year.}

\item{dailyInputFlags}{A logical vector of length \code{MAX_INPUT_COLUMNS},
see \code{"weathsetup.in"}.}

\item{method}{A character string. \code{"R"} uses code in \code{R} to read files as-is
whereas \code{"C"} uses \code{"SOILWAT2"} code to read and process files.}
}
\value{
A list of elements of class \code{\linkS4class{swWeatherData}}.
}
\description{
\pkg{SOILWAT2} does not handle missing weather data. If you have missing
data, then you have to impute yourself or use the built-in Markov weather
generator (see examples for \code{\link{sw_exec}}).
}
\section{Details}{

\code{\link[=dbW_weather_to_SOILWATfiles]{dbW_weather_to_SOILWATfiles()}} offers the inverse operation, i.e.,
writing weather data to disk files.
}

\examples{

path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")

## ------ Simulation with data prepared beforehand and separate weather data
## Read inputs from files on disk (via SOILWAT2)
sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")

## Read forcing weather data from files on disk (via SOILWAT2)
sw_weath3c <- getWeatherData_folders(
  LookupWeatherFolder = file.path(path_demo, "Input"),
  weatherDirName = "data_weather",
  filebasename = "weath",
  startYear = 1979,
  endYear = 2010,
  method = "C"
)

## Read forcing weather data from files on disk (via R)
sw_weath3r <- getWeatherData_folders(
  LookupWeatherFolder = file.path(path_demo, "Input"),
  weatherDirName = "data_weather",
  filebasename = "weath",
  startYear = 1979,
  endYear = 2010,
  method = "R"
)

## Weather data (for the non-calculated variables) should be identical
identical(
  sw_weath3c[[1L]]@data[, 1:4],
  rSOILWAT2::get_WeatherHistory(sw_in3)[[1L]]@data[, 1:4]
)
identical(
  sw_weath3r[[1L]]@data[, 1:4],
  rSOILWAT2::get_WeatherHistory(sw_in3)[[1L]]@data[, 1:4]
)

## List of the slots of the input objects of class 'swWeatherData'
utils::str(sw_weath3c, max.level = 1)
utils::str(sw_weath3r, max.level = 1)

## Execute the simulation run
sw_out3c <- sw_exec(inputData = sw_in3, weatherList = sw_weath3c)
sw_out3r <- sw_exec(inputData = sw_in3, weatherList = sw_weath3r)

all.equal(sw_out3c, sw_out3r)

}
\seealso{
\code{\link{dbW_getWeatherData}}
}
