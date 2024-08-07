% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D_swWeatherData.R
\docType{class}
\name{swWeatherData-class}
\alias{swWeatherData-class}
\alias{swWeatherData}
\alias{weatherHistory}
\alias{swReadLines,swWeatherData,character-method}
\title{Class \code{"swWeatherData"}}
\usage{
swWeatherData(...)

weatherHistory(weatherList = NULL)

\S4method{swReadLines}{swWeatherData,character}(object, file)
}
\arguments{
\item{...}{Arguments to the helper constructor function.
Dots can either contain objects to copy into slots of that class
(must be named identical to the corresponding slot) or
be one object of that class (in which case it will be copied and
any missing slots will take their default values).
If dots are missing, then corresponding values of
\code{rSOILWAT2::sw_exampleData}
(i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.}

\item{weatherList}{A list or \code{NULL}. Each element is an object of class
\code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}
containing daily weather data of a specific year.}

\item{object}{An object of class \code{\linkS4class{swWeatherData}}.}

\item{file}{A character string. The file name from which to read.}
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\section{Slots}{

\describe{
\item{\code{year}}{An integer value. The calendar year of the weather \code{data}
object.}

\item{\code{data}}{A 365 x 15 or 366 x 15 matrix representing daily weather data for
one calendar \code{year} with columns
\var{DOY},
\var{Tmax_C}, \var{Tmin_C}, \var{PPT_cm},
\var{cloudCov_pct},
\var{windSpeed_mPERs},
\var{windSpeed_east_mPERs}, \var{windSpeed_north_mPERs},
\var{rHavg_pct}, \var{rHmax_pct}, \var{rHmin_pct},
\var{specHavg_pct}, \var{Tdewpoint_C},
\var{actVP_kPa}, and
\var{shortWR}.}
}}

\examples{
showClass("swWeatherData")
x <- new("swWeatherData")
x <- swWeatherData()

}
\seealso{
\code{\linkS4class{swInputData}}
\code{\linkS4class{swFiles}}
\code{\linkS4class{swYears}}
\code{\linkS4class{swWeather}}
\code{\linkS4class{swCloud}}
\code{\linkS4class{swMarkov}}
\code{\linkS4class{swProd}}
\code{\linkS4class{swSite}}
\code{\linkS4class{swSoils}}
\code{\linkS4class{swSpinup}}
\code{\linkS4class{swEstab}}
\code{\linkS4class{swOUT}}
\code{\linkS4class{swCarbon}}
\code{\linkS4class{swSWC}}
\code{\linkS4class{swLog}}
}
