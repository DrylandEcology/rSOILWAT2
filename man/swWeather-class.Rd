% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D_swWeather.R
\docType{class}
\name{swWeather-class}
\alias{swWeather-class}
\alias{swWeather}
\alias{swWeather_DaysRunningAverage,swWeather-method}
\alias{swWeather_FirstYearHistorical,swWeather-method}
\alias{swWeather_pct_SnowDrift,swWeather-method}
\alias{swWeather_pct_SnowRunoff,swWeather-method}
\alias{swWeather_UseMarkov,swWeather-method}
\alias{swWeather_UseMarkovOnly,swWeather-method}
\alias{swWeather_UseSnow,swWeather-method}
\alias{swWeather_MonScalingParams,swWeather-method}
\alias{swWeather_DaysRunningAverage<-,swWeather-method}
\alias{swWeather_FirstYearHistorical<-,swWeather-method}
\alias{swWeather_pct_SnowDrift<-,swWeather-method}
\alias{swWeather_pct_SnowRunoff<-,swWeather-method}
\alias{swWeather_UseMarkov<-,swWeather-method}
\alias{swWeather_UseMarkovOnly<-,swWeather-method}
\alias{swWeather_UseSnow<-,swWeather-method}
\alias{swWeather_MonScalingParams<-,swWeather-method}
\alias{swReadLines,swWeather,character-method}
\title{Class \code{"swWeather"}}
\usage{
swWeather(...)

\S4method{swWeather_DaysRunningAverage}{swWeather}(object)

\S4method{swWeather_FirstYearHistorical}{swWeather}(object)

\S4method{swWeather_pct_SnowDrift}{swWeather}(object)

\S4method{swWeather_pct_SnowRunoff}{swWeather}(object)

\S4method{swWeather_UseMarkov}{swWeather}(object)

\S4method{swWeather_UseMarkovOnly}{swWeather}(object)

\S4method{swWeather_UseSnow}{swWeather}(object)

\S4method{swWeather_MonScalingParams}{swWeather}(object)

\S4method{swWeather_DaysRunningAverage}{swWeather}(object) <- value

\S4method{swWeather_FirstYearHistorical}{swWeather}(object) <- value

\S4method{swWeather_pct_SnowDrift}{swWeather}(object) <- value

\S4method{swWeather_pct_SnowRunoff}{swWeather}(object) <- value

\S4method{swWeather_UseMarkov}{swWeather}(object) <- value

\S4method{swWeather_UseMarkovOnly}{swWeather}(object) <- value

\S4method{swWeather_UseSnow}{swWeather}(object) <- value

\S4method{swWeather_MonScalingParams}{swWeather}(object) <- value

\S4method{swReadLines}{swWeather,character}(object, file)
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

\item{object}{An object of class \code{\linkS4class{swWeather}}.}

\item{value}{A value to assign to a specific slot of the object.}

\item{file}{A character string. The file name from which to read.}
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\examples{
showClass("swWeather")
x <- new("swWeather")
x <- swWeather()

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
