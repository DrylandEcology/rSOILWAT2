% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_dbW_WeatherDatabase.R
\name{dbW_convert_to_GregorianYears}
\alias{dbW_convert_to_GregorianYears}
\title{Transfer existing weather data to a different (Gregorian) calendar (period)}
\usage{
dbW_convert_to_GregorianYears(
  weatherData,
  new_startYear = NULL,
  new_endYear = NULL,
  type = c("asis", "sequential"),
  name_year = "Year",
  name_DOY = "DOY",
  name_data = weather_dataColumns(),
  valNA = NULL
)
}
\arguments{
\item{weatherData}{A list of elements of class \code{\linkS4class{swWeatherData}}
that each hold daily weather data for one calendar year.}

\item{new_startYear}{An integer value. The first Calendar year of the new
time period. If \code{NULL}, then the first year of \code{weatherData}.}

\item{new_endYear}{An integer value. The last Calendar year of the new
time period. If \code{NULL}, then the last year of \code{weatherData}.}

\item{type}{A string that affects how years of \code{weatherData} are
used for transfer. If \code{"asis"}, then years of are used as is.
If \code{"sequential"}, then years are re-coded to start with
\code{new_startYear}.}

\item{name_year}{A string. Column name of the weather data that corresponds
to year.}

\item{name_DOY}{A string. Column name of the weather data that corresponds
to day of year.}

\item{name_data}{A vector of strings. Column names of the weather data.}

\item{valNA}{The (numerical) value of missing weather data.
If \code{NULL}, then default values are interpreted as missing.}
}
\value{
A data.frame formatted as a return object from function
  \code{\link{dbW_weatherData_to_dataframe}} with column names as given by
  \code{name_year}, \code{name_DOY}, and \code{name_data}.
}
\description{
This function can transfer from existing weather data to, e.g.,
different years / a subset of years (partially overlapping or not), or
can convert from a non-leap to a Gregorian calendar.
}
\section{Note}{
 The returned object may contain \code{NA}, e.g., for
  leap days that were added. Use function \code{\link{dbW_generateWeather}}
  to fill in.
}

\examples{
wdata <- rSOILWAT2::weatherData

## Transfer to different years (partially overlapping)
wnew <- dbW_convert_to_GregorianYears(
  wdata,
  new_startYear = 2000,
  new_endYear = 2020
)
all.equal(unique(wnew[, "Year"]), 2000:2020)
anyNA(wnew) # --> use `dbW_generateWeather`

## Transfer to a subset of years (i.e., subset)
wnew <- dbW_convert_to_GregorianYears(
  wdata,
  new_startYear = 2000,
  new_endYear = 2005
)
all.equal(unique(wnew[, "Year"]), 2000:2005)
anyNA(wnew)

## Correct/convert from a non-leap to a Gregorian calendar
wempty <- data.frame(
  dbW_weatherData_to_dataframe(weatherHistory())
)[1:365, ]

wnew <- dbW_convert_to_GregorianYears(
  wempty,
  new_startYear = 2016,
  new_endYear = 2016
)
all.equal(unique(wnew[, "Year"]), 2016:2016)
all.equal(nrow(wnew), 366) # leap year

}
