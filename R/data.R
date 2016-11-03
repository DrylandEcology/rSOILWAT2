#' Daily weather data for a site
#'
#' A dataset containing complete daily weather values for maximum and minimum air temperature
#'  and precipitation for 62 years from 1949 to 2010 for a unspecified location.
#'
#' @format A list of 62 elements. Each element corresponds to one gregorian year and
#'  is an object of class \code{\linkS4class{swWeather}}. Each element contains two slots:
#'  \describe{
#'    \item{year}{gregorian calendar year}
#'    \item{data}{a matrix where rows correspond to days and columns to \describe{
#'      \item{DOY}{day of year}
#'      \item{Tmax_C}{maximum air temperature in degree Celsius}
#'      \item{Tmin_C}{minimum air temperature in degree Celsius}
#'      \item{PPT_cm}{total precipitation in centimeters}
#'    }}
#' }
#'
"weatherData"
