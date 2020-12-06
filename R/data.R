#' Daily weather data for a site
#'
#' A dataset containing complete daily weather values for maximum and minimum
#' air temperature and precipitation for 62 years from 1949 to 2010 for an
#' unspecified location.
#'
#' @format A list of 62 elements. Each element corresponds to one gregorian
#'  year and is an object of class \code{\linkS4class{swWeather}}. Each
#'  element contains two slots:
#'  \describe{
#'    \item{year}{gregorian calendar year}
#'    \item{data}{a matrix where rows correspond to days and columns to
#'      \describe{
#'      \item{DOY}{day of year}
#'      \item{Tmax_C}{maximum air temperature in degree Celsius}
#'      \item{Tmin_C}{minimum air temperature in degree Celsius}
#'      \item{PPT_cm}{total precipitation in centimeters}
#'    }}
#' }
#'
"weatherData"

#' Input data object for a site
#'
#' A dataset containing complete input data for an unspecified location.
#'
#' @format An object of class \code{\linkS4class{swInputData}}. Values
#'   correspond to \code{example1}; see example code.
#'
#' @examples
#' utils::data(sw_exampleData)
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#' x <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#' identical(x, sw_exampleData) ## TRUE
"sw_exampleData"


#' Default vegetation biomass table
#'
#' @format A data.frame with 12 rows (one for each month) and columns
#'   \code{X.Biomass}, \code{X.Amount.Live}, \code{X.Perc.Live}, and
#'   \code{X.Litter} where \code{X} are for the functional groups shrubs,
#'   \code{X = Sh}; C3-grasses, \code{X = C3}; C4-grasses, \code{X = C4}; and
#'   annuals, \code{X = Annual} containing default biomass values [g / m2]
#'   from Bradford et al. 2014.
#'
#' @references Bradford, J.B., Schlaepfer, D.R., Lauenroth, W.K. & Burke, I.C.
#'   (2014). Shifts in plant functional types have time-dependent and regionally
#'   variable impacts on dryland ecosystem water balance. J Ecol, 102,
#'   1408-1418.
#'
"sw2_tr_VegBiom"



#' \var{Lookup} table of rooting profiles
#'
#' @format A named list with two elements:
#' \describe{
#'   \item{desc}{
#'     A \code{data.frame} with one row. Column names are used to
#'     look-up rooting profiles and are identical to those of \code{data}. The
#'     only row are character strings with either
#'     \var{\dQuote{Layer}} or \var{\dQuote{DepthCM}} as value.
#'     \var{\dQuote{Layer}} identifies that the corresponding values in
#'     \code{data} are organized by soil layers whereas
#'     \var{\dQuote{DepthCM}} identifies that the \code{data} values
#'     represent amount of roots per centimeter soil depth.
#'   }
#'   \item{data}{
#'     A code{data.frame} with the rooting profile values.
#'   }
#' }
#'
"sw2_trco_table"
