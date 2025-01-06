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
#' @section Notes: Using \code{sw_exampleData} as basis for creating a
#'   site-specific simulation run is discouraged (even though there are many
#'   such examples throughout the documentation of this package).
#'   The recommended approach is to create a clean new object with
#'   the helper constructor \code{swInputData()} (or based on the prototype
#'   \code{new("swInputData")}) and then set all site-specific inputs and
#'   parameters. See \var{\dQuote{rSOILWAT2_demo}} vignette.
#'
#' @examples
#' utils::data(sw_exampleData)
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#' x <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#' ## expect differences in slots "timestamp" and possibly "version"
#' all.equal(x, sw_exampleData)
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
#'     A \code{data.frame} with the rooting profile values.
#'   }
#' }
#'
"sw2_trco_table"



#' \var{Lookup} table of yearly atmospheric CO2 concentrations
#'
#' @format A \code{data.frame}. The first column contains calendar years. The
#'   remaining columns hold values of global average, annual
#'   atmospheric CO2 concentrations in \[ppm\] for historical observations and
#'   projected values under different scenarios. Currently available values are
#'   from Meinshausen et al. 2011 for \var{CMIP5} and Meinshausen et
#'   al. 2020 for \var{CMIP6}:
#'
# nolint start: line_length_linter.
#'   | Name             | Years     | Published source data file |
#'   | ---------------- | --------- | ------------------- |
#'   | CMIP5_historical | 1765-2005 | \var{20THCENTURY_MIDYEAR_CONCENTRATIONS.xls} |
#'   | CMIP5_RCP3PD     | 1765-2500 | \var{RCP3PD_MIDYEAR_CONCENTRATIONS.xls} |
#'   | CMIP5_RCP45      | 1765-2500 | \var{RCP45_MIDYEAR_CONCENTRATIONS.xls} |
#'   | CMIP5_RCP60      | 1765-2500 | \var{RCP6_MIDYEAR_CONCENTRATIONS.xls} |
#'   | CMIP5_RCP85      | 1765-2500 | \var{RCP85_MIDYEAR_CONCENTRATIONS.xls} |
#'   | CMIP6_historical | 1750-2014 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP119     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP126     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP245     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP370     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP434     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP460     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'   | CMIP6_SSP585     | 2015-2500 | \var{SUPPLEMENT_DataTables_Meinshausen_6May2020.xlsx} |
#'
# nolint end
#'
#' @seealso \code{\link{lookup_annual_CO2a}} to extract specific values.
#'
#' @references Meinshausen, M., S. J. Smith, K. V. Calvin, J. S. Daniel, M. L.
#'   T. Kainuma, J.-F. Lamarque, K. Matsumoto, S. A. Montzka, S. C. B. Raper, K.
#'   Riahi, A. M. Thomson, G. J. M. Velders and D. van Vuuren (2011). The RCP
#'   Greenhouse Gas Concentrations and their extension from 1765 to 2300.
#'   Climatic Change (Special Issue), \doi{10.1007/s10584-011-0156-z}.
#'   Data tables downloaded from \url{http://www.pik-potsdam.de/~mmalte/rcps/}.
#'
#' @references Meinshausen, M., Z. R. J. Nicholls, J. Lewis, M. J. Gidden, E.
#'   Vogel, M. Freund, U. Beyerle, C. Gessner, A. Nauels, N. Bauer, J. G.
#'   Canadell, J. S. Daniel, A. John, P. B. Krummel, G. Luderer, N. Meinshausen,
#'   S. A. Montzka, P. J. Rayner, S. Reimann, S. J. Smith, M. van den Berg, G.
#'   J. M. Velders, M. K. Vollmer, and R. H. J. Wang. 2020. The shared
#'   socio-economic pathway (SSP) greenhouse gas concentrations and their
#'   extensions to 2500. Geoscientific Model Development 13:3571-3605.
#'   \doi{10.5194/gmd-13-3571-2020}.
#'   Data accessed from supplemental tables.
#'
#' @md
"sw2_tr_CO2a"

#' Parameters of fibric and sapric peat for implemented `SWRCs`
#'
#' @format A named list with a matrix for each implemented
#' soil water retention curve (`SWRC`)
#'    * Columns represent the `SWRC` parameters
#'    * Rows represent (1) fibric peat and (2) sapric peat
#'
#' @examples
#'   swin <- rSOILWAT2::sw_exampleData
#'   swSoils_omSWRCp(swin) <- rSOILWAT2::sw2_list_omSWRCp[["Campbell1974"]]
#'
#' @md
"sw2_list_omSWRCp"
