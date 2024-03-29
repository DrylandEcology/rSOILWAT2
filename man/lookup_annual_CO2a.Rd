% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_Miscellaneous_Functions.R
\name{lookup_annual_CO2a}
\alias{lookup_annual_CO2a}
\title{\var{Look-up} yearly atmospheric CO2 concentration values}
\usage{
lookup_annual_CO2a(start, end, name_co2, tr_CO2a = rSOILWAT2::sw2_tr_CO2a)
}
\arguments{
\item{start}{An integer value. First year for which to \var{look-up} values.}

\item{end}{An integer value. Last year for which to \var{look-up} values.}

\item{name_co2}{A character string. The (partial) name of the CO2 series,
i.e., a column name of \code{tr_CO2a}. See details.}

\item{tr_CO2a}{A numeric \code{data.frame} with the CO2 values [ppm].
Default values are taken from \code{\link{sw2_tr_CO2a}}.}
}
\description{
\var{Look-up} yearly atmospheric CO2 concentration values
}
\section{Details}{
 \code{name_co2} may contain multiple data set names,
  either as vector of strings, or as names separated by "|".
  Values of the first match are used;
  any missing values are filled in from the second matching column,
  and so forth.
}

\examples{
lookup_annual_CO2a(start = 1765, end = 2300, name_co2 = "RCP45")
lookup_annual_CO2a(start = 1765, end = 2300, name_co2 = "CMIP5_RCP45")
lookup_annual_CO2a(start = 1980, end = 2005, name_co2 = "CMIP5_historical")
lookup_annual_CO2a(
  start = 1980,
  end = 2021,
  name_co2 = c("CMIP6_historical", "CMIP6_SSP119")
)
lookup_annual_CO2a(
  start = 1980,
  end = 2021,
  name_co2 = "CMIP6_historical|CMIP6_SSP119"
)

\dontrun{
## This fails because "CMIP5_historical" has no values after 2005
lookup_annual_CO2a(start = 1980, end = 2020, name_co2 = "CMIP5_historical")
}

}
\seealso{
\code{\link{sw2_tr_CO2a}} for description of data
}
