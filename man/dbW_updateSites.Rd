% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_dbW_WeatherDatabase.R
\name{dbW_updateSites}
\alias{dbW_updateSites}
\title{Updates existing sites or adds new sites to a registered weather database}
\usage{
dbW_updateSites(Site_ids, site_data, ignore.case = FALSE, verbose = FALSE)
}
\arguments{
\item{Site_ids}{An integer vector. The IDs/database keys of the queried sites}

\item{site_data}{A data.frame. The site data with column names
\code{Longitude}, \code{Latitude}, and \code{Label}.}

\item{ignore.case}{A logical value.}

\item{verbose}{A logical value.}
}
\value{
An invisible logical value indicating success with \code{TRUE} and
 failure with \code{FALSE}.
}
\description{
Updates existing sites or adds new sites to a registered weather database
}
