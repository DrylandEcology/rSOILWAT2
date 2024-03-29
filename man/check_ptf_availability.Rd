% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_Pedotransfer_Functions.R
\name{check_ptf_availability}
\alias{check_ptf_availability}
\title{Check availability of \code{PTFs}}
\usage{
check_ptf_availability(ptfs = names(ptf_names()), verbose = interactive())
}
\arguments{
\item{ptfs}{A character vector. \code{PTF} names to be checked;
defaults to \code{\link[=ptf_names]{ptf_names()}}.}

\item{verbose}{A logical value.}
}
\value{
A named logical vector with current availability of \code{PTFs};
\code{PTFs} that are not implemented return \code{NA}.
}
\description{
\code{PTFs} implemented in \code{SOILWAT2} are always available;
\code{PTFs} implemented in \code{rSOILWAT2} may have additional requirements, e.g.,
live internet connection or access to specific data files.
}
\examples{
check_ptf_availability()
check_ptf_availability("neuroFX2021")
check_ptf_availability("nonexistent_PTF")

}
