% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_Vegetation.R
\name{check_TranspirationRegions}
\alias{check_TranspirationRegions}
\title{Check transpiration regions}
\usage{
check_TranspirationRegions(swInputData)
}
\arguments{
\item{swInputData}{A \pkg{rSOILWAT2} input object of class
\code{\linkS4class{swInputData}}.}
}
\value{
A logical value.
}
\description{
The transpiration regions are checked:
\enumerate{
 \item There is least one transpiration region
 \item All transpiration regions include at least one soil layer
 \item Transpiration regions are strictly increasing
 \item Transpiration regions go no deeper than the most shallow
      rooting profile of any active vegetation type
}
}
\examples{
sw_in <- rSOILWAT2::sw_exampleData
check_TranspirationRegions(sw_in) ## Expected: TRUE

# Make a mistake: set a transpiration region deeper than the rooting profile
swSite_TranspirationRegions(sw_in)[2, 2] <- 10
check_TranspirationRegions(sw_in) ## Expected: FALSE

}
