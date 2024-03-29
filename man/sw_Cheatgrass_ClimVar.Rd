% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rSOILWAT2_deprecated.R
\name{sw_Cheatgrass_ClimVar}
\alias{sw_Cheatgrass_ClimVar}
\title{Calculate climate variables required to estimate percent cheatgrass cover
in North America}
\usage{
sw_Cheatgrass_ClimVar(
  monthlyPPT_cm,
  monthlyTempMean_C = NULL,
  monthlyTempMin_C = NULL
)
}
\arguments{
\item{monthlyPPT_cm}{A numeric matrix of monthly precipitation values in
centimeter. There are 12 rows, one for each month of the year;
and there is one column for each year.}

\item{monthlyTempMean_C}{A numeric matrix of monthly mean temperature values
in degree Celsius. There are 12 rows, one for each month of the year;
and there is one column for each year.}

\item{monthlyTempMin_C}{A numeric matrix of monthly minimum temperature
value sin degree Celsius. There are 12 rows, one for each month of the
year; and there is one column for each year.}
}
\value{
A named numeric vector of length 6 with mean and standard deviation
  for \var{Month7th_PPT_mm}, \var{MeanTemp_ofDriestQuarter_C}, and
  \var{MinTemp_of2ndMonth_C}.
}
\description{
Calculate climate variables required to estimate percent cheatgrass cover
in North America
}
\section{Note}{
 This function does not correct for northern/southern
  hemisphere.
}

\references{
Brummer, T. J., K. T. Taylor, J. Rotella, B. D. Maxwell,
  L. J. Rew, and M. Lavin. 2016. Drivers of Bromus tectorum Abundance in
  the Western North American Sagebrush Steppe. Ecosystems 19:986-1000.
}
