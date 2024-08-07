% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D_swCarbon.R
\docType{class}
\name{swCarbon-class}
\alias{swCarbon-class}
\alias{swCarbon}
\alias{get_swCarbon,swCarbon-method}
\alias{swCarbon_Use_Bio,swCarbon-method}
\alias{swCarbon_Use_WUE,swCarbon-method}
\alias{swCarbon_Scenario,swCarbon-method}
\alias{swCarbon_DeltaYear,swCarbon-method}
\alias{swCarbon_CO2ppm,swCarbon-method}
\alias{set_swCarbon<-,swCarbon-method}
\alias{swCarbon_Use_Bio<-,swCarbon-method}
\alias{swCarbon_Use_WUE<-,swCarbon-method}
\alias{swCarbon_Scenario<-,swCarbon-method}
\alias{swCarbon_DeltaYear<-,swCarbon-method}
\alias{swCarbon_CO2ppm<-,swCarbon-method}
\title{Class \code{"swCarbon"}}
\usage{
swCarbon(...)

\S4method{get_swCarbon}{swCarbon}(object)

\S4method{swCarbon_Use_Bio}{swCarbon}(object)

\S4method{swCarbon_Use_WUE}{swCarbon}(object)

\S4method{swCarbon_Scenario}{swCarbon}(object)

\S4method{swCarbon_DeltaYear}{swCarbon}(object)

\S4method{swCarbon_CO2ppm}{swCarbon}(object)

\S4method{set_swCarbon}{swCarbon}(object) <- value

\S4method{swCarbon_Use_Bio}{swCarbon}(object) <- value

\S4method{swCarbon_Use_WUE}{swCarbon}(object) <- value

\S4method{swCarbon_Scenario}{swCarbon}(object) <- value

\S4method{swCarbon_DeltaYear}{swCarbon}(object) <- value

\S4method{swCarbon_CO2ppm}{swCarbon}(object) <- value
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

\item{object}{An object of class \code{\linkS4class{swCarbon}}.}

\item{value}{A value to assign to a specific slot of the object.}
}
\description{
Class \code{swCarbon} defines variables that allow \code{SOILWAT2} to
simulate the effects of atmospheric carbon dioxide.
}
\section{Slots}{

\describe{
\item{\code{CarbonUseBio}}{Object of class \code{"integer"}, where a value of 1
enables the \var{CO2} biomass multiplier.}

\item{\code{CarbonUseWUE}}{Object of class \code{"integer"}, where a value of 1
enables the \var{CO2} water-use efficiency \var{WUE} multiplier.}

\item{\code{Scenario}}{Object of class \code{"character"}, that represents the name
of the scenario that is being simulated. This slot is not used in
\pkg{rSOILWAT2}, but it's useful to see what scenario was used in the
\pkg{SOILWAT2} input object.}

\item{\code{DeltaYear}}{Object of class \code{"integer"}, that represents the number
of years in the future that this simulation is being run.}

\item{\code{CO2ppm}}{Object of class \code{"matrix"}, that holds years in the first
column and CO2 ppm concentrations in the second column.}
}}

\examples{
showClass("swCarbon")
x <- new("swCarbon")
x <- swCarbon()

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
