% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/J_swLog.R
\docType{class}
\name{swLog-class}
\alias{swLog-class}
\alias{swLog}
\title{Class \code{"swLog"}}
\usage{
swLog(...)
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
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\examples{
showClass("swLog")
x <- new("swLog")
x <- swLog()

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
