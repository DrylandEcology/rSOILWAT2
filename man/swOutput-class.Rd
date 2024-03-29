% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/L_swOutput.R
\docType{class}
\name{swOutput-class}
\alias{swOutput-class}
\alias{swOutput}
\alias{$,swOutput-method}
\alias{swOutput_getKEY,swOutput-method}
\alias{swOutput_setKEY<-,swOutput,ANY,swOutput_KEY-method}
\title{Class \code{"swOutput"}}
\usage{
\S4method{$}{swOutput}(x, name)

\S4method{swOutput_getKEY}{swOutput}(object, index)

\S4method{swOutput_setKEY}{swOutput,ANY,swOutput_KEY}(object, index) <- value
}
\arguments{
\item{x}{An object of class \code{\linkS4class{swOutput}}.}

\item{name}{A character string. The name of the of the slots of this class.}

\item{object}{An object of class \code{\linkS4class{swOutput}}.}

\item{index}{An integer value. One of the four possible time steps.}

\item{value}{A value to assign to a specific slot of the object.}
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\examples{
showClass("swOutput")
x <- new("swOutput")

}
\seealso{
\code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
\code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
\code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
\code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
\code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
\code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
}
