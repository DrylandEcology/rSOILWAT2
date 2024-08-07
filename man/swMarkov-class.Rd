% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D_swMarkov.R
\docType{class}
\name{swMarkov-class}
\alias{swMarkov-class}
\alias{swMarkov}
\alias{get_Markov,swMarkov-method}
\alias{get_swMarkov,swMarkov-method}
\alias{swMarkov_Prob,swMarkov-method}
\alias{swMarkov_Conv,swMarkov-method}
\alias{set_Markov<-,swMarkov-method}
\alias{set_swMarkov<-,swMarkov-method}
\alias{swMarkov_Prob<-,swMarkov-method}
\alias{swMarkov_Conv<-,swMarkov-method}
\alias{swReadLines,swMarkov,character-method}
\title{Class \code{"swMarkov"}}
\usage{
swMarkov(...)

\S4method{get_Markov}{swMarkov}(object)

\S4method{get_swMarkov}{swMarkov}(object)

\S4method{swMarkov_Prob}{swMarkov}(object)

\S4method{swMarkov_Conv}{swMarkov}(object)

\S4method{set_Markov}{swMarkov}(object) <- value

\S4method{set_swMarkov}{swMarkov}(object) <- value

\S4method{swMarkov_Prob}{swMarkov}(object) <- value

\S4method{swMarkov_Conv}{swMarkov}(object) <- value

\S4method{swReadLines}{swMarkov,character}(object, file)
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

\item{object}{An object of class \code{\linkS4class{swMarkov}}.}

\item{value}{A value to assign to a specific slot of the object.}

\item{file}{A character string. The file name from which to read.}
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\examples{
showClass("swMarkov")
x <- new("swMarkov")
x <- swMarkov()

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
