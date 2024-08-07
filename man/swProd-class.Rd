% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/E_swProd.R
\docType{class}
\name{swProd-class}
\alias{swProd-class}
\alias{swProd}
\alias{get_swProd,swProd-method}
\alias{swProd_Composition,swProd-method}
\alias{swProd_Albedo,swProd-method}
\alias{swProd_CanopyHeight,swProd-method}
\alias{swProd_VegInterParam,swProd-method}
\alias{swProd_LitterInterParam,swProd-method}
\alias{swProd_EsTpartitioning_param,swProd-method}
\alias{swProd_Es_param_limit,swProd-method}
\alias{swProd_Shade,swProd-method}
\alias{swProd_HydrRedstro_use,swProd-method}
\alias{swProd_HydrRedstro,swProd-method}
\alias{swProd_CritSoilWaterPotential,swProd-method}
\alias{swProd_CO2Coefficients,swProd-method}
\alias{swProd_MonProd_veg,swProd,numeric-method}
\alias{swProd_MonProd_veg,swProd,character-method}
\alias{swProd_MonProd_grass,swProd-method}
\alias{swProd_MonProd_shrub,swProd-method}
\alias{swProd_MonProd_tree,swProd-method}
\alias{swProd_MonProd_forb,swProd-method}
\alias{set_swProd<-,swProd-method}
\alias{swProd_Composition<-,swProd-method}
\alias{swProd_Albedo<-,swProd-method}
\alias{swProd_CanopyHeight<-,swProd-method}
\alias{swProd_VegInterParam<-,swProd-method}
\alias{swProd_LitterInterParam<-,swProd-method}
\alias{swProd_EsTpartitioning_param<-,swProd-method}
\alias{swProd_Es_param_limit<-,swProd-method}
\alias{swProd_Shade<-,swProd-method}
\alias{swProd_HydrRedstro_use<-,swProd-method}
\alias{swProd_HydrRedstro<-,swProd-method}
\alias{swProd_CritSoilWaterPotential<-,swProd-method}
\alias{swProd_CO2Coefficients<-,swProd-method}
\alias{swProd_MonProd_veg<-,swProd,numeric,matrix-method}
\alias{swProd_MonProd_veg<-,swProd,character,matrix-method}
\alias{swProd_MonProd_grass<-,swProd-method}
\alias{swProd_MonProd_shrub<-,swProd-method}
\alias{swProd_MonProd_tree<-,swProd-method}
\alias{swProd_MonProd_forb<-,swProd-method}
\alias{swReadLines,swProd,character-method}
\title{Class \code{"swProd"}}
\usage{
swProd(...)

\S4method{get_swProd}{swProd}(object)

\S4method{swProd_Composition}{swProd}(object)

\S4method{swProd_Albedo}{swProd}(object)

\S4method{swProd_CanopyHeight}{swProd}(object)

\S4method{swProd_VegInterParam}{swProd}(object)

\S4method{swProd_LitterInterParam}{swProd}(object)

\S4method{swProd_EsTpartitioning_param}{swProd}(object)

\S4method{swProd_Es_param_limit}{swProd}(object)

\S4method{swProd_Shade}{swProd}(object)

\S4method{swProd_HydrRedstro_use}{swProd}(object)

\S4method{swProd_HydrRedstro}{swProd}(object)

\S4method{swProd_CritSoilWaterPotential}{swProd}(object)

\S4method{swProd_CO2Coefficients}{swProd}(object)

\S4method{swProd_MonProd_veg}{swProd,numeric}(object, vegtype)

\S4method{swProd_MonProd_veg}{swProd,character}(object, vegtype)

\S4method{swProd_MonProd_grass}{swProd}(object)

\S4method{swProd_MonProd_shrub}{swProd}(object)

\S4method{swProd_MonProd_tree}{swProd}(object)

\S4method{swProd_MonProd_forb}{swProd}(object)

\S4method{set_swProd}{swProd}(object) <- value

\S4method{swProd_Composition}{swProd}(object) <- value

\S4method{swProd_Albedo}{swProd}(object) <- value

\S4method{swProd_CanopyHeight}{swProd}(object) <- value

\S4method{swProd_VegInterParam}{swProd}(object) <- value

\S4method{swProd_LitterInterParam}{swProd}(object) <- value

\S4method{swProd_EsTpartitioning_param}{swProd}(object) <- value

\S4method{swProd_Es_param_limit}{swProd}(object) <- value

\S4method{swProd_Shade}{swProd}(object) <- value

\S4method{swProd_HydrRedstro_use}{swProd}(object) <- value

\S4method{swProd_HydrRedstro}{swProd}(object) <- value

\S4method{swProd_CritSoilWaterPotential}{swProd}(object) <- value

\S4method{swProd_CO2Coefficients}{swProd}(object) <- value

\S4method{swProd_MonProd_veg}{swProd,numeric,matrix}(object, vegtype) <- value

\S4method{swProd_MonProd_veg}{swProd,character,matrix}(object, vegtype) <- value

\S4method{swProd_MonProd_grass}{swProd}(object) <- value

\S4method{swProd_MonProd_shrub}{swProd}(object) <- value

\S4method{swProd_MonProd_tree}{swProd}(object) <- value

\S4method{swProd_MonProd_forb}{swProd}(object) <- value

\S4method{swReadLines}{swProd,character}(object, file)
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

\item{object}{An object of class \code{\linkS4class{swProd}}.}

\item{vegtype}{The name or index of the vegetation type.}

\item{value}{A value to assign to a specific slot of the object.}

\item{file}{A character string. The file name from which to read.}
}
\description{
The methods listed below work on this class and the proper slot of the class
  \code{\linkS4class{swInputData}}.
}
\examples{
showClass("swProd")
x <- new("swProd")
x <- swProd()

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
