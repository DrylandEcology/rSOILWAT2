###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018)
###############################################################################


###############################################################SITE############
#' Class \code{"swSite"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swSite}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso
#' \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}}
#' \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}}
#' \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}}
#' \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swSpinup}}
#' \code{\linkS4class{swEstab}}
#' \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swCarbon}}
#' \code{\linkS4class{swSWC}}
#' \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swSite")
#' x <- new("swSite")
#' x <- swSite()
#'
#' @name swSite-class
#' @export
setClass(
  "swSite",
  slots = c(
    SWClimits = "numeric",
    ModelFlags = "logical",
    ModelCoefficients = "numeric",
    SnowSimulationParameters = "numeric",
    DrainageCoefficient = "numeric",
    EvaporationCoefficients = "numeric",
    TranspirationCoefficients = "numeric",
    IntrinsicSiteParams = "numeric",
    SurfaceTemperatureMethod = "integer",
    SoilTemperatureFlag = "logical",
    SoilTemperatureConstants = "numeric",
    SoilDensityInputType = "integer",
    TranspirationRegions = "matrix",
    swrc_flags = "character",
    has_swrcp = "logical",
    depth_sapric = "numeric"
  ),
  prototype = list(
    SWClimits = c(swc_min = NA_real_, swc_init = NA_real_, swc_wet = NA_real_),
    ModelFlags = c(Reset = NA, DeepDrain = NA),
    ModelCoefficients = c(
      PETmultiplier = NA_real_,
      DailyRunoff = NA_real_,
      DailyRunon = NA_real_
    ),
    SnowSimulationParameters = stats::setNames(
      rep(NA_real_, 5L),
      c("TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax")
    ),
    DrainageCoefficient = c("SlowDrainCoefficientPerYear_cm/dy" = NA_real_),
    EvaporationCoefficients = stats::setNames(
      rep(NA_real_, 4L),
      c("RateShift", "RateSlope", "InflectionPoint", "Range")
    ),
    TranspirationCoefficients = stats::setNames(
      rep(NA_real_, 4L),
      c("RateShift", "RateShape", "InflectionPoint", "Range")
    ),
    IntrinsicSiteParams = stats::setNames(
      rep(NA_real_, 5L),
      c("Longitude", "Latitude", "Altitude", "Slope", "Aspect")
    ),
    SurfaceTemperatureMethod = NA_integer_,
    SoilTemperatureFlag = NA,
    SoilTemperatureConstants = stats::setNames(
      rep(NA_real_, 10L),
      c(
        "BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b", "T1constant_c",
        "cs_constant_SoilThermCondct", "cs_constant",
        "sh_constant_SpecificHeatCapacity",
        "ConstMeanAirTemp", "deltaX_Param", "MaxDepth"
      )
    ),
    SoilDensityInputType = NA_integer_,
    TranspirationRegions = array(
      NA_integer_,
      dim = c(3L, 2L),
      dimnames = list(NULL, c("ndx", "layer"))
    ),
    swrc_flags = c(swrc_name = NA_character_, ptf_name = NA_character_),
    has_swrcp = NA,
    depth_sapric = NA_real_
  )
)

setValidity(
  "swSite",
  function(object) {
    val <- TRUE

    if (length(object@SWClimits) != 3L) {
      msg <- "@SWClimits length != 3."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@ModelFlags) != 2L) {
      msg <- "@ModelFlags length != 2."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@ModelCoefficients) != 3L) {
      msg <- "@ModelCoefficients length != 3."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@SnowSimulationParameters) != 5L) {
      msg <- "@SnowSimulationParameters length != 5."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@DrainageCoefficient) != 1L) {
      msg <- "@DrainageCoefficient length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@EvaporationCoefficients) != 4L) {
      msg <- "@EvaporationCoefficients length != 4."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@TranspirationCoefficients) != 4L) {
      msg <- "@TranspirationCoefficients length != 4."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@IntrinsicSiteParams) != 5L) {
      msg <- "@IntrinsicSiteParams length != 5."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@SurfaceTemperatureMethod) != 1L) {
      msg <- "@SurfaceTemperatureMethod length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@SoilTemperatureFlag) != 1L) {
      msg <- "@SoilTemperatureFlag length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@SoilTemperatureConstants) != 10L) {
      msg <- "@SoilTemperatureConstants length != 10."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (length(object@SoilDensityInputType) != 1L) {
      msg <- "@SoilDensityInputType length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (NCOL(object@TranspirationRegions) != 2L) {
      msg <- "@TranspirationRegions columns != 2."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (typeof(object@TranspirationRegions) != "integer") {
      msg <- "@TranspirationRegions must be integers."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@swrc_flags) != 2L) {
      msg <- "@swrc_flags length != 2."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@has_swrcp) != 1L) {
      msg <- "@has_swrcp length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@depth_sapric) != 1L) {
      msg <- "@depth_sapric length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swSite-class
#' @export
swSite <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "site")
  sns <- slotNames("swSite")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSite")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slots `IntrinsicSiteParams` and
  # `TranspirationRegions`; this is to prevent simulation runs with
  # accidentally incorrect values
  if (!("IntrinsicSiteParams" %in% dns)) {
    tmp <- c("Longitude", "Latitude", "Altitude", "Slope", "Aspect")
    def@IntrinsicSiteParams[tmp] <- NA_real_
  }
  if ("TranspirationRegions" %in% dns) {
    # Guarantee names
    dimnames(dots[["TranspirationRegions"]]) <- list(
      NULL,
      colnames(def@TranspirationRegions)
    )
  } else {
    def@TranspirationRegions[, "layer"] <- NA_integer_
  }

  if ("swrc_flags" %in% dns) {
    # Guarantee names
    names(dots[["swrc_flags"]]) <- names(def@swrc_flags)
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSite", tmp))
}



#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "swSite",
  definition = function(object, verbose = FALSE) {
    tmp <- try(validObject(object), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swSite`.")
      }
      object <- suppressWarnings(swSite(object))
    }

    object
  }
)


#' @rdname swSite-class
#' @export
setMethod("get_swSite", "swSite", function(object) object)

#' @rdname swSite_SWRCflags
setMethod(
  "swSite_SWRCflags",
  signature = "swSite",
  function(object) slot(object, "swrc_flags")
)

#' @rdname swSite_hasSWRCp
setMethod(
  "swSite_hasSWRCp",
  signature = "swSite",
  function(object) slot(object, "has_swrcp")
)

#' @rdname swSite_depthSapric
setMethod(
  "swSite_depthSapric",
  signature = "swSite",
  function(object) slot(object, "depth_sapric")
)


#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SWClimits",
  "swSite",
  function(object) slot(object, "SWClimits")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_ModelFlags",
  "swSite",
  function(object) slot(object, "ModelFlags")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_ModelCoefficients",
  "swSite",
  function(object) slot(object, "ModelCoefficients")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SnowSimulationParams",
  "swSite",
  function(object) slot(object, "SnowSimulationParameters")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_DrainageCoefficient",
  "swSite",
  function(object) slot(object, "DrainageCoefficient")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_EvapCoefficients",
  "swSite",
  function(object) slot(object, "EvaporationCoefficients")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_TranspCoefficients",
  "swSite",
  function(object) slot(object, "TranspirationCoefficients")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_IntrinsicSiteParams",
  "swSite",
  function(object) slot(object, "IntrinsicSiteParams")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SurfaceTempMethod",
  "swSite",
  function(object) slot(object, "SurfaceTemperatureMethod")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SoilTemperatureFlag",
  "swSite",
  function(object) slot(object, "SoilTemperatureFlag")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SoilTemperatureConsts",
  "swSite",
  function(object) slot(object, "SoilTemperatureConstants")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_TranspirationRegions",
  "swSite",
  function(object) slot(object, "TranspirationRegions")
)

#' @rdname swSite-class
#' @export
setMethod(
  "swSite_SoilDensityInputType",
  "swSite",
  function(object) slot(object, "SoilDensityInputType")
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "set_swSite",
  signature = "swSite",
  definition = function(object, value) {
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swSite_SWRCflags
setReplaceMethod(
  "swSite_SWRCflags",
  signature = "swSite",
  definition = function(object, value) {
    object@swrc_flags[] <- as.character(value)
    validObject(object)
    object
  }
)


#' @rdname swSite_hasSWRCp
setReplaceMethod(
  "swSite_hasSWRCp",
  signature = "swSite",
  definition = function(object, value) {
    object@has_swrcp <- isTRUE(as.logical(value))
    validObject(object)
    object
  }
)

#' @rdname swSite_depthSapric
setReplaceMethod(
  "swSite_depthSapric",
  signature = "swSite",
  definition = function(object, value) {
    object@has_swrcp <- isTRUE(as.logical(value))
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SWClimits",
  signature = "swSite",
  definition = function(object, value) {
    object@SWClimits[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_ModelFlags",
  signature = "swSite",
  definition = function(object, value) {
    object@ModelFlags[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_ModelCoefficients",
  signature = "swSite",
  definition = function(object, value) {
    object@ModelCoefficients[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SnowSimulationParams",
  signature = "swSite",
  definition = function(object, value) {
    object@SnowSimulationParameters[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_DrainageCoefficient",
  signature = "swSite",
  definition = function(object, value) {
    object@DrainageCoefficient[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_EvapCoefficients",
  signature = "swSite",
  definition = function(object, value) {
    object@EvaporationCoefficients[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_TranspCoefficients",
  signature = "swSite",
  definition = function(object, value) {
    object@TranspirationCoefficients[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_IntrinsicSiteParams",
  signature = "swSite",
  definition = function(object, value) {
    object@IntrinsicSiteParams[] <- value
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SurfaceTempMethod",
  signature = "swSite",
  definition = function(object, value) {
    object@SurfaceTemperatureMethod <- as.integer(value)
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SoilTemperatureFlag",
  signature = "swSite",
  definition = function(object, value) {
    object@SoilTemperatureFlag <- as.logical(value)
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SoilTemperatureConsts",
  signature = "swSite",
  definition = function(object, value) {
    object@SoilTemperatureConstants[] <- value
    validObject(object)
    object
})

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_SoilDensityInputType",
  signature = "swSite",
  definition = function(object, value) {
    object@SoilDensityInputType <- as.integer(value[1L])
    validObject(object)
    object
  }
)

#' @rdname swSite-class
#' @export
setReplaceMethod(
  "swSite_TranspirationRegions",
  signature = "swSite",
  definition = function(object, value) {
    if (typeof(value) != "integer") {
      # Check whether we can convert to integer without great loss of info
      x <- as.integer(value)
      d <- sum(abs(value[] - x))
      if (d < rSW2_glovars[["tol"]]) {
        # We can convert without great loss
        value <- array(x, dim = dim(value))
      }
      # otherwise, we copy non-integer values which will trigger `validObject`
    }
    object@TranspirationRegions <- array(
      as.integer(value),
      dim = dim(value),
      dimnames = list(NULL, colnames(object@TranspirationRegions))
    )
    validObject(object)
    object
  }
)



#' @rdname swSite-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object="swSite",file="character"),
  function(object,file) {
    print("TODO: method 'swReadLines' is not up-to-date; hard-coded indices are incorrect")
			infiletext <- readLines(con = file)
			object@SWClimits[1] = readNumeric(infiletext[2])
			object@SWClimits[2] = readNumeric(infiletext[3])
			object@SWClimits[3] = readNumeric(infiletext[4])
			object@ModelFlags[1] = readLogical(infiletext[7])
			object@ModelFlags[2] = readLogical(infiletext[8])
			object@ModelCoefficients[1] = readNumeric(infiletext[10])
			object@ModelCoefficients[2] = readNumeric(infiletext[11])
			object@ModelCoefficients[3] = readNumeric(infiletext[12])
			object@SnowSimulationParameters[1] = readNumeric(infiletext[15])
			object@SnowSimulationParameters[2] = readNumeric(infiletext[16])
			object@SnowSimulationParameters[3] = readNumeric(infiletext[17])
			object@SnowSimulationParameters[4] = readNumeric(infiletext[18])
			object@SnowSimulationParameters[5] = readNumeric(infiletext[19])
			object@DrainageCoefficient = readNumeric(infiletext[22])
			object@EvaporationCoefficients[1] = readNumeric(infiletext[30])
			object@EvaporationCoefficients[2] = readNumeric(infiletext[33])
			object@EvaporationCoefficients[3] = readNumeric(infiletext[35])
			object@EvaporationCoefficients[4] = readNumeric(infiletext[36])
			object@TranspirationCoefficients[1] = readNumeric(infiletext[40])
			object@TranspirationCoefficients[2] = readNumeric(infiletext[41])
			object@TranspirationCoefficients[3] = readNumeric(infiletext[42])
			object@TranspirationCoefficients[4] = readNumeric(infiletext[43])
			object@IntrinsicSiteParams[1] = readNumeric(infiletext[46])
			object@IntrinsicSiteParams[2] = readNumeric(infiletext[47])
			object@IntrinsicSiteParams[3] = readNumeric(infiletext[48])
			object@IntrinsicSiteParams[4] = readNumeric(infiletext[49])
			object@SoilTemperatureConstants[1] = readNumeric(infiletext[53])
			object@SoilTemperatureConstants[2] = readNumeric(infiletext[54])
			object@SoilTemperatureConstants[3] = readNumeric(infiletext[55])
			object@SoilTemperatureConstants[4] = readNumeric(infiletext[56])
			object@SoilTemperatureConstants[5] = readNumeric(infiletext[57])
			object@SoilTemperatureConstants[6] = readNumeric(infiletext[58])
			object@SoilTemperatureConstants[7] = readNumeric(infiletext[59])
			object@SoilTemperatureConstants[8] = readNumeric(infiletext[60])
			object@SoilTemperatureConstants[9] = readNumeric(infiletext[61])
			object@SoilTemperatureConstants[10] = readNumeric(infiletext[62])
			object@SoilTemperatureFlag = readLogical(infiletext[63])

			for(i in 71:length(infiletext)) {
				if(grepl(pattern="#", infiletext[i]))
					infiletext=infiletext[-i]
			}

			data = matrix(NA,nrow=length(71:length(infiletext)),ncol=2)
			colnames(data)<-c("ndx", "layer")
			for(i in 71:length(infiletext)) {
				if(!grepl(pattern="#", infiletext[i]))
					data[i-70,] = readNumerics(infiletext[i],2)
			}
			object@TranspirationRegions = data
			return(object)
})
# nolint end
