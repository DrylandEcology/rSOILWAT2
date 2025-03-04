###############################################################################
#rSOILWAT2
#    Copyright (C) 2009-2018  Ryan Murphy, Daniel Schlaepfer, William Lauenroth,
#    John Bradford
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


# TODO: Add comment
#
# Author: Ryan J. Murphy (2013)
###############################################################################

##########################GENERIC METHODS/FUNCTIONS############################
#' \code{swReadLines}
#' @param object An object of a class such \code{\linkS4class{swInputData}}.
#' @param file A character string. The file path.
#' @seealso \code{\linkS4class{swInputData}}
setGeneric("swReadLines", function(object, file) standardGeneric("swReadLines"))


#' Create \pkg{rSOILWAT2} version representation
rSW2_version <- function() {
  as.character(as.numeric_version(getNamespaceVersion("rSOILWAT2")))
}

#' Retrieve version of \pkg{rSOILWAT2} that was used to create object
#'
#' @param object An object of class \code{\linkS4class{swInputData}} or
#'   \code{\linkS4class{swOutput}}.
#'
#' @return A character string representing the version number (or \code{NA}).
#'
#' @seealso \code{\link{check_version}}
#'
#' @examples
#' get_version(rSOILWAT2::sw_exampleData)
#' get_version(sw_exec(rSOILWAT2::sw_exampleData))
#' get_version(as.numeric_version("4.1.3"))
#' get_version(packageVersion("rSOILWAT2"))
#'
#' @export
setGeneric("get_version", function(object) standardGeneric("get_version"))

#' @rdname get_version
setMethod(
  "get_version",
  signature = "ANY",
  definition = function(object) {
    tmp <- try(inherits(object, "numeric_version"), silent = TRUE)
    if (inherits(tmp, "try-error") || !isTRUE(tmp)) {
      tmp <- try(object@version, silent = TRUE)
      if (length(tmp) == 0L || inherits(tmp, "try-error")) {
        NA_character_
      } else {
        as.character(as.numeric_version(tmp))
      }
    } else {
      as.character(object) # numeric version
    }
  }
)


#' Check that version of an input or output object is up-to-date
#'
#' @param object An object of class \code{\linkS4class{swInputData}} or
#'   \code{\linkS4class{swOutput}}.
#' @param expected_version A numeric version. The \code{object} should have
#'   the same or a newer version number than the expected version;
#'   defaults to the current \pkg{rSOILWAT2} version number.
#' @param level A character string. The level at which to detect changes
#'   in the version number \var{major.minor.patch.devel}.
#'   For instance, a value of \var{"minor"} would ignore patch-level changes.
#'
#' @return A logical value.
#'
#' @seealso \code{\link{validObject}}
#'
#' @examples
#' # Should pass
#' check_version(rSOILWAT2::sw_exampleData, level = "minor")
#' check_version(rSOILWAT2::sw_exampleData, "1.0.0", level = "patch")
#'
#' # May fail due to a recent patch
#' try(check_version(rSOILWAT2::sw_exampleData, level = "patch"))
#'
#' @export
check_version <- function(
  object,
  expected_version = rSW2_version(),
  level = c("minor", "major", "patch", "devel")
) {
  has <- get_version(object)

  if (length(has) == 0L || is.na(has) || is.na(expected_version)) {
    FALSE

  } else {
    has <- as.numeric_version(has)
    expected <- as.numeric_version(expected_version)

    # Adjust for level to compare versions
    level <- match.arg(level)

    if (level %in% c("major", "minor", "patch")) {
      # zero all development-levels

      # identify number of levels
      # rely on `format.numeric_version()` using "." to concatenate levels
      ns <- lapply(
        c(has, expected),
        function(x) {
          1 + length(gregexpr(".", as.character(x), fixed = TRUE)[[1]])
        }
      )

      if (ns[[1]] > 3) for (k in seq(4, ns[[1]])) has[[c(1, k)]] <- 0
      if (ns[[2]] > 3) for (k in seq(4, ns[[2]])) expected[[c(1, k)]] <- 0
    }

    if (level %in% c("major", "minor")) {
      # zero the patchlevel
      has[[c(1, 3)]] <- 0
      expected[[c(1, 3)]] <- 0
    }

    if (identical(level, "major")) {
      # zero the minor-level
      has[[c(1, 2)]] <- 0
      expected[[c(1, 2)]] <- 0
    }

    # Compare
    has >= expected
  }
}


#' Create \pkg{rSOILWAT2} time stamp
rSW2_timestamp <- function() unclass(Sys.time())

#' Retrieve time stamp of an object
#'
#' @param object An object of class \code{\linkS4class{swInputData}} or
#'   \code{\linkS4class{swOutput}}.
#'
#' @seealso \code{\link{format_timestamp}}
#'
#' @examples
#' get_timestamp(rSOILWAT2::sw_exampleData)
#' get_timestamp(sw_exec(rSOILWAT2::sw_exampleData))
#'
#' @export
setGeneric("get_timestamp", function(object) standardGeneric("get_timestamp"))

#' @rdname get_timestamp
setMethod(
  "get_timestamp",
  signature = "ANY",
  definition = function(object) {
    tmp <- try(object@timestamp, silent = TRUE)
    if (inherits(tmp, "try-error")) NA_real_ else tmp
  }
)

#' Format time stamp of an input or output object
#'
#' @param object An object of class \code{\linkS4class{swInputData}} or
#'   \code{\linkS4class{swOutput}}.
#'
#' @seealso \code{\link{get_timestamp}}
#'
#' @examples
#' format_timestamp(rSOILWAT2::sw_exampleData)
#' format_timestamp(sw_exec(rSOILWAT2::sw_exampleData))
#'
#' @export
format_timestamp <- function(object) {
  as.POSIXct(get_timestamp(object), origin = "1970-01-01 00:00.00 UTC")
}


#------ Upgrade sw objects to newer rSOILWAT2 versions ------

#' Upgrade a `rSOILWAT2`-classed object from an older package version
#'
#' Missing slots and elements are added and
#' take the new default values from `SOILWAT2`.
#'
#' @param object An object of a `rSOILWAT2` class.
#' @param verbose A logical value.
#'
#' @return The upgraded `object`, if needed, to match the current version
#'   with missing slots and elements filled with default values.
#'
#' @section Details:
#' List of changes:
#'   * Changes with `v6.3.0`:
#'       * class [`swSoils-class`]:
#'           * new slot `"omSWRCp"`; slot `"SWRCp"` refers to mineral soil
#'           * slot `"Layers"` gained column `"som_frac"`
#'       * class [`swSite-class`]: new slots `"depth_sapric"` and
#'         `"SurfaceTemperatureMethod"`
#'   * Changes with `v6.2.0`:
#'       * class [`swWeatherData`]: slot `"data"` changed column name
#'         (`"specHavg_pct"` to `"specHavg_gPERkg"`) and
#'         units (`"%"` to `"g kg-1"`).
#'   * Changes with `v6.1.0`:
#'       * class [`swInputData-class`]:
#'         new slot `"spinup"` of new class [`swSpinup-class`]
#'       * class [`swFiles-class`]: new total of 27 input files
#'   * Changes with `v6.0.0`:
#'       * class [`swSite-class`]:
#'         new slots `"swrc_flags"`, `"has_swrcp"`, and
#'         `"SoilDensityInputType"`
#'       * class [`swSoils-class`]: new slot `"SWRCp"`
#'       * class [`swFiles-class`]:
#'         `SWRC` parameter input file added as file 6 for a new total of 23
#'       * class [`swProd-class`]: new slot `"veg_method"`
#'       * class [`swWeatherData`]: new slots `"use_cloudCoverMonthly"`,
#'         `"use_windSpeedMonthly"`, `"use_humidityMonthly"`,
#'         `"dailyInputFlags"`, and `"desc_rsds"`
#'       * class [`swWeatherData`]: slot `"data"` gained 11 new columns:
#'         `"cloudCov_pct"`, `"windSpeed_mPERs"`, `"windSpeed_east_mPERs"`,
#'         `"windSpeed_north_mPERs"`, `"rHavg_pct"`, `"rHmax_pct"`,
#'         `"rHmin_pct"`, `"specHavg_pct"`, `"Tdewpoint_C"`, `"actVP_kPa"`,
#'         and `"shortWR"`
#'   * Changes with `v5.4.0`:
#'       * classes [`swEstabSpecies-class`] and [`swEstab-class`]:
#'         new slot `"vegType"`
#'   * Changes with `v5.2.0`:
#'       * class [`swOUT-class`]:
#'         `"FROZEN"` added as `outkey` 28 for a new total of 32
#'   * Changes with `v3.1.0`:
#'       * class [`swOUT-class`]:
#'         `"BIOMASS"` added as `outkey` 31 for a new total of 31
#'   * Changes with `v2.3.0`:
#'       * class [`swOUT-class`]:
#'         `"SWA"` added as `outkey` 8 for a new total of 30
#'
#' @examples
#' x <- sw_upgrade(rSOILWAT2::sw_exampleData, verbose = TRUE)
#'
#' @md
#' @exportMethod sw_upgrade
setGeneric(
  "sw_upgrade",
  function(object, verbose = FALSE) standardGeneric("sw_upgrade")
)

#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "ANY",
  definition = function(object, verbose = FALSE) {
    object
  }
)



#########FILES##########
#' \code{get_swFiles}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("get_swFiles", function(object) standardGeneric("get_swFiles"))

#' \code{swFiles_ProjDir}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_ProjDir",
  function(object) standardGeneric("swFiles_ProjDir")
)

#' \code{swFiles_filesIn}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_filesIn",
  function(object) standardGeneric("swFiles_filesIn")
)

#' \code{swFiles_Years}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Years",
  function(object) standardGeneric("swFiles_Years")
)

#' \code{swFiles_LogFile}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_LogFile",
  function(object) standardGeneric("swFiles_LogFile")
)

#' \code{swFiles_SiteParams}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SiteParams",
  function(object) standardGeneric("swFiles_SiteParams")
)

#' \code{swFiles_Soils}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Soils",
  function(object) standardGeneric("swFiles_Soils")
)

#' \code{swFiles_SWRCp}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SWRCp",
  function(object) standardGeneric("swFiles_SWRCp")
)

#' \code{swFiles_WeatherSetup}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_WeatherSetup",
  function(object) standardGeneric("swFiles_WeatherSetup")
)

#' \code{swFiles_MarkovProbs}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_MarkovProbs",
  function(object) standardGeneric("swFiles_MarkovProbs")
)

#' \code{swFiles_MarkovCov}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_MarkovCov",
  function(object) standardGeneric("swFiles_MarkovCov")
)

#' \code{swFiles_Cloud}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Cloud",
  function(object) standardGeneric("swFiles_Cloud")
)

#' \code{swFiles_Prod}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Prod",
  function(object) standardGeneric("swFiles_Prod")
)

#' \code{swFiles_Estab}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Estab",
  function(object) standardGeneric("swFiles_Estab")
)

#' \code{swFiles_Carbon}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Carbon",
  function(object) standardGeneric("swFiles_Carbon")
)

#' \code{swFiles_SWCsetup}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SWCsetup",
  function(object) standardGeneric("swFiles_SWCsetup")
)

#' \code{swFiles_Output}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Output",
  function(object) standardGeneric("swFiles_Output")
)

#' \code{swFiles_WeatherPrefix}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_WeatherPrefix",
  function(object) standardGeneric("swFiles_WeatherPrefix")
)

#' \code{swFiles_OutputPrefix}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_OutputPrefix",
  function(object) standardGeneric("swFiles_OutputPrefix")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swFiles<-}} doesn't work.
#' \code{set_swFiles}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swFiles",
  function(object, value) standardGeneric("set_swFiles")
)

#' \code{set_swFiles<-}
#' @inheritParams set_swFiles
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swFiles<-",
  function(object, value) standardGeneric("set_swFiles<-")
)

#' \code{swFiles_ProjDir<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_ProjDir<-",
  function(object, value) standardGeneric("swFiles_ProjDir<-")
)

#' \code{swFiles_filesIn<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_filesIn<-",
  function(object, value) standardGeneric("swFiles_filesIn<-")
)

#' \code{swFiles_Years<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Years<-",
  function(object, value) standardGeneric("swFiles_Years<-")
)

#' \code{swFiles_LogFile<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_LogFile<-",
  function(object, value) standardGeneric("swFiles_LogFile<-")
)

#' \code{swFiles_SiteParams<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SiteParams<-",
  function(object, value) standardGeneric("swFiles_SiteParams<-")
)

#' \code{swFiles_Soils<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Soils<-",
  function(object, value) standardGeneric("swFiles_Soils<-")
)

#' \code{swFiles_SWRCp<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SWRCp<-",
  function(object, value) standardGeneric("swFiles_SWRCp<-")
)

#' \code{swFiles_WeatherSetup<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_WeatherSetup<-",
  function(object, value) standardGeneric("swFiles_WeatherSetup<-")
)

#' \code{swFiles_MarkovProbs<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_MarkovProbs<-",
  function(object, value) standardGeneric("swFiles_MarkovProbs<-")
)

#' \code{swFiles_MarkovCov<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_MarkovCov<-",
  function(object, value) standardGeneric("swFiles_MarkovCov<-")
)

#' \code{swFiles_Cloud<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Cloud<-",
  function(object, value) standardGeneric("swFiles_Cloud<-")
)

#' \code{swFiles_Prod<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Prod<-",
  function(object, value) standardGeneric("swFiles_Prod<-")
)

#' \code{swFiles_Estab<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Estab<-",
  function(object, value) standardGeneric("swFiles_Estab<-")
)

#' \code{swFiles_Carbon<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Carbon<-",
  function(object, value) standardGeneric("swFiles_Carbon<-")
)

#' \code{swFiles_SWCsetup<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_SWCsetup<-",
  function(object, value) standardGeneric("swFiles_SWCsetup<-")
)

#' \code{swFiles_Output<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_Output<-",
  function(object, value) standardGeneric("swFiles_Output<-")
)

#' \code{swFiles_WeatherPrefix<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_WeatherPrefix<-",
  function(object, value) standardGeneric("swFiles_WeatherPrefix<-")
)

#' \code{swFiles_OutputPrefix<-}
#' @param object An object of class \code{\linkS4class{swFiles}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swFiles_OutputPrefix<-",
  function(object, value) standardGeneric("swFiles_OutputPrefix<-")
)
########################

########YEARS###########
#' \code{get_swYears}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swYears",
  function(object) standardGeneric("get_swYears")
)

#' \code{swYears_StartYear}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_StartYear",
  function(object) standardGeneric("swYears_StartYear")
)

#' \code{swYears_EndYear}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_EndYear",
  function(object) standardGeneric("swYears_EndYear")
)

#' \code{swYears_FDOFY}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_FDOFY",
  function(object) standardGeneric("swYears_FDOFY")
)

#' \code{swYears_EDOEY}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_EDOEY",
  function(object) standardGeneric("swYears_EDOEY")
)

#' \code{swYears_isNorth}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_isNorth",
  function(object) standardGeneric("swYears_isNorth")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swYears<-}} doesn't work.
#' \code{set_swYears}
#'
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#'
#' @export
setGeneric(
  "set_swYears",
  function(object, value) standardGeneric("set_swYears")
)

#' \code{set_swYears<-}
#' @inheritParams set_swYears
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swYears<-",
  function(object, value) standardGeneric("set_swYears<-")
)

#' \code{swYears_StartYear<-}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_StartYear<-",
  function(object, value) standardGeneric("swYears_StartYear<-")
)

#' \code{swYears_EndYear<-}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_EndYear<-",
  function(object, value) standardGeneric("swYears_EndYear<-")
)

#' \code{swYears_FDOFY<-}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_FDOFY<-",
  function(object, value) standardGeneric("swYears_FDOFY<-")
)
#' \code{swYears_EDOEY<-}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_EDOEY<-",
  function(object, value) standardGeneric("swYears_EDOEY<-")
)

#' \code{swYears_isNorth<-}
#' @param object An object of class \code{\linkS4class{swYears}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swYears_isNorth<-",
  function(object, value) standardGeneric("swYears_isNorth<-")
)
########################

#########DOMAIN#########
#' \code{get_swSpinup}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swSpinup",
  function(object) standardGeneric("get_swSpinup")
)

#' \code{swSpinup_SpinupMode}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupActive",
  function(object) standardGeneric("swSpinup_SpinupActive")
)

#' \code{swSpinup_SpinupMode}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupMode",
  function(object) standardGeneric("swSpinup_SpinupMode")
)

#' \code{swSpinup_SpinupScope}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupScope",
  function(object) standardGeneric("swSpinup_SpinupScope")
)

#' \code{swSpinup_SpinupDuration}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupDuration",
  function(object) standardGeneric("swSpinup_SpinupDuration")
)

#' \code{swSpinup_SpinupSeed}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupSeed",
  function(object) standardGeneric("swSpinup_SpinupSeed")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swSpinup<-}} doesn't work.
#' \code{set_swSpinup}
#'
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#'
#' @export
setGeneric(
  "set_swSpinup",
  function(object, value) standardGeneric("set_swSpinup")
)

#' \code{set_swSpinup<-}
#' @inheritParams set_swSpinup
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swSpinup<-",
  function(object, value) standardGeneric("set_swSpinup<-")
)

#' \code{swSpinup_SpinupActive<-}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupActive<-",
  function(object, value) standardGeneric("swSpinup_SpinupActive<-")
)

#' \code{swSpinup_SpinupMode<-}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupMode<-",
  function(object, value) standardGeneric("swSpinup_SpinupMode<-")
)

#' \code{swSpinup_SpinupScope<-}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupScope<-",
  function(object, value) standardGeneric("swSpinup_SpinupScope<-")
)

#' \code{swSpinup_SpinupDuration<-}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupDuration<-",
  function(object, value) standardGeneric("swSpinup_SpinupDuration<-")
)

#' \code{swSpinup_SpinupSeed<-}
#' @param object An object of class \code{\linkS4class{swSpinup}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSpinup}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSpinup_SpinupSeed<-",
  function(object, value) standardGeneric("swSpinup_SpinupSeed<-")
)
########################

########WEATHER#########
#' \code{get_swWeather}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swWeather",
  function(object) standardGeneric("get_swWeather")
)

#' \code{swWeather_DaysRunningAverage}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_DaysRunningAverage",
  function(object) standardGeneric("swWeather_DaysRunningAverage")
)

#' \code{swWeather_FirstYearHistorical}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_FirstYearHistorical",
  function(object) standardGeneric("swWeather_FirstYearHistorical")
)

#' \code{swWeather_pct_SnowDrift}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_pct_SnowDrift",
  function(object) standardGeneric("swWeather_pct_SnowDrift")
)

#' \code{swWeather_pct_SnowRunoff}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_pct_SnowRunoff",
  function(object) standardGeneric("swWeather_pct_SnowRunoff")
)

#' \code{swWeather_UseMarkov}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseMarkov",
  function(object) standardGeneric("swWeather_UseMarkov")
)

#' \code{swWeather_UseMarkovOnly}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseMarkovOnly",
  function(object) standardGeneric("swWeather_UseMarkovOnly")
)

#' \code{swWeather_UseSnow}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseSnow",
  function(object) standardGeneric("swWeather_UseSnow")
)

#' \code{swWeather_MonScalingParams}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_MonScalingParams",
  function(object) standardGeneric("swWeather_MonScalingParams")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swWeather<-}} doesn't work.
#' \code{set_swWeather}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swWeather",
  function(object, value) standardGeneric("set_swWeather")
)

#' \code{set_swWeather<-}
#' @inheritParams set_swWeather
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swWeather<-",
  signature = "object",
  function(object, value) standardGeneric("set_swWeather<-")
)

#' \code{swWeather_DaysRunningAverage<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_DaysRunningAverage<-",
  function(object, value) standardGeneric("swWeather_DaysRunningAverage<-")
)

#' \code{swWeather_FirstYearHistorical<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_FirstYearHistorical<-",
  function(object, value) standardGeneric("swWeather_FirstYearHistorical<-")
)

#' \code{swWeather_pct_SnowDrift<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_pct_SnowDrift<-",
  function(object, value) standardGeneric("swWeather_pct_SnowDrift<-")
)

#' \code{swWeather_pct_SnowRunoff<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_pct_SnowRunoff<-",
  function(object, value) standardGeneric("swWeather_pct_SnowRunoff<-")
)

#' \code{swWeather_UseMarkov<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseMarkov<-",
  function(object, value) standardGeneric("swWeather_UseMarkov<-")
)

#' \code{swWeather_UseMarkovOnly<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseMarkovOnly<-",
  function(object, value) standardGeneric("swWeather_UseMarkovOnly<-")
)

#' \code{swWeather_UseSnow<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_UseSnow<-",
  function(object, value) standardGeneric("swWeather_UseSnow<-")
)

#' \code{swWeather_MonScalingParams<-}
#' @param object An object of class \code{\linkS4class{swWeather}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swWeather_MonScalingParams<-",
  function(object, value) standardGeneric("swWeather_MonScalingParams<-")
)
########################

########MARKOV##########
#' \code{get_Markov}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_Markov",
  function(object) standardGeneric("get_Markov")
)

#' \code{get_swMarkov}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swMarkov",
  function(object) standardGeneric("get_swMarkov")
)

#' \code{swMarkov_Prob}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swMarkov_Prob",
  function(object) standardGeneric("swMarkov_Prob")
)

#' \code{swMarkov_Conv}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swMarkov_Conv",
  function(object) standardGeneric("swMarkov_Conv")
)


# Need to define and export this generic method -- otherwise,
# \code{\link{set_Markov<-}} doesn't work.
#' \code{set_Markov}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_Markov",
  function(object, value) standardGeneric("set_Markov")
)

#' \code{set_Markov<-}
#' @inheritParams set_Markov
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_Markov<-",
  function(object, value) standardGeneric("set_Markov<-")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swMarkov<-}} doesn't work.
#' \code{set_swMarkov}
#' @inheritParams set_Markov
#' @export
setGeneric(
  "set_swMarkov",
  function(object, value) standardGeneric("set_swMarkov")
)

#' \code{set_swMarkov<-}
#' @inheritParams set_Markov
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swMarkov<-",
  function(object, value) standardGeneric("set_swMarkov<-")
)

#' \code{swMarkov_Prob<-}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swMarkov_Prob<-",
  function(object, value) standardGeneric("swMarkov_Prob<-")
)

#' \code{swMarkov_Conv<-}
#' @param object An object of class \code{\linkS4class{swMarkov}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swMarkov_Conv<-",
  function(object, value) standardGeneric("swMarkov_Conv<-")
)
########################


#####WeatherData########
#' \code{get_WeatherHistory}
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swInputData}}
setGeneric(
  "get_WeatherHistory",
  function(object) standardGeneric("get_WeatherHistory")
)

#' \code{get_swWeatherData}
#' @param object An object of class \code{\linkS4class{swWeatherData}} or
#'   \code{\linkS4class{swInputData}}.
#' @param year An numeric value. The calendar year.
#' @seealso \code{\linkS4class{swWeatherData}} and
#'   \code{\linkS4class{swInputData}}
setGeneric(
  "get_swWeatherData",
  function(object, year) standardGeneric("get_swWeatherData")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_WeatherHistory<-}} doesn't work.
#' \code{set_WeatherHistory}
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_WeatherHistory",
  function(object, value) standardGeneric("set_WeatherHistory")
)

#' \code{set_WeatherHistory<-}
#' @inheritParams set_WeatherHistory
#' @seealso \code{\linkS4class{swInputData}}
setGeneric(
  "set_WeatherHistory<-",
  function(object, value) standardGeneric("set_WeatherHistory<-")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swWeatherData<-}} doesn't work.
#' \code{set_swWeatherData}
#' @param object An object of class \code{\linkS4class{swWeatherData}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swWeatherData",
  function(object, value) standardGeneric("set_swWeatherData")
)

#' \code{set_swWeatherData<-}
#' @inheritParams set_swWeatherData
#' @seealso \code{\linkS4class{swWeatherData}} and
#'   \code{\linkS4class{swInputData}}
setGeneric(
  "set_swWeatherData<-",
  function(object, value) standardGeneric("set_swWeatherData<-")
)
########################

#######CLOUD############
#' \code{get_swCloud}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swCloud",
  function(object) standardGeneric("get_swCloud")
)

#' \code{swCloud_SkyCover}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_SkyCover",
  function(object) standardGeneric("swCloud_SkyCover")
)

#' \code{swCloud_WindSpeed}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_WindSpeed",
  function(object) standardGeneric("swCloud_WindSpeed")
)

#' \code{swCloud_Humidity}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_Humidity",
  function(object) standardGeneric("swCloud_Humidity")
)

#' \code{swCloud_SnowDensity}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_SnowDensity",
  function(object) standardGeneric("swCloud_SnowDensity")
)

#' \code{swCloud_RainEvents}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_RainEvents",
  function(object) standardGeneric("swCloud_RainEvents")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swCloud<-}} doesn't work.
#' \code{set_swCloud}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swCloud",
  function(object, value) standardGeneric("set_swCloud")
)

#' \code{set_swCloud<-}
#' @inheritParams set_swCloud
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swCloud<-",
  function(object, value) standardGeneric("set_swCloud<-")
)

#' \code{swCloud_SkyCover<-}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_SkyCover<-",
  function(object, value) standardGeneric("swCloud_SkyCover<-")
)

#' \code{swCloud_WindSpeed<-}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_WindSpeed<-",
  function(object, value) standardGeneric("swCloud_WindSpeed<-")
)

#' \code{swCloud_Humidity<-}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_Humidity<-",
  function(object, value) standardGeneric("swCloud_Humidity<-")
)

#' \code{swCloud_SnowDensity<-}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_SnowDensity<-",
  function(object, value) standardGeneric("swCloud_SnowDensity<-")
)

#' \code{swCloud_RainEvents<-}
#' @param object An object of class \code{\linkS4class{swCloud}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCloud_RainEvents<-",
  function(object, value) standardGeneric("swCloud_RainEvents<-")
)
########################

########PROD############
#' \code{get_swProd}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swProd",
  function(object) standardGeneric("get_swProd")
)

#' \code{swProd_Composition}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Composition",
  function(object) standardGeneric("swProd_Composition")
)

#' \code{swProd_Albedo}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Albedo",
  function(object) standardGeneric("swProd_Albedo")
)

#' \code{swProd_CanopyHeight}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CanopyHeight",
  function(object) standardGeneric("swProd_CanopyHeight")
)

#' \code{swProd_VegInterParam}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_VegInterParam",
  function(object) standardGeneric("swProd_VegInterParam")
)

#' \code{swProd_LitterInterParam}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_LitterInterParam",
  function(object) standardGeneric("swProd_LitterInterParam")
)

#' \code{swProd_EsTpartitioning_param}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_EsTpartitioning_param",
  function(object) standardGeneric("swProd_EsTpartitioning_param")
)

#' \code{swProd_Es_param_limit}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Es_param_limit",
  function(object) standardGeneric("swProd_Es_param_limit")
)

#' \code{swProd_Shade}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Shade",
  function(object) standardGeneric("swProd_Shade")
)

#' \code{swProd_HydrRedstro_use}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_HydrRedstro_use",
  function(object) standardGeneric("swProd_HydrRedstro_use")
)

#' \code{swProd_HydrRedstro}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_HydrRedstro",
  function(object) standardGeneric("swProd_HydrRedstro")
)

#' \code{swProd_CritSoilWaterPotential}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CritSoilWaterPotential",
  function(object) standardGeneric("swProd_CritSoilWaterPotential")
)

#' \code{swProd_CO2Coefficients}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CO2Coefficients",
  function(object) standardGeneric("swProd_CO2Coefficients")
)

#' \code{swProd_MonProd_veg}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param vegtype The name or index of the vegetation type.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_veg",
  function(object, vegtype) standardGeneric("swProd_MonProd_veg")
)

#' \code{swProd_MonProd_grass}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_grass",
  function(object) standardGeneric("swProd_MonProd_grass")
)

#' \code{swProd_MonProd_shrub}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_shrub",
  function(object) standardGeneric("swProd_MonProd_shrub")
)

#' \code{swProd_MonProd_tree}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_tree",
  function(object) standardGeneric("swProd_MonProd_tree")
)

#' \code{swProd_MonProd_forb}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_forb",
  function(object) standardGeneric("swProd_MonProd_forb")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swProd<-}} doesn't work.
#' \code{set_swProd}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swProd",
  function(object, value) standardGeneric("set_swProd")
)

#' \code{set_swProd<-}
#' @inheritParams set_swProd
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swProd<-",
  function(object, value) standardGeneric("set_swProd<-")
)

#' \code{swProd_Composition<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Composition<-",
  function(object, value) standardGeneric("swProd_Composition<-")
)

#' \code{swProd_Albedo<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Albedo<-",
  function(object, value) standardGeneric("swProd_Albedo<-")
)

#' \code{swProd_CanopyHeight<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CanopyHeight<-",
  function(object, value) standardGeneric("swProd_CanopyHeight<-")
)

#' \code{swProd_VegInterParam<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_VegInterParam<-",
  function(object, value) standardGeneric("swProd_VegInterParam<-")
)

#' \code{swProd_LitterInterParam<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_LitterInterParam<-",
  function(object, value) standardGeneric("swProd_LitterInterParam<-")
)

#' \code{swProd_EsTpartitioning_param<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_EsTpartitioning_param<-",
  function(object, value) standardGeneric("swProd_EsTpartitioning_param<-")
)

#' \code{swProd_Es_param_limit<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Es_param_limit<-",
  function(object, value) standardGeneric("swProd_Es_param_limit<-")
)

#' \code{swProd_Shade<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_Shade<-",
  function(object, value) standardGeneric("swProd_Shade<-")
)

#' \code{swProd_HydrRedstro_use<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_HydrRedstro_use<-",
  function(object, value) standardGeneric("swProd_HydrRedstro_use<-")
)

#' \code{swProd_HydrRedstro<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_HydrRedstro<-",
  function(object, value) standardGeneric("swProd_HydrRedstro<-")
)

#' \code{swProd_CritSoilWaterPotential<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CritSoilWaterPotential<-",
  function(object, value) standardGeneric("swProd_CritSoilWaterPotential<-")
)

#' \code{swProd_CO2Coefficients<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_CO2Coefficients<-",
  function(object, value) standardGeneric("swProd_CO2Coefficients<-")
)

#' \code{swProd_MonProd_veg<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param vegtype The name or index of the vegetation type.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_veg<-",
  function(object, vegtype, value) standardGeneric("swProd_MonProd_veg<-")
)

#' \code{swProd_MonProd_grass<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_grass<-",
  function(object, value) standardGeneric("swProd_MonProd_grass<-")
)

#' \code{swProd_MonProd_shrub<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_shrub<-",
  function(object, value) standardGeneric("swProd_MonProd_shrub<-")
)

#' \code{swProd_MonProd_tree<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_tree<-",
  function(object, value) standardGeneric("swProd_MonProd_tree<-")
)

#' \code{swProd_MonProd_forb<-}
#' @param object An object of class \code{\linkS4class{swProd}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swProd_MonProd_forb<-",
  function(object, value) standardGeneric("swProd_MonProd_forb<-")
)
########################

#######SITE#############
#' \code{get_swSite}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swSite",
  function(object) standardGeneric("get_swSite")
)

#' Names of `SWRC` and `PTF`
#'
#' @param object An object of class [swSite-class] or [swInputData-class].
#'
#' @return A character vector with two elements `"swrc_name"` and `"ptf_name"`.
#'
#' @md
#' @exportMethod swSite_SWRCflags
setGeneric(
  "swSite_SWRCflags",
  function(object) standardGeneric("swSite_SWRCflags")
)

#' Are mineral soil `SWRC` parameters provided in `SWRCp`?
#'
#' @param object An object of class [swSite-class] or [swInputData-class].
#'
#' @return A logical value.
#' `TRUE` if mineral soil `SWRC` parameters are provided in `SWRCp`;
#' `FALSE` if `SWRCp` should be estimated during a simulation run
#' via specified pedotransfer function
#' (see `"ptf_name"` of [swSite_SWRCflags()]).
#'
#' @md
#' @exportMethod swSite_hasSWRCp
setGeneric(
  "swSite_hasSWRCp",
  function(object) standardGeneric("swSite_hasSWRCp")
)

#' \code{swSite_depthSapric}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
#' @exportMethod swSite_depthSapric
setGeneric(
  "swSite_depthSapric",
  function(object) standardGeneric("swSite_depthSapric")
)


#' \code{swSite_SWClimits}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SWClimits",
  function(object) standardGeneric("swSite_SWClimits")
)

#' \code{swSite_ModelFlags}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_ModelFlags",
  function(object) standardGeneric("swSite_ModelFlags")
)

#' \code{swSite_ModelCoefficients}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_ModelCoefficients",
  function(object) standardGeneric("swSite_ModelCoefficients")
)

#' \code{swSite_SnowSimulationParams}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SnowSimulationParams",
  function(object) standardGeneric("swSite_SnowSimulationParams")
)

#' \code{swSite_DrainageCoefficient}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_DrainageCoefficient",
  function(object) swSite_DrainageCoefficient("swSite_DrainageCoefficient")
)

#' \code{swSite_EvapCoefficients}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_EvapCoefficients",
  function(object) standardGeneric("swSite_EvapCoefficients")
)

#' \code{swSite_TranspCoefficients}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_TranspCoefficients",
  function(object) standardGeneric("swSite_TranspCoefficients")
)

#' \code{swSite_IntrinsicSiteParams}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_IntrinsicSiteParams",
  function(object) standardGeneric("swSite_IntrinsicSiteParams")
)

# swSite_SurfaceTempMethod() should be called
# swSite_SurfaceTemperatureMethod() for consistency but that is
# an "overlong name" (31 + 3 > 32, see swSite_SurfaceTempMethod())

#' \code{swSite_SurfaceTempMethod}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SurfaceTempMethod",
  function(object) standardGeneric("swSite_SurfaceTempMethod")
)

#' \code{swSite_SoilTemperatureFlag}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilTemperatureFlag",
  function(object) standardGeneric("swSite_SoilTemperatureFlag")
)

#' \code{swSite_SoilTemperatureConsts}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilTemperatureConsts",
  function(object) standardGeneric("swSite_SoilTemperatureConsts")
)

#' \code{swSite_SoilDensityInputType}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilDensityInputType",
  function(object) standardGeneric("swSite_SoilDensityInputType")
)


#' \code{swSite_TranspirationRegions}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_TranspirationRegions",
  function(object) standardGeneric("swSite_TranspirationRegions")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swSite<-}} doesn't work.
#' \code{set_swSite}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swSite",
  function(object, value) standardGeneric("set_swSite")
)

#' \code{set_swSite<-}
#' @inheritParams set_swSite
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swSite<-",
  function(object, value) standardGeneric("set_swSite<-")
)

#' @rdname swSite_SWRCflags
#'
#' @param value A character vector with two elements for
#' `"swrc_name"` and `"ptf_name"`.
#'
#' @exportMethod swSite_SWRCflags<-
#' @md
setGeneric(
  "swSite_SWRCflags<-",
  function(object, value) standardGeneric("swSite_SWRCflags<-")
)

#' @rdname swSite_hasSWRCp
#'
#' @param value A logical value.
#'
#' @exportMethod swSite_hasSWRCp<-
setGeneric(
  "swSite_hasSWRCp<-",
  function(object, value) standardGeneric("swSite_hasSWRCp<-")
)

#' @rdname swSite_depthSapric
#'
#' @param value A numeric value.
#'
#' @exportMethod swSite_depthSapric<-
setGeneric(
  "swSite_depthSapric<-",
  function(object, value) standardGeneric("swSite_depthSapric<-")
)

#' \code{swSite_SWClimits<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SWClimits<-",
  function(object, value) standardGeneric("swSite_SWClimits<-")
)

#' \code{swSite_ModelFlags<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_ModelFlags<-",
  function(object, value) standardGeneric("swSite_ModelFlags<-")
)

#' \code{swSite_ModelCoefficients<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_ModelCoefficients<-",
  function(object, value) standardGeneric("swSite_ModelCoefficients<-")
)

#' \code{swSite_SnowSimulationParams<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SnowSimulationParams<-",
  function(object, value) standardGeneric("swSite_SnowSimulationParams<-")
)

#' \code{swSite_DrainageCoefficient<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_DrainageCoefficient<-",
  function(object, value) standardGeneric("swSite_DrainageCoefficient<-")
)

#' \code{swSite_EvapCoefficients<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_EvapCoefficients<-",
  function(object, value) standardGeneric("swSite_EvapCoefficients<-")
)

#' \code{swSite_TranspCoefficients<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_TranspCoefficients<-",
  function(object, value) standardGeneric("swSite_TranspCoefficients<-")
)

#' \code{swSite_IntrinsicSiteParams<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_IntrinsicSiteParams<-",
  function(object, value) standardGeneric("swSite_IntrinsicSiteParams<-")
)

#' \code{swSite_SurfaceTempMethod<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SurfaceTempMethod<-",
  function(object, value) standardGeneric("swSite_SurfaceTempMethod<-")
)

#' \code{swSite_SoilTemperatureFlag<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilTemperatureFlag<-",
  function(object, value) standardGeneric("swSite_SoilTemperatureFlag<-")
)

#' \code{swSite_SoilTemperatureConsts<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilTemperatureConsts<-",
  function(object, value) standardGeneric("swSite_SoilTemperatureConsts<-")
)

#' \code{swSite_SoilDensityInputType<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_SoilDensityInputType<-",
  function(object, value) standardGeneric("swSite_SoilDensityInputType<-")
)

#' \code{swSite_TranspirationRegions<-}
#' @param object An object of class \code{\linkS4class{swSite}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSite_TranspirationRegions<-",
  function(object, value) standardGeneric("swSite_TranspirationRegions<-")
)
########################

#########SOILS##########
#' \code{get_swSoils}
#' @param object An object of class \code{\linkS4class{swSoils}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swSoils",
  function(object) standardGeneric("get_swSoils")
)

#' Interact with the soil layer data frame
#'
#' @param object An object of class [`swSoils`] or [swInputData-class].
#'
#' @md
#' @exportMethod swSoils_Layers
setGeneric(
  "swSoils_Layers",
  function(object) standardGeneric("swSoils_Layers")
)

#' `SWRC` parameters of the mineral soil
#'
#' @param object An object of class [`swSoils`] or [swInputData-class].
#'
#' @return A data matrix.
#'
#' @md
#' @exportMethod swSoils_SWRCp
setGeneric(
  "swSoils_SWRCp",
  function(object) standardGeneric("swSoils_SWRCp")
)

#' `SWRC` parameters of fibric and sapric organic matter
#'
#' @param object An object of class [`swSoils`] or [swInputData-class].
#'
#' @return A data matrix.
#'
#' @examples
#'   swin <- rSOILWAT2::sw_exampleData
#'   swSoils_omSWRCp(swin) <- rSOILWAT2::sw2_list_omSWRCp[["Campbell1974"]]
#'
#' @md
#' @exportMethod swSoils_omSWRCp
setGeneric(
  "swSoils_omSWRCp",
  function(object) standardGeneric("swSoils_omSWRCp")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swSoils<-}} doesn't work.
#' \code{set_swSoils}
#' @param object An object of class \code{\linkS4class{swSoils}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swSoils",
  function(object, value) standardGeneric("set_swSoils")
)

#' \code{set_swSoils<-}
#' @inheritParams set_swSoils
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swSoils<-",
  function(object, value) standardGeneric("set_swSoils<-")
)

#' @rdname swSoils_Layers
#'
#' @param value An object that can be converted to a data matrix and represents
#' required soil layer information.
#'
#' @exportMethod swSoils_Layers<-
setGeneric(
  "swSoils_Layers<-",
  function(object, value) standardGeneric("swSoils_Layers<-")
)

#' @rdname swSoils_SWRCp
#'
#' @param value An object that can be converted to a data matrix and represents
#' required `SWRC` parameters.
#'
#' @exportMethod swSoils_SWRCp<-
setGeneric(
  "swSoils_SWRCp<-",
  function(object, value) standardGeneric("swSoils_SWRCp<-")
)

#' @rdname swSoils_omSWRCp
#'
#' @param value An object that can be converted to a data matrix and represents
#' required `SWRC` parameters.
#'
#' @exportMethod swSoils_omSWRCp<-
setGeneric(
  "swSoils_omSWRCp<-",
  function(object, value) standardGeneric("swSoils_omSWRCp<-")
)

########################

#########ESTAB##########
#' \code{get_swEstab}
#' @param object An object of class \code{\linkS4class{swEstab}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swEstab",
  function(object) standardGeneric("get_swEstab")
)

#' \code{swEstab_useEstab}
#' @param object An object of class \code{\linkS4class{swEstab}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swEstab_useEstab",
  function(object) standardGeneric("swEstab_useEstab")
)
#species here#

#' \code{set_swEstab<-}
#' @param object An object of class \code{\linkS4class{swEstab}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swEstab<-",
  function(object, value) standardGeneric("set_swEstab<-")
)

#' \code{swEstab_useEstab<-}
#' @param object An object of class \code{\linkS4class{swEstab}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swEstab_useEstab<-",
  function(object, value) standardGeneric("swEstab_useEstab<-")
)
#species here#
########################

#########CARBON##########
#' \code{get_swCarbon}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swCarbon",
  function(object) standardGeneric("get_swCarbon")
)

#' \code{swCarbon_Use_Bio}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Use_Bio",
  function(object) standardGeneric("swCarbon_Use_Bio")
)

#' \code{swCarbon_Use_WUE}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Use_WUE",
  function(object) standardGeneric("swCarbon_Use_WUE")
)

#' \code{swCarbon_Scenario}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Scenario",
  function(object) standardGeneric("swCarbon_Scenario")
)

#' \code{swCarbon_DeltaYear}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_DeltaYear",
  function(object) standardGeneric("swCarbon_DeltaYear")
)

#' \code{swCarbon_CO2ppm}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_CO2ppm",
  function(object) standardGeneric("swCarbon_CO2ppm")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swCarbon<-}} doesn't work.
#' \code{set_swCarbon}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swCarbon",
  function(object, value) standardGeneric("set_swCarbon")
)

#' \code{set_swCarbon<-}
#' @inheritParams set_swCarbon
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swCarbon<-",
  function(object, value) standardGeneric("set_swCarbon<-")
)

#' \code{swCarbon_Use_Bio<-}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Use_Bio<-",
  function(object, value) standardGeneric("swCarbon_Use_Bio<-")
)

#' \code{swCarbon_Use_WUE<-}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Use_WUE<-",
  function(object, value) standardGeneric("swCarbon_Use_WUE<-")
)

#' \code{swCarbon_Scenario<-}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_Scenario<-",
  function(object, value) standardGeneric("swCarbon_Scenario<-")
)

#' \code{swCarbon_DeltaYear<-}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_DeltaYear<-",
  function(object, value) standardGeneric("swCarbon_DeltaYear<-")
)

#' \code{swCarbon_CO2ppm<-}
#' @param object An object of class \code{\linkS4class{swCarbon}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swCarbon_CO2ppm<-",
  function(object, value) standardGeneric("swCarbon_CO2ppm<-")
)
########################

#########SWC############
#' \code{get_swSWC}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swSWC",
  function(object) standardGeneric("get_swSWC")
)

#' \code{swSWC_use}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_use",
  function(object) standardGeneric("swSWC_use")
)

#' \code{swSWC_prefix}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_prefix",
  function(object) standardGeneric("swSWC_prefix")
)

#' \code{swSWC_FirstYear}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_FirstYear",
  function(object) standardGeneric("swSWC_FirstYear")
)

#' \code{swSWC_Method}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_Method",
  function(object) standardGeneric("swSWC_Method")
)

#' \code{swSWC_HistoricList}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_HistoricList",
  function(object) standardGeneric("swSWC_HistoricList")
)

#' \code{swSWC_HistoricData}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param year An numeric value. The calendar year.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_HistoricData",
  function(object, year) standardGeneric("swSWC_HistoricData")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swSWC<-}} doesn't work.
#' \code{set_swSWC}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swSWC",
  function(object, value) standardGeneric("set_swSWC")
)

#' \code{set_swSWC<-}
#' @inheritParams set_swSWC
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swSWC<-",
  function(object, value) standardGeneric("set_swSWC<-")
)

#' \code{swSWC_use<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_use<-",
  function(object, value) standardGeneric("swSWC_use<-")
)

#' \code{swSWC_prefix<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_prefix<-",
  function(object, value) standardGeneric("swSWC_prefix<-")
)

#' \code{swSWC_FirstYear<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_FirstYear<-",
  function(object, value) standardGeneric("swSWC_FirstYear<-")
)

#' \code{swSWC_Method<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_Method<-",
  function(object, value) standardGeneric("swSWC_Method<-")
)

#' \code{swSWC_HistoricList<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_HistoricList<-",
  function(object, value) standardGeneric("swSWC_HistoricList<-")
)

#' \code{swSWC_HistoricData<-}
#' @param object An object of class \code{\linkS4class{swSWC}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swSWC_HistoricData<-",
  function(object, value) standardGeneric("swSWC_HistoricData<-")
)
########################

#######OUT###########
#' \code{get_swOUT}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "get_swOUT",
  function(object) standardGeneric("get_swOUT")
)

#' \code{swOUT_TimeStep}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOUT_TimeStep",
  function(object) standardGeneric("swOUT_TimeStep")
)

#' \code{swOUT_OutputSeparator}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOUT_OutputSeparator",
  function(object) standardGeneric("swOUT_OutputSeparator")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{set_swOUT<-}} doesn't work.
#' \code{set_swOUT}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @export
setGeneric(
  "set_swOUT",
  function(object, value) standardGeneric("set_swOUT")
)

#' \code{set_swOUT<-}
#' @inheritParams set_swOUT
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "set_swOUT<-",
  function(object, value) standardGeneric("set_swOUT<-")
)

#' \code{swOUT_TimeStep<-}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOUT_TimeStep<-",
  function(object, value) standardGeneric("swOUT_TimeStep<-")
)

# Need to define and export this generic method -- otherwise,
# \code{\link{swOUT_TimeStepsForEveryKey<-}} doesn't work.
#' \code{swOUT_TimeStepsForEveryKey}
#'
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#'
#' @export
setGeneric(
  "swOUT_TimeStepsForEveryKey",
  function(object, value) standardGeneric("swOUT_TimeStepsForEveryKey")
)

#' \code{swOUT_TimeStepsForEveryKey<-}
#' @inheritParams swOUT_TimeStepsForEveryKey
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOUT_TimeStepsForEveryKey<-",
  function(object, value) standardGeneric("swOUT_TimeStepsForEveryKey<-")
)

#' \code{swOUT_OutputSeparator<-}
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOUT_OutputSeparator<-",
  function(object, value) standardGeneric("swOUT_OutputSeparator<-")
)

#' Activate/deactivate an output slot (\var{swOUT_OutKey})
#'
#' @param object An object of class \code{\linkS4class{swOUT}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#'
#' @seealso
#'   \code{\linkS4class{swOUT}}, \code{\linkS4class{swInputData}}, and
#'   \code{\link{sw_exec}}
#'
#' @aliases activate_swOUT_OutKey
#'   deactivate_swOUT_OutKey deactivate_swOUT_OutKey-set
#' @name activate_swOUT_OutKey-set
NULL

#' @rdname activate_swOUT_OutKey-set
setGeneric(
  "activate_swOUT_OutKey<-",
  function(object, value) standardGeneric("activate_swOUT_OutKey<-")
)

#' @rdname activate_swOUT_OutKey-set
setGeneric(
  "deactivate_swOUT_OutKey<-",
  function(object, value) standardGeneric("deactivate_swOUT_OutKey<-")
)

########################

########LOG#############
#' \code{swLog_setLine<-}
#' @param object An object of class \code{\linkS4class{swLog}} or
#'   \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swLog}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swLog_setLine<-",
  function(object, value) standardGeneric("swLog_setLine<-")
)
########################

########swOutput########
#' \code{swOutput_getKEY}
#' @param object An object of class \code{\linkS4class{swOutput}} or
#'   \code{\linkS4class{swInputData}}.
#' @param index An integer value. The "key" (slot) position.
#'
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_getKEY",
  function(object, index) standardGeneric("swOutput_getKEY")
)

#' \code{swOutput_KEY_Period}
#' @inheritParams swOutput_getKEY
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_Period",
  function(object, index) standardGeneric("swOutput_KEY_Period")
)

#' \code{swOutput_KEY_TimeStep}
#' @inheritParams swOutput_getKEY
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_TimeStep",
  function(object) standardGeneric("swOutput_KEY_TimeStep")
)

#' \code{swOutput_KEY_Columns}
#' @inheritParams swOutput_getKEY
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_Columns",
  function(object) standardGeneric("swOutput_KEY_Columns")
)

#' \code{swOutput_setKEY<-}
#' @inheritParams swOutput_getKEY
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_setKEY<-",
  function(object, index, value) standardGeneric("swOutput_setKEY<-")
)

#' \code{swOutput_KEY_Period<-}
#' @inheritParams swOutput_setKEY<-
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_Period<-",
  function(object, index, value) standardGeneric("swOutput_KEY_Period<-")
)

#' \code{swOutput_KEY_TimeStep<-}
#' @inheritParams swOutput_setKEY<-
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_TimeStep<-",
  function(object, value) standardGeneric("swOutput_KEY_TimeStep<-")
)

#' \code{swOutput_KEY_Columns<-}
#' @inheritParams swOutput_setKEY<-
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric(
  "swOutput_KEY_Columns<-",
  function(object, value) standardGeneric("swOutput_KEY_Columns<-")
)
