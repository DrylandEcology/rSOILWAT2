###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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
##############################################

#################################
#' Class \code{"swFiles"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swFiles}}.
#' @param .Object An object of class \code{\linkS4class{swFiles}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' \code{\linkS4class{swCarbon}}
#'
#' @examples
#' showClass("swFiles")
#' x <- new("swFiles")
#'
#' @name swFiles-class
#' @export
setClass("swFiles", slots = c(ProjDir = "character", InFiles = "character",
  WeatherPrefix = "character", OutputPrefix = "character"))

#' @rdname swFiles-class
#' @export
setMethod("initialize", signature = "swFiles", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "files")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

swFiles_validity <- function(object) {
  val <- TRUE

  if (length(object@ProjDir) != 1) {
    msg <- "There must be exactly one @ProjDir value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@InFiles) != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_NFILES"]]) {
    msg <- "The number of (non-empty) @InFiles must be SW_NFILES."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@WeatherPrefix) != 1 || nchar(object@WeatherPrefix) == 0) {
    msg <- "There must be exactly one non-empty @WeatherPrefix value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@OutputPrefix) != 1) {
    msg <- "There must be exactly one @OutputPrefix value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swFiles", swFiles_validity)


#' @rdname swFiles-class
#' @export
setMethod("swFiles_ProjDir", "swFiles", function(object) {
  object@ProjDir
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_WeatherPrefix", "swFiles", function(object) {
  object@WeatherPrefix
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_OutputPrefix", "swFiles", function(object) {
  object@OutputPrefix
})

#' @rdname swFiles-class
#' @export
setMethod("swFiles_filesIn", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eFirst"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Years", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eModel"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_LogFile", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLog"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_SiteParams", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSite"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Soils", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLayers"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_WeatherSetup", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eWeather"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_MarkovProbs", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovProb"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_MarkovCov", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovCov"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Cloud", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSky"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Prod", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegProd"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Estab", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegEstab"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_SWCsetup", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSoilwat"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Carbon", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eCarbon"]]]
})
#' @rdname swFiles-class
#' @export
setMethod("swFiles_Output", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eOutput"]]]
})


#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_ProjDir", signature = "swFiles", function(object, value) {
  object@ProjDir <- value
  validObject(object)
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_WeatherPrefix", signature = "swFiles", function(object, value) {
  object@WeatherPrefix <- value
  validObject(object)
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_OutputPrefix", signature = "swFiles", function(object, value) {
  object@OutputPrefix <- value
  validObject(object)
  object
})

#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_filesIn", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eFirst"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Years", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eModel"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_LogFile", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLog"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_SiteParams", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSite"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Soils", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLayers"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_WeatherSetup", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eWeather"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_MarkovProbs", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovProb"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_MarkovCov", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovCov"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Cloud", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSky"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Prod", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegProd"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Estab", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegEstab"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_SWCsetup", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSoilwat"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Carbon", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eCarbon"]]] <- value
  object
})
#' @rdname swFiles-class
#' @export
setReplaceMethod("swFiles_Output", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eOutput"]]] <- value
  object
})


#' @rdname swFiles-class
#' @export
setMethod("swReadLines", signature = c(object="swFiles",file="character"), function(object,file) {
  print("TODO: method 'swReadLines' is not up-to-date; hard-coded indices are incorrect")

			infiletext <- readLines(con = file)
			object@InFiles[1] = file
			object@InFiles[2] = strsplit(x=infiletext[5],split="\t")[[1]][1]
			object@InFiles[3] = strsplit(x=infiletext[6],split="\t")[[1]][1]
			object@InFiles[4] = strsplit(x=infiletext[9],split="\t")[[1]][1]
			object@InFiles[5] = strsplit(x=infiletext[10],split="\t")[[1]][1]
			object@InFiles[6] = strsplit(x=infiletext[13],split="\t")[[1]][1]
			object@WeatherPrefix = strsplit(x=infiletext[14],split="\t")[[1]][1]
			object@InFiles[7] = strsplit(x=infiletext[15],split="\t")[[1]][1]
			object@InFiles[8] = strsplit(x=infiletext[16],split="\t")[[1]][1]
			object@InFiles[9] = strsplit(x=infiletext[17],split="\t")[[1]][1]
			object@InFiles[10] = strsplit(x=infiletext[20],split="\t")[[1]][1]
			object@InFiles[11] = strsplit(x=infiletext[21],split="\t")[[1]][1]
			object@InFiles[12] = strsplit(x=infiletext[24],split="\t")[[1]][1]
			object@OutputPrefix = strsplit(x=infiletext[27],split="\t")[[1]][1]
			object@InFiles[13] = strsplit(x=infiletext[28],split="\t")[[1]][1]
			names(object@InFiles) <- c("InputFilesForSimulation", "Model_Years", "Model_LogFile", "Site_Params", "Site_Soils", "Weather_setup", "Markov_precip_probs", "Markov_covarianceTable", "Weather_atmosphericParams", "Vegetation_Productivity", "Vegetation_Establishment", "SWC_setup", "Output_setup")
			object
		})
