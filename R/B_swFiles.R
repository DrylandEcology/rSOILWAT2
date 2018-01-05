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
#' @export
setClass("swFiles", slots = c(ProjDir = "character", InFiles = "character",
  WeatherPrefix = "character", OutputPrefix = "character"))

setMethod("initialize", signature = "swFiles", function(.Object, ...) {
  def <- slot(inputData, "files")
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


setMethod("swFiles_ProjDir", "swFiles", function(object) {
  object@ProjDir
})
setMethod("swFiles_WeatherPrefix", "swFiles", function(object) {
  object@WeatherPrefix
})
setMethod("swFiles_OutputPrefix", "swFiles", function(object) {
  object@OutputPrefix
})

setMethod("swFiles_filesIn", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eFirst"]]]
})
setMethod("swFiles_Years", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eModel"]]]
})
setMethod("swFiles_LogFile", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLog"]]]
})
setMethod("swFiles_SiteParams", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSite"]]]
})
setMethod("swFiles_Soils", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLayers"]]]
})
setMethod("swFiles_WeatherSetup", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eWeather"]]]
})
setMethod("swFiles_MarkovProbs", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovProb"]]]
})
setMethod("swFiles_MarkovCov", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovCov"]]]
})
setMethod("swFiles_Cloud", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSky"]]]
})
setMethod("swFiles_Prod", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegProd"]]]
})
setMethod("swFiles_Estab", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegEstab"]]]
})
setMethod("swFiles_SWCsetup", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSoilwat"]]]
})
setMethod("swFiles_Carbon", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eCarbon"]]]
})
setMethod("swFiles_Output", "swFiles", function(object) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eOutput"]]]
})


setReplaceMethod("swFiles_ProjDir", signature = "swFiles", function(object, value) {
  object@ProjDir <- value
  validObject(object)
  object
})
setReplaceMethod("swFiles_WeatherPrefix", signature = "swFiles", function(object, value) {
  object@WeatherPrefix <- value
  validObject(object)
  object
})
setReplaceMethod("swFiles_OutputPrefix", signature = "swFiles", function(object, value) {
  object@OutputPrefix <- value
  validObject(object)
  object
})

setReplaceMethod("swFiles_filesIn", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eFirst"]]] <- value
  object
})
setReplaceMethod("swFiles_Years", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eModel"]]] <- value
  object
})
setReplaceMethod("swFiles_LogFile", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLog"]]] <- value
  object
})
setReplaceMethod("swFiles_SiteParams", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSite"]]] <- value
  object
})
setReplaceMethod("swFiles_Soils", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eLayers"]]] <- value
  object
})
setReplaceMethod("swFiles_WeatherSetup", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eWeather"]]] <- value
  object
})
setReplaceMethod("swFiles_MarkovProbs", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovProb"]]] <- value
  object
})
setReplaceMethod("swFiles_MarkovCov", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eMarkovCov"]]] <- value
  object
})
setReplaceMethod("swFiles_Cloud", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSky"]]] <- value
  object
})
setReplaceMethod("swFiles_Prod", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegProd"]]] <- value
  object
})
setReplaceMethod("swFiles_Estab", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eVegEstab"]]] <- value
  object
})
setReplaceMethod("swFiles_SWCsetup", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eSoilwat"]]] <- value
  object
})
setReplaceMethod("swFiles_Carbon", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eCarbon"]]] <- value
  object
})
setReplaceMethod("swFiles_Output", signature = "swFiles", function(object, value) {
  object@InFiles[1 + rSW2_glovars[["kSOILWAT2"]][["InFiles"]][["eOutput"]]] <- value
  object
})


setMethod("swReadLines", signature=c(object="swFiles",file="character"), function(object,file) {
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
			names(object@InFiles) <- c("InputFilesForSimulation","Model_Years","Model_LogFile","Site_Params","Site_Soils","Weather_setup","Markov_precip_probs","Markov_covarianceTable","Weather_atmosphericParams","Vegetation_Productivity","Vegetation_Establishment","SWC_setup","Output_setup")
			object
		})
