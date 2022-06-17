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


#' Class \code{"swEstabSpecies"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swEstabSpecies}}.
#' @param .Object An object of class \code{\linkS4class{swEstabSpecies}}.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swEstabSpecies")
#' x <- new("swEstabSpecies")
#'
#' @name swEstabSpecies-class
#' @export
setClass("swEstabSpecies", slot = c(fileName = "character", Name = "character",
  estab_lyrs = "integer", barsGERM = "numeric", barsESTAB = "numeric",
  min_pregerm_days = "integer", max_pregerm_days = "integer",
  min_wetdays_for_germ = "integer", max_drydays_postgerm = "integer",
  min_wetdays_for_estab = "integer", min_days_germ2estab = "integer",
  max_days_germ2estab = "integer", min_temp_germ = "numeric",
  max_temp_germ = "numeric", min_temp_estab = "numeric",
  max_temp_estab = "numeric"))

setValidity("swEstabSpecies", function(object) {
  TRUE
})

#' @rdname swEstabSpecies-class
#' @export
setMethod("initialize", signature = "swEstabSpecies", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "estab")
  sns <- slotNames("swEstabSpecies")
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  if (FALSE) {
    # not needed because no relevant inheritance
    .Object <- callNextMethod(.Object, ...)
  }

  validObject(.Object)
  .Object
})


#' @rdname swEstabSpecies-class
#' @export
# nolint start
setMethod("swReadLines", signature = c(object="swEstabSpecies",file="character"), function(object,file) {
  print("TODO: method 'swReadLines' for class 'swInputData' is not up-to-date; hard-coded indices are incorrect")
			infiletext <- readLines(con = file)

			object@Name = c(object@Name, gsub("[[:space:]]", "",strsplit(x=infiletext[1],split = c("#", " ", "\t"),fixed=F)[[1]][1]))
			object@estab_lyrs = c(object@estab_lyrs,readInteger(infiletext[3]))
			object@barsGERM = c(object@barsGERM,readNumeric(infiletext[4]))
			object@barsESTAB = c(object@barsESTAB,readNumeric(infiletext[5]))
			object@min_pregerm_days = c(object@min_pregerm_days,readInteger(infiletext[7]))
			object@max_pregerm_days = c(object@max_pregerm_days,readInteger(infiletext[8]))
			object@min_wetdays_for_germ = c(object@min_wetdays_for_germ,readInteger(infiletext[9]))
			object@max_drydays_postgerm = c(object@max_drydays_postgerm,readInteger(infiletext[10]))
			object@min_wetdays_for_estab = c(object@min_wetdays_for_estab,readInteger(infiletext[11]))
			object@min_days_germ2estab = c(object@min_days_germ2estab,readInteger(infiletext[12]))
			object@max_days_germ2estab = c(object@max_days_germ2estab,readInteger(infiletext[13]))
			object@min_temp_germ = c(object@min_temp_germ,readInteger(infiletext[15]))
			object@max_temp_germ = c(object@max_temp_germ,readNumeric(infiletext[16]))
			object@min_temp_estab = c(object@min_temp_estab,readNumeric(infiletext[17]))
			object@max_temp_estab = c(object@max_temp_estab,readNumeric(infiletext[18]))
			return(object)
		})
# nolint end

#############################ESTAB.IN#########################################
#' Class \code{"swEstab"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swEstab}}.
#' @param .Object An object of class \code{\linkS4class{swEstab}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swEstab")
#' x <- new("swEstab")
#'
#' @name swEstab-class
#' @export
setClass("swEstab", slot = c(useEstab = "logical", count = "integer"),
  contains = "swEstabSpecies")

setValidity("swEstab", function(object) {
  TRUE
})

#' @rdname swEstab-class
#' @export
setMethod("initialize", signature = "swEstab", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "estab")
  sns <- setdiff(slotNames("swEstab"), inheritedSlotNames("swEstab"))
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)

  .Object
})

#' @rdname swEstab-class
#' @export
setMethod("swEstab_useEstab", "swEstab", function(object) object@useEstab)
#' @rdname swEstab-class
#' @export
setReplaceMethod("swEstab_useEstab", signature = "swEstab",
  function(object, value) {
    object@useEstab <- as.logical(value)
    validObject(object)
    object
})


#' @rdname swEstab-class
#' @export
# nolint start
setMethod("swReadLines", signature = c(object="swEstab",file="character"), function(object,file) {
  print("TODO: method 'swReadLines' for class 'swInputData' is not up-to-date; hard-coded indices are incorrect")
			infiletext <- readLines(con = file[1])
			index<-length(object@fileName)+1
			object@useEstab = readLogical(infiletext[9])
			object@count = 0L
			if(object@useEstab) {
				infiletext <- infiletext[-c(1:9)]
				infiletext <- infiletext[infiletext != ""]
				for(i in 1:length(infiletext)) {
					#see if the line is commented out
					line<-gsub("[[:space:]]", "",strsplit(x=infiletext[i],split=c("#"))[[1]][1])
					if(line != "") {
						object@fileName <- c(object@fileName, line)
						object@count <- object@count + 1L
						as(object,"swEstabSpecies") <- swReadLines(as(object,"swEstabSpecies"),file.path(file[2],line))
					}
				}
			}
			return(object)
		})
# nolint end
