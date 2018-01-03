###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2016}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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


#' @export
setClass("swEstabSpecies", slot = c(fileName = "character", Name = "character",
  estab_lyrs = "integer", barsGERM = "numeric", barsESTAB = "numeric",
  min_pregerm_days = "integer", max_pregerm_days = "integer",
  min_wetdays_for_germ = "integer", max_drydays_postgerm = "integer",
  min_wetdays_for_estab = "integer", min_days_germ2estab = "integer",
  max_days_germ2estab = "integer", min_temp_germ = "numeric", max_temp_germ = "numeric",
  min_temp_estab = "numeric", max_temp_estab = "numeric"))

setValidity("swEstabSpecies", function(object) {
  temp <- c(object@fileName, object@Name, object@estab_lyrs, object@barsGERM,
    object@barsESTAB, object@min_pregerm_days, object@max_pregerm_days,
    object@min_wetdays_for_germ, object@max_drydays_postgerm, object@min_wetdays_for_estab,
    object@min_days_germ2estab, object@max_days_germ2estab, object@min_temp_germ,
    object@max_temp_germ, object@min_temp_estab, object@max_temp_estab)

  if (any(!lapply(temp, function(x) length(x) == 1)))
    return("Missing values...")

  TRUE
})

setMethod("initialize", signature = "swEstabSpecies", function(.Object, ...) {
  def <- slot(inputData, "estab")

  .Object@fileName <- def@fileName
  .Object@Name <- def@Name
  .Object@estab_lyrs <- def@estab_lyrs
  .Object@barsGERM <- def@barsGERM
  .Object@barsESTAB <- def@barsESTAB
  .Object@min_pregerm_days <- def@min_pregerm_days
  .Object@max_pregerm_days <- def@max_pregerm_days
  .Object@min_wetdays_for_germ <- def@min_wetdays_for_germ
  .Object@max_drydays_postgerm <- def@max_drydays_postgerm
  .Object@min_wetdays_for_estab <- def@min_wetdays_for_estab
  .Object@min_days_germ2estab <- def@min_days_germ2estab
  .Object@max_days_germ2estab <- def@max_days_germ2estab
  .Object@min_temp_germ <- def@min_temp_germ
  .Object@max_temp_germ <- def@max_temp_germ
  .Object@min_temp_estab <- def@min_temp_estab
  .Object@max_temp_estab <- def@max_temp_estab

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})


setMethod("swReadLines", signature=c(object="swEstabSpecies",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)

			object@Name = c(object@Name, gsub("[[:space:]]","",strsplit(x=infiletext[1],split = c("#"," ", "\t"),fixed=F)[[1]][1]))
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

#############################ESTAB.IN#########################################
#' @export
setClass("swEstab", slot = c(useEstab = "logical", count = "integer"),
  contains = "swEstabSpecies")

setMethod("initialize", signature = "swEstab", function(.Object, ...) {
  def <- slot(inputData, "estab")

  .Object@useEstab <- def@useEstab
  .Object@count <- def@count

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)
  .Object
})

setMethod("swEstab_useEstab", "swEstab", function(object) {return(object@useEstab)})
setReplaceMethod(f="swEstab_useEstab", signature="swEstab", definition=function(object,value) {object@useEstab <- value; return(object)})


setMethod("swReadLines", signature=c(object="swEstab",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file[1])
			index<-length(object@fileName)+1
			object@useEstab = readLogical(infiletext[9])
			object@count = 0L
			if(object@useEstab) {
				infiletext <- infiletext[-c(1:9)]
				infiletext <- infiletext[infiletext != ""]
				for(i in 1:length(infiletext)) {
					#see if the line is commented out
					line<-gsub("[[:space:]]","",strsplit(x=infiletext[i],split=c("#"))[[1]][1])
					if(line != "") {
						object@fileName <- c(object@fileName, line)
						object@count <- object@count + 1L
						as(object,"swEstabSpecies") <- swReadLines(as(object,"swEstabSpecies"),file.path(file[2],line))
					}
				}
			}
			return(object)
		})

