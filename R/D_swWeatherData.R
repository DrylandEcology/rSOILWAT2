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
###############################################################################

##############################################################################

#' @export
setClass("swWeatherData", slots = c(data = "matrix", year = "integer"))

swWeatherData_validity <- function(object) {
  val <- TRUE

  if (!(length(object@year) == 1 && is.finite(object@year) && object@year >= 0)) {
    msg <- "@year must be exactly one positive finite value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@data)
  if (temp[2] != 4) {
    msg <- paste("@data must have exactly 4 columns corresponding to",
      "DOY, Tmax_C, Tmin_C, PPT_cm")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (!(temp[1] %in% c(365, 366))) {
    msg <- paste("@data must 365 or 366 rows corresponding to day of year.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swWeatherData", swWeatherData_validity)

setMethod("initialize", signature = "swWeatherData", function(.Object, ...,
  year = 0L, data = NULL) {

  def <- slot(inputData, "weatherHistory")[[1]] # first year of weather data
  # We don't set values for slots `year` and `data`; this is to prevent simulation runs with
  # accidentally incorrect values

  if (is.null(data)) {
    data <- matrix(c(1:366, rep(rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_MISSING"]], 366 * 3)),
      nrow = 366, ncol = 4)
  }
  colnames(data) <- colnames(slot(def, "data"))
  .Object@data <- data

  .Object@year <- as.integer(year)

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

setMethod("swReadLines", signature = c(object="swWeatherData",file="character"), function(object,file) {
			object@year = as.integer(strsplit(x=basename(file),split=".",fixed=TRUE)[[1]][2])
			#data <-read.csv(file,header=FALSE,skip=2,sep="\t")
			data <- read.table(file, header=FALSE, comment.char="#", blank.lines.skip=TRUE, sep="\t")
			colnames(data)<-c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
			object@data = as.matrix(data)
			return(object)
		})
