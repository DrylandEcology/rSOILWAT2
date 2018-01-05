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


########################CLOUD DATA################################
# TODO: consider individual slots for each row of the 5 x 12 matrix
#' @export
setClass("swCloud", slots = c(Cloud = "matrix"))


swCloud_validity <- function(object) {
  val <- TRUE
  temp <- dim(object@Cloud)

  if (temp[1] != 5) {
    msg <- paste("@Cloud must have exactly 5 rows corresponding to",
      "SkyCoverPCT, WindSpeed_m/s, HumidityPCT, Transmissivity, and SnowDensity_kg/m^3")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (temp[2] != 12) {
    msg <- paste("@Cloud must have exactly 12 columns corresponding months.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swCloud", swCloud_validity)

setMethod("initialize", signature = "swCloud", function(.Object, ...) {
  def <- slot(inputData, "cloud")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slot `Cloud` (except SnowDensity) if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("Cloud" %in% dns)) {
    def@Cloud[-5, ] <- NA_real_
  } else {
    # Guarantee dimnames
    dimnames(dots[["Cloud"]]) <- dimnames(def@Cloud)
  }

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})



setMethod("get_swCloud", "swCloud", function(object) object)
setMethod("swCloud_SkyCover", "swCloud", function(object) object@Cloud[1, ])
setMethod("swCloud_WindSpeed", "swCloud", function(object) object@Cloud[2, ])
setMethod("swCloud_Humidity", "swCloud", function(object) object@Cloud[3, ])
setMethod("swCloud_Transmissivity", "swCloud", function(object) object@Cloud[4, ])
setMethod("swCloud_SnowDensity", "swCloud", function(object) object@Cloud[5, ])

setReplaceMethod("set_swCloud", signature = "swCloud", function(object, value) {
  object <- value
  validObject(object)
  object
})
setReplaceMethod("swCloud_SkyCover", signature = "swCloud", function(object, value) {
  object@Cloud[1, ] <- value
  validObject(object)
  object
})
setReplaceMethod("swCloud_WindSpeed", signature = "swCloud", function(object, value) {
  object@Cloud[2, ] <- value
  validObject(object)
  object
})
setReplaceMethod("swCloud_Humidity", signature = "swCloud", function(object, value) {
  object@Cloud[3, ] <- value
  validObject(object)
  object
})
setReplaceMethod("swCloud_Transmissivity", signature = "swCloud", function(object, value) {
  object@Cloud[4, ] <- value
  validObject(object)
  object
})
setReplaceMethod("swCloud_SnowDensity", signature = "swCloud", function(object, value) {
  object@Cloud[5, ] <- value
  validObject(object)
  object
})


setMethod("swReadLines", signature = c(object="swCloud",file="character"), function(object,file) {
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]

			object@Cloud=matrix(data=NA,nrow=5,ncol=12,byrow=T)
			colnames(object@Cloud)<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
			rownames(object@Cloud)<-c("SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT", "Transmissivity", "SnowDensity_kg/m^3")

			for(i in 1:length(infiletext)) {
				object@Cloud[i,] <- readNumerics(infiletext[i],12)
			}
			return(object)
		})
