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


#######################Markov##########################################
#' @export
setClass("swMarkov", slots = c(Prob = "matrix", Conv = "matrix"))

setMethod("initialize", signature = "swMarkov", function(.Object, ...) {
  def <- slot(inputData, "markov")
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slots `Prob` and `Conv`; this is to prevent simulation runs with
  # accidentally incorrect values

  # We have to explicitly give column names (as defined in `onGet_MKV_prob` and
  # `onGet_MKV_conv`) because they are not read in by C code if the weather generator is
  # turned off
  ctemp_Prob <- c("day", "wet", "dry", "avg_ppt", "std_ppt")
  ctemp_Conv <- c("week", "t1", "t2", "t3", "t4", "t5", "t6")

  if ("Prob" %in% dns) {
    temp <- dots[["Prob"]]
    if (sum(dim(temp)) > 0) {
      colnames(temp) <- ctemp_Prob
    }
  } else {
    temp <- matrix(NA_real_, nrow = 366, ncol = length(ctemp_Prob),
      dimnames = list(NULL, ctemp_Prob))
    temp[, "day"] <- 1:366
  }
  .Object@Prob <- temp

  if ("Conv" %in% dns) {
    temp <- dots[["Conv"]]
    if (sum(dim(temp)) > 0) {
      colnames(temp) <- ctemp_Conv
    }
  } else {
    temp <- matrix(NA_real_, nrow = 53, ncol = length(ctemp_Conv),
      dimnames = list(NULL, ctemp_Conv))
    temp[, "week"] <- 1:53
  }
  .Object@Conv <- temp

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

swMarkov_validity <- function(object) {
  val <- TRUE

  temp <- dim(object@Prob)
  if (!(temp == c(0, 0) || temp == c(366, 5))) {
    msg <- paste("@Prob must be a 0x0 or a 366x5 matrix.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@Conv)
  if (!(temp == c(0, 0) || temp == c(53, 7))) {
    msg <- paste("@Conv must be a 0x0 or a 53x7 matrix.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swMarkov", swMarkov_validity)


setMethod("get_Markov", "swMarkov", function(object) object)
setMethod("swMarkov_Prob", "swMarkov", function(object) object@Prob)
setMethod("swMarkov_Conv", "swMarkov", function(object) object@Conv)

setReplaceMethod("set_Markov", signature = "swMarkov", function(object, value) {
  object <- value
  validObject(object)
  object
})
setReplaceMethod("swMarkov_Prob", signature = "swMarkov", function(object, value) {
  object@Prob <- value
  validObject(object)
  object
})
setReplaceMethod("swMarkov_Conv", signature = "swMarkov", function(object, value) {
  object@Conv <- value
  validObject(object)
  object
})


setMethod("swReadLines", signature = c(object="swMarkov",file="character"), function(object,file) {
			infiletext <- readLines(con = file[1])
			infiletext <- infiletext[-(1:2)]
			if(length(infiletext) != 366)
				stop("Markov Prod wrong number of lines")

			object@Prob=matrix(0,366,5)
			for(i in 1:366) {
				object@Prob[i,]<- readNumerics(infiletext[i],5)
			}

			infiletext <- readLines(con = file[2])
			infiletext <- infiletext[-(1:2)]
			if(length(infiletext) != 53)
				stop("Markov Prod wrong number of lines")

			object@Conv=matrix(0,53,7)
			for(i in 1:366) {
				object@Conv[i,]<- readNumerics(infiletext[i],7)
			}
			return(object)
		})
