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
# Author: ryan
###############################################################################


#######################Markov##########################################
#' @export
setClass("swMarkov", slots = c(Prob = "matrix", Conv = "matrix"))

setMethod("initialize", signature = "swMarkov", function(.Object, ...) {
  def <- slot(inputData, "markov")

  # We don't set values for slots `Prob` and `Conv`; this is to prevent simulation runs with
  # accidentally incorrect values

  # We have to explicitly give column names (as defined in `onGet_MKV_prob` and
  # `onGet_MKV_conv`) because they are not read in by C code if the weather generator is
  # turned off
  ctemp <- c("day", "wet", "dry", "avg_ppt", "std_ppt")
  temp <- matrix(NA_real_, nrow = 366, ncol = length(ctemp), dimnames = list(NULL, ctemp))
  temp[, "day"] <- 1:366
  .Object@Prob <- temp

  ctemp <- c("week", "t1", "t2", "t3", "t4", "t5", "t6")
  temp <- matrix(NA_real_, nrow = 53, ncol = length(ctemp), dimnames = list(NULL, ctemp))
  temp[, "week"] <- 1:53
  .Object@Conv <- temp

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})


setMethod("swReadLines", signature=c(object="swMarkov",file="character"), definition=function(object,file) {
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
