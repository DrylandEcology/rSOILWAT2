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

setMethod(f="swClear",
		signature="swMarkov",
		definition=function(object) {
			object@Prob=matrix(numeric(0),0,0)
			object@Conv=matrix(numeric(0),0,0)
			return(object)
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
