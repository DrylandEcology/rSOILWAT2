###############################################################################
#Rsoilwat and Rsoilwat31
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
print("swMarkov")
swMarkov <- setClass("swMarkov", representation(Prob="matrix",Conv="matrix"))
setMethod(f="swClear",
		signature="swMarkov",
		definition=function(object) {
			object@Prob=matrix(numeric(0),0,0)
			object@Conv=matrix(numeric(0),0,0)
			return(object)
		})

setMethod("swWriteLines", signature=c(object="swMarkov", file="character"), definition=function(object, file) {
			infilename <- file.path(file[1])
			infiletext <- character(366+2)
			infiletext[1] = "# Markov Prob In v1.0 (RJM) 2015 update"
			infiletext[2] = "# day		wet		dry		avg		std"
			for(i in 1:366) {
				infiletext[2+i] = paste(object@Prob[i,],collapse=" ")
			}
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
			
			infilename <- file.path(file[2])
			infiletext <- character(53+2)
			infiletext[1] = "# Markov Covariance In v1.0 (RJM) 2015 update"
			infiletext[2] = "# week		u_cov1		u_cov2		v_cov1		v_cov2		v_cov3		v_cov4"
			for(i in 1:53) {
				infiletext[2+i] = paste(object@Conv[i,],collapse=" ")
			}
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
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
