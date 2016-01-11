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
# Author: Ryan J. Murphy (2013)
###############################################################################


print("swLog")
swLog <- setClass(Class="swLog", representation(LogData="character",MaxLines="integer",UsedLines="integer"), prototype=prototype(LogData=character(150),MaxLines=as.integer(150),UsedLines=as.integer(1)))
setMethod(f="swClear",
		signature="swLog",
		definition=function(object) {
			object@LogData=character(150)
			object@MaxLines=as.integer(150)
			object@UsedLines=integer(1)
			return(object)
		})

