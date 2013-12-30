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

