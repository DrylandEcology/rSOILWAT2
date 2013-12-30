# TODO: Add comment
# 
# Author: ryan
###############################################################################


#######################Markov##########################################

swMarkov <- setClass("swMarkov", representation(Prob="matrix",Conv="matrix"))
setMethod(f="swClear",
		signature="swMarkov",
		definition=function(object) {
			object@Prob=matrix(numeric(0),0,0)
			object@Conv=matrix(numeric(0),0,0)
			return(object)
		})

