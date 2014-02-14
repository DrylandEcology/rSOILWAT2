# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################

print("swYears")
swYears<-setClass("swYears",representation(StartYear="integer",EndYear="integer",FDOFY="integer",EDOEY="integer",isNorth="logical"),prototype=prototype(StartYear=as.integer(1979),EndYear=as.integer(2010),FDOFY=as.integer(1),EDOEY=as.integer(365),isNorth=TRUE))
swYears_validity<-function(object){
	if(length(object@StartYear)!=1 | is.na(object@StartYear) | object@StartYear<0)
		return("@StartYear:Needstobeintegerlength1.Non-negativeandnotNA")	
	if(length(object@EndYear)!=1 | is.na(object@EndYear) | object@EndYear<0 | object@EndYear<object@StartYear)
		return("@EndYear:Needstobeintegerlength1.Non-negativeandnotNA")
	if(length(object@FDOFY)!=1 | is.na(object@FDOFY) | object@FDOFY<0 | object@FDOFY>365)
		return("@FDOFY:Needstobeintegerlength1.Non-negativeandnotNA.Lessthan365")
	if(length(object@EDOEY)!=1 | is.na(object@EDOEY) | object@EDOEY<0 | object@EDOEY>366 | object@EDOEY<object@FDOFY)
		return("@EDOEY:Needstobeintegerlength1.Non-negativeandnotNA.Lessthanorequalto366.GreaterthanFirstDayofFirstYear")
	if(length(object@isNorth)!=1)
		return("@isNorthneedstobelength1.")
	TRUE
}
setValidity("swYears",swYears_validity)
setMethod("initialize","swYears",function(.Object,StartYear=as.integer(1979),EndYear=as.integer(2010),FDOFY=as.integer(1),EDOEY=as.integer(365),isNorth=TRUE){
			.Object@StartYear=StartYear
			.Object@EndYear=EndYear
			.Object@FDOFY=FDOFY
			.Object@EDOEY=EDOEY
			.Object@isNorth=isNorth
			validObject(.Object)
			return(.Object)
		})
setMethod(f="swClear",
		signature="swYears",
		definition=function(object) {
			object@StartYear=integer(1)
			object@EndYear=integer(1)
			object@FDOFY=integer(1)
			object@EDOEY=integer(1)
			object@isNorth=logical(1)
			return(object)
		})
setMethod("swYears_StartYear", "swYears", function(object) {return(object@StartYear)})
setMethod("swYears_EndYear", "swYears", function(object) {return(object@EndYear)})
setMethod("swYears_FDOFY", "swYears", function(object) {return(object@FDOFY)})
setMethod("swYears_StartYear", "swYears", function(object) {return(object@EDOEY)})
setMethod("swYears_StartYear", "swYears", function(object) {return(object@isNorth)})
setReplaceMethod(f="swYears_StartYear", signature="swYears", definition=function(object,value) {object@StartYear <- value; return(object)})
setReplaceMethod(f="swYears_EndYear", signature="swYears", definition=function(object,value) {object@EndYear <- value; return(object)})
setReplaceMethod(f="swYears_FDOFY", signature="swYears", definition=function(object,value) {object@FDOFY <- value; return(object)})
setReplaceMethod(f="swYears_EDOEY", signature="swYears", definition=function(object,value) {object@EDOEY <- value; return(object)})
setReplaceMethod(f="swYears_isNorth", signature="swYears", definition=function(object,value) {object@isNorth <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swYears", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(8)
			infiletext[1] <- "# Model time definition file"
			infiletext[2] <- paste("# Location: ")
			
			infiletext[4] <- paste(object@StartYear, "\t# starting year (but see weather and swc inputs)", sep="")
			infiletext[5] <- paste(object@EndYear, "\t# ending year", sep="")
			infiletext[6] <- paste(object@FDOFY, "\t# first day of first year", sep="")
			infiletext[7] <- paste(object@EDOEY, "\t# ending day of last year", sep="")
			infiletext[8] <- paste(ifelse(object@isNorth,"N","S"), "\t# ending day of last year", sep="")
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swYears",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			object@StartYear = readInteger(infiletext[4])
			object@EndYear = readInteger(infiletext[5])
			object@FDOFY = readInteger(infiletext[6])
			object@EDOEY = readInteger(infiletext[7])
			temp <- unlist(strsplit(x=infiletext[8],split="\t"))
			temp <- unlist(strsplit(x=temp,split=" "))
			temp <- temp[temp != ""][1]
			if(temp == "N") object@isNorth = TRUE
			if(temp == "S") object@isNorth = FALSE			
			return(object)
		})
