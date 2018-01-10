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


#' @export
setClass("swSWC_hist", slot = c(data = "matrix", year = "integer"))


setMethod("initialize", signature = "swSWC_hist", function(.Object, ..., year = 0L,
  data = NULL) {
  # We don't set values; this is to prevent simulation runs with
  # accidentally incorrect values

  # We have to explicitly give column names (as defined in `onGet_SW_SWC_hist`)
  # because they are not read in by C code if the historical soil moisture data are not
  # provided as input
  ctemp <- c("doy", "lyr", "swc", "st_err")
  if (is.null(data)) {
    data <- matrix(NA_real_, nrow = 366, ncol = length(ctemp))
    data[, "doy"] <- 1:366
  }
  colnames(data) <- ctemp
  .Object@data <- data

  .Object@year <- as.integer(year)

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})



setMethod("swReadLines", signature = c(object="swSWC_hist",file="character"), function(object,file) {
			object@year = as.integer(strsplit(x=file,split=".",fixed=TRUE)[[1]][2])
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]
			days <- (length(infiletext)-2)
			data=matrix(data=NA,nrow=days,ncol=4)
			colnames(data)<-c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
			for(i in 3:length(infiletext)) {
				data[i-2,] <- readNumerics(infiletext[i],4)
			}
			object@data = data
			return(object)
		})


##########################swcsetup.in#########################################

#' @export
setClass("swSWC", slot = c(UseSWCHistoricData = "logical", DataFilePrefix = "character",
  FirstYear = "integer", Method = "integer", History = "list"))

setMethod("initialize", signature = "swSWC", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "swc")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slot `History` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("History" %in% dns)) {
    def@History <- list()
  } else {
    # Guarantee dimnames
    dimnames(dots[["History"]]) <- dimnames(def@History)
  }

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

setMethod("swSWC_use", "swSWC", function(object) object@UseSWCHistoricData)
setMethod("swSWC_prefix", "swSWC", function(object) object@DataFilePrefix)
setMethod("swSWC_FirstYear", "swSWC", function(object) object@FirstYear)
setMethod("swSWC_Method", "swSWC", function(object) object@Method)
setMethod("swSWC_HistoricList", "swSWC", function(object) object@History)
setMethod("swSWC_HistoricData", "swSWC", function(object, year) {
  index <- which(names(object@History) == as.character(year))
  if (length(index) != 1) {
    print("swc historic data Index has wrong length.")
    return(NULL)
  }
  if (object@History[[index]]@year != as.integer(year))
    print("Somethings wrong with the historical soil moisture data.")

  object@History[[index]]
})

setReplaceMethod("swSWC_use", signature = c(object = "swSWC", value = "logical"),
  function(object, value) {
    object@UseSWCHistoricData[] <- value
    validObject(object)
    object
})
setReplaceMethod("swSWC_prefix", signature = c(object = "swSWC", value = "character"),
  function(object, value) {
    object@DataFilePrefix <- as.character(value)
    validObject(object)
    object
})
setReplaceMethod("swSWC_FirstYear", signature = c(object = "swSWC", value = "integer"),
  function(object, value) {
    object@FirstYear <- as.integer(value)
    validObject(object)
    object
})
setReplaceMethod("swSWC_Method", signature = c(object = "swSWC", value = "integer"),
  function(object, value) {
    object@Method <- as.integer(value)
    validObject(object)
    object
})
setReplaceMethod("swSWC_HistoricList", signature = c(object = "swSWC", value = "list"),
  function(object, value) {
    object@History <- value
    validObject(object)
    object
})
setReplaceMethod("swSWC_HistoricData", signature = c(object = "swSWC", value = "swSWC_hist"),
function(object, value) {
			index<-which(names(object@History) == as.character(value@year))
			if(length(index) == 0) {
				object@History[[length(object@History)+1]] <- value
				years <- unlist(lapply(object@History, function(x) {return(x@year)}))
				object@History <- object@History[order(years)]
				names(object@History)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("SWC data is Missing")
			} else if(length(index) == 1) {
				object@History[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})


setMethod("swReadLines", signature = c(object="swSWC",file="character"), function(object,file) {
			infiletext <- readLines(con = file)
			#should be no empty lines
			infiletext <- infiletext[infiletext != ""]
			object@UseSWCHistoricData = readLogical(infiletext[4])
			object@DataFilePrefix = readCharacter(infiletext[5])
			object@FirstYear = readInteger(infiletext[6])
			object@Method = readInteger(infiletext[7])
			return(object)
		})

