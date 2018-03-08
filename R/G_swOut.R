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



#######
#' Class \code{"swOUT_key"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param .Object An object of class \code{\linkS4class{swOUT_key}}.
#' @param ... Further arguments to methods.
#'
#' @examples
#' showClass("swOUT_key")
#' x <- new("swOUT_key")
#'
#' @name swOUT_key-class
#' @export
setClass("swOUT_key", slots = c(mykey = "integer", myobj = "integer",
  sumtype = "integer", use = "logical", first_orig = "integer", last_orig = "integer",
  outfile = "character"))

swOUT_key_validity <- function(object) {
  val <- TRUE

  temp <- lengths(lapply(slotNames(object), function(x) slot(object, x)))

  id <- temp != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]

  if (any(id)) {
    msg <- paste0(names(temp)[id], " must be a vector of length 'SW_OUTNKEYS'")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swOUT_key", swOUT_key_validity)


#' @rdname swOUT_key-class
#' @export
setMethod("initialize", signature = "swOUT_key", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "output")
  sns <- slotNames("swOUT_key")
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})


###########################OUTSETUP.IN########################################

#' Class \code{swOUT}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swOUT}}.
#' @param .Object An object of class \code{\linkS4class{swOUT}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @slot outputSeparator A character string. Currently, only "\\t" is functional.
#' @slot timeSteps An integer matrix. See details.
#'
#' @details Output can be generated for four different time steps: daily (DY),
#'  weekly (WK), monthly (MO), and yearly (YR) periods.
#'  We have two options to specify time steps:\itemize{
#'    \item The same time step(s) for every output; this option corresponds to specifying
#'        a line with `TIMESTEP ...` in the SOILWAT2 input file `outsetup.in`. The matrix
#'        in slot `timeSteps` should have `SW_OUTNKEYS` rows and `used_SW_OUTNPERIODS`
#'        columns where each row contains identical values.
#'    \item A different time step for each output; however, only one time step per
#'        output variable can be specified. this option corresponds to specifying the
#'        time step in the column `PERIOD` in the SOILWAT2 input file `outsetup.in`. The
#'        matrix in slot `timeSteps` should have `SW_OUTNKEYS` rows and 1 column.
#' }
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swOUT")
#' x <- new("swOUT")
#'
#' @name swOUT-class
#' @export
setClass("swOUT", slot = c(outputSeparator = "character", timeSteps = "matrix"),
  contains = "swOUT_key")

swOUT_validity <- function(object) {
  val <- TRUE

  if (length(object@outputSeparator) != 1) {
    msg <- "@outputSeparator needs to be of length 1."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(dim(object@timeSteps)) != 2) {
    msg <- "@timeSteps must be a 2-dimensional matrix"
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (nrow(object@timeSteps) != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]) {
    msg <- "@timeSteps must be a matrix with 'SW_OUTNKEYS' rows"
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  ok <- c(rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_MISSING"]],
    seq_len(rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]) - 1L) # timeSteps is base0
  if (!all(object@timeSteps %in% ok)) {
    msg <- "@timeSteps values must be within SW_OUTNPERIODS or be equal to SW_MISSING"
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swOUT", swOUT_validity)


#' @rdname swOUT-class
#' @export
setMethod("initialize", signature = "swOUT", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "output")
  sns <- setdiff(slotNames("swOUT"), inheritedSlotNames("swOUT"))
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)
  .Object
})


#' @rdname swOUT-class
#' @export
setMethod("get_swOUT", "swOUT", function(object) object)
#' @rdname swOUT-class
#' @export
setMethod("swOUT_TimeStep", "swOUT", function(object) object@timeSteps)
#' @rdname swOUT-class
#' @export
setMethod("swOUT_OutputSeparator", "swOUT", function(object) object@outputSeparator)

#' @rdname swOUT-class
#' @export
setReplaceMethod("set_swOUT", signature = "swOUT", function(object, value) {
  object <- value
  validObject(object)
  object
})
#' @rdname swOUT-class
#' @export
setReplaceMethod("swOUT_TimeStep", signature = "swOUT", function(object, value) {
  object@timeSteps <- value
  validObject(object)
  object
})

#' Set time steps to the same set of values for each output key.
#' @examples
#' x <- new("swOUT")
#' swOUT_TimeStepsForEveryKey(x) <- c(2, 3)
#' identical(as.vector(unique(swOUT_TimeStep(x))), as.integer(c(2, 3)))
#' @rdname swOUT-class
#' @export
setReplaceMethod("swOUT_TimeStepsForEveryKey", signature = "swOUT", function(object, value) {
  object@timeSteps <- matrix(as.integer(value), byrow = TRUE,
    nrow = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]],
    ncol = length(value))
  validObject(object)
  object
})

#' @rdname swOUT-class
#' @export
setReplaceMethod("swOUT_OutputSeparator", signature = "swOUT", function(object, value) {
  object@outputSeparator <- as.character(value)
  validObject(object)
  object
})


# used by swReadLines
			KEY <- c("WTHR", "TEMP", "PRECIP", "SOILINFILT", "RUNOFF", "ALLH2O", "VWCBULK",
				"VWCMATRIC", "SWCBULK", "SWA", "SWABULK", "SWAMATRIC", "SWPMATRIC", "SURFACEWATER", "TRANSP",
				"EVAPSOIL", "EVAPSURFACE", "INTERCEPTION", "LYRDRAIN", "HYDRED", "ET", "AET", "PET",
				"WETDAY", "SNOWPACK", "DEEPSWC", "SOILTEMP", "ALLVEG", "ESTABL")
OutSum <- c("off", "sum", "avg", "fnl") # only used for 'swReadLines'
#Remember this models the C code so index starts at 0 not 1
timePeriods <- c("dy", "wk", "mo", "yr")


#' @rdname swOUT-class
#' @export
setMethod("swReadLines", signature = c(object="swOUT",file="character"), function(object,file) {
  print("TODO: method 'swReadLines' for class 'swOUT' is not up-to-date; hard-coded indices are incorrect")

			infiletext <- readLines(con = file)
			if(temp<-strsplit(infiletext[41],split=" ")[[1]][2] == "t") {
				object@outputSeparator="\t"
			} else if(temp == "s") {
				object@outputSeparator=" "
			} else {
				object@outputSeparator="\t"
			}

			if(infiletext[42]==""){
				useTimeStep = FALSE
			} else {
				useTimeStep = TRUE
				temp<-strsplit(x=infiletext[42],split=" ")[[1]][-1]
				object@timeSteps = as.integer(sapply(1:length(temp), FUN=function(i) which(temp[i] == timePeriods))-1)
			}

			for(i in 45:length(infiletext)) {
				if(infiletext[i] != "") {
					temp<-strsplit(x=infiletext[i],split="\t")[[1]]
					temp<-unlist(strsplit(x=temp,split=" "))
					temp <- temp[temp != ""][1:6]
					mykey<- as.integer(grep(pattern=temp[1],x=KEY)[1])
					sumtype <- as.integer(grep(pattern=temp[2],x=OutSum))-1
					period <- which(tolower(temp[3]) == timePeriods)-1
					start <- as.integer(temp[4])
					if(grepl(pattern="end",x=temp[5])) {
						end <- as.integer(366)
					} else {
						end <- as.integer(temp[5])
					}
					object@mykey[mykey] = as.integer(mykey-1)
					object@sumtype[mykey] = as.integer(sumtype)
					object@first_orig[mykey] = start
					object@last_orig[mykey] = end
					object@outfile[mykey] = temp[6]
					if(object@sumtype[mykey] != 0) {
						object@use[mykey] = TRUE
					} else {
						object@use[mykey] = FALSE
					}
				}
			}
			return(object)
		})

