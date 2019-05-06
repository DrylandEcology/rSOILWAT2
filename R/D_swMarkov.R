###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
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
#' Class \code{"swMarkov"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swMarkov}}.
#' @param .Object An object of class \code{\linkS4class{swMarkov}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swMarkov")
#' x <- new("swMarkov")
#'
#' @name swMarkov-class
#' @export
setClass("swMarkov", slots = c(Prob = "matrix", Conv = "matrix"))

#' @rdname swMarkov-class
#' @export
setMethod("initialize", signature = "swMarkov", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "markov")
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slots `Prob` and `Conv`; this is to prevent
  # simulation runs with accidentally incorrect values

  # We have to explicitly give column names (as defined in `onGet_MKV_prob` and
  # `onGet_MKV_conv`) because they are not read in by C code if the weather
  # generator is turned off
  ctemp_Prob <- c("DOY", "p_wet_wet", "p_wet_dry", "avg_ppt", "std_ppt")
  ctemp_Conv <- c("WEEK", "wTmax_C", "wTmin_C", "var_wTmax",
    "cov_wTmaxmin", "cov_wTminmax", "var_wTmin",
    "cfmax_wet", "cfmax_dry", "cfmin_wet", "cfmin_dry")

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

  if (FALSE) {
    # not needed because no relevant inheritance
    .Object <- callNextMethod(.Object, ...)
  }

  validObject(.Object)
  .Object
})

swMarkov_validity <- function(object) {
  val <- TRUE

  temp <- dim(object@Prob)
  if (!isTRUE(all.equal(temp, c(0, 0))) &&
      !isTRUE(all.equal(temp, c(366, 5)))) {
    msg <- paste("@Prob must be a 0x0 or a 366x5 matrix.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@Conv)
  if (!isTRUE(all.equal(temp, c(0, 0))) &&
      !isTRUE(all.equal(temp, c(53, 11)))) {
      msg <- paste("@Conv must be a 0x0 or a 53x11 matrix.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swMarkov", swMarkov_validity)


#' @rdname swMarkov-class
#' @export
setMethod("get_Markov", "swMarkov", function(object) object)
#' @rdname swMarkov-class
#' @export
setMethod("swMarkov_Prob", "swMarkov", function(object) object@Prob)
#' @rdname swMarkov-class
#' @export
setMethod("swMarkov_Conv", "swMarkov", function(object) object@Conv)

#' @rdname swMarkov-class
#' @export
setReplaceMethod("set_Markov", signature = "swMarkov", function(object, value) {
  if (ncol(value@Prod) == ncol(object@Prob)) {
    dimnames(value@Prob) <- dimnames(object@Prob)
  }
  if (ncol(value@Conv) == ncol(object@Conv)) {
    dimnames(value@Conv) <- dimnames(object@Conv)
  }
  object <- value
  validObject(object)
  object
})

#' @rdname swMarkov-class
#' @export
setReplaceMethod("swMarkov_Prob", signature = "swMarkov",
  function(object, value) {
    if (ncol(value) == ncol(object@Prob)) {
      colnames(value) <- dimnames(object@Prob)[[2]]
    }
    object@Prob <- as.matrix(value)
    validObject(object)
    object
})

#' @rdname swMarkov-class
#' @export
setReplaceMethod("swMarkov_Conv", signature = "swMarkov",
  function(object, value) {
    if (ncol(value) == ncol(object@Conv)) {
      colnames(value) <- dimnames(object@Conv)[[2]]
    }
    object@Conv <- as.matrix(value)
    validObject(object)
    object
})


#' @rdname swMarkov-class
#' @export
setMethod("swReadLines", signature = c(object = "swMarkov", file = "character"),
  function(object, file) {
    id_skip <- 1:2

    infiletext <- readLines(con = file[1])
    infiletext <- infiletext[-id_skip]
    if (length(infiletext) != 366)
      stop("Markov Prod wrong number of lines")

    object@Prob <- matrix(0, 366, 5)
    for (i in seq_len(366)) {
      object@Prob[i, ] <- readNumerics(infiletext[i], 5)
    }

    infiletext <- readLines(con = file[2])
    infiletext <- infiletext[-id_skip]
    if (length(infiletext) != 53)
      stop("Markov Prod wrong number of lines")

    object@Conv <- matrix(0, 53, 11)
    for (i in seq_len(366)) {
      object@Conv[i, ] <- readNumerics(infiletext[i], 11)
    }

    object
})
