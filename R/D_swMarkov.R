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
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso
#' \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}}
#' \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}}
#' \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}}
#' \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swSpinup}}
#' \code{\linkS4class{swEstab}}
#' \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swCarbon}}
#' \code{\linkS4class{swSWC}}
#' \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swMarkov")
#' x <- new("swMarkov")
#' x <- swMarkov()
#'
#' @name swMarkov-class
#' @export
setClass(
  "swMarkov",
  slots = c(Prob = "matrix", Conv = "matrix"),
  prototype = list(
    Prob = array(
      NA_real_,
      dim = c(366, 5),
      dimnames = list(
        NULL,
        c("DOY", "p_wet_wet", "p_wet_dry", "avg_ppt", "std_ppt")
      )
    ),
    Conv = array(
      NA_real_,
      dim = c(53, 11),
      dimnames = list(
        NULL,
        c(
          "WEEK", "wTmax_C", "wTmin_C", "var_wTmax", "cov_wTmaxmin",
          "cov_wTminmax", "var_wTmin", "cfmax_wet", "cfmax_dry", "cfmin_wet",
          "cfmin_dry"
        )
      )
    )
  )
)

#' @rdname swMarkov-class
#' @export
swMarkov <- function(...) {
  # Copy from SOILWAT2 "testing", but dot arguments take precedence
  def <- slot(rSOILWAT2::sw_exampleData, "markov")
  sns <- slotNames("swMarkov")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swMarkov")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slots `Prob` and `Conv`; this is to prevent
  # simulation runs with accidentally incorrect values

  # We have to explicitly give column names (as defined in `onGet_MKV_prob` and
  # `onGet_MKV_conv`) because they are not read in by C code if the weather
  # generator is turned off
  tmp <- new("swMarkov")
  ctemp_Prob <- colnames(slot(tmp, "Prob"))
  ctemp_Conv <- colnames(slot(tmp, "Conv"))

  if ("Prob" %in% dns) {
    tmp <- dots[["Prob"]]
    if (sum(dim(tmp)) > 0) {
      colnames(tmp) <- ctemp_Prob
    }
  } else {
    tmp <- array(
      NA_real_,
      dim = c(366, length(ctemp_Prob)),
      dimnames = list(NULL, ctemp_Prob)
    )
    tmp[, "DOY"] <- 1:366
  }
  dots[["Prob"]] <- tmp

  if ("Conv" %in% dns) {
    tmp <- dots[["Conv"]]
    if (sum(dim(tmp)) > 0) {
      colnames(tmp) <- ctemp_Conv
    }
  } else {
    tmp <- array(
      NA_real_,
      dim = c(53, length(ctemp_Conv)),
      dimnames = list(NULL, ctemp_Conv)
    )
    tmp[, "WEEK"] <- 1:53
  }
  dots[["Conv"]] <- tmp

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swMarkov", tmp))
}



setValidity(
  "swMarkov",
  function(object) {
    val <- TRUE

    temp <- dim(object@Prob)
    if (
      !isTRUE(all.equal(temp, c(0, 0))) &&
      !isTRUE(all.equal(temp, c(366, 5)))
    ) {
      msg <- "@Prob must be a 0x0 or a 366x5 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@Conv)
    if (
      !isTRUE(all.equal(temp, c(0, 0))) &&
      !isTRUE(all.equal(temp, c(53, 11)))
    ) {
      msg <- "@Conv must be a 0x0 or a 53x11 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)


# use `get_swMarkov()`; `get_Markov()` is a legacy name
#' @rdname swMarkov-class
#' @export
setMethod("get_Markov", "swMarkov", function(object) object)

#' @rdname swMarkov-class
#' @export
setMethod("get_swMarkov", "swMarkov", function(object) object)

#' @rdname swMarkov-class
#' @export
setMethod("swMarkov_Prob", "swMarkov", function(object) object@Prob)
#' @rdname swMarkov-class
#' @export
setMethod("swMarkov_Conv", "swMarkov", function(object) object@Conv)

# use `set_swMarkov()`; `set_Markov()` is a legacy name
#' @rdname swMarkov-class
#' @export
setReplaceMethod(
  "set_Markov",
  signature = "swMarkov", function(object, value) {
    set_swMarkov(object) <- value
    object
  }
)

#' @rdname swMarkov-class
#' @export
setReplaceMethod(
  "set_swMarkov",
  signature = "swMarkov", function(object, value) {
    if (ncol(value@Prod) == ncol(object@Prob)) {
      dimnames(value@Prob) <- dimnames(object@Prob)
    }
    if (ncol(value@Conv) == ncol(object@Conv)) {
      dimnames(value@Conv) <- dimnames(object@Conv)
    }
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swMarkov-class
#' @export
setReplaceMethod(
  "swMarkov_Prob",
  signature = "swMarkov",
  function(object, value) {
    if (ncol(value) == ncol(object@Prob)) {
      colnames(value) <- dimnames(object@Prob)[[2]]
    }
    object@Prob <- as.matrix(value)
    validObject(object)
    object
  }
)

#' @rdname swMarkov-class
#' @export
setReplaceMethod(
  "swMarkov_Conv",
  signature = "swMarkov",
  function(object, value) {
    if (ncol(value) == ncol(object@Conv)) {
      colnames(value) <- dimnames(object@Conv)[[2]]
    }
    object@Conv <- as.matrix(value)
    validObject(object)
    object
  }
)


#' @rdname swMarkov-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object = "swMarkov", file = "character"),
  function(object, file) {
    stop("swReadLines is defunct", call. = FALSE)
    id_skip <- 1:2

    infiletext <- readLines(con = file[1])
    infiletext <- infiletext[-id_skip]
    if (length(infiletext) != 366)
      stop("Markov Prod wrong number of lines", call. = FALSE)

    object@Prob <- matrix(0, 366, 5)
    for (i in seq_len(366)) {
      object@Prob[i, ] <- readNumerics(infiletext[i], 5)
    }

    infiletext <- readLines(con = file[2])
    infiletext <- infiletext[-id_skip]
    if (length(infiletext) != 53)
      stop("Markov Prod wrong number of lines", call. = FALSE)

    object@Conv <- matrix(0, 53, 11)
    for (i in seq_len(53)) {
      object@Conv[i, ] <- readNumerics(infiletext[i], 11)
    }

    object
  }
)
# nolint end
