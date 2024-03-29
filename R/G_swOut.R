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



#######
#' Class \code{"swOUT_key"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @examples
#' showClass("swOUT_key")
#' x <- new("swOUT_key")
#' x <- swOUT_key()
#'
#' @name swOUT_key-class
#' @export
setClass(
  "swOUT_key",
  slots = c(
    mykey = "integer",
    myobj = "integer",
    sumtype = "integer",
    use = "logical",
    first_orig = "integer",
    last_orig = "integer",
    outfile = "character"
  ),
  # TODO: lengths must be rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
  prototype = list(
    mykey = rep(NA_integer_, 32L),
    myobj = rep(NA_integer_, 32L),
    sumtype = rep(NA_integer_, 32L),
    use = rep(NA, 32L),
    first_orig = rep(NA_integer_, 32L),
    last_orig = rep(NA_integer_, 32L),
    outfile = rep(NA_character_, 32L)
  )
)

setValidity(
  "swOUT_key",
  function(object) {
    val <- TRUE

    temp <- lengths(lapply(slotNames(object), function(x) slot(object, x)))

    id <- temp != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]

    if (any(id)) {
      msg <- paste0(
        names(temp)[id],
        " must be a vector of length 'SW_OUTNKEYS'"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)


#' @rdname swOUT_key-class
#' @export
swOUT_key <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "output")
  sns <- slotNames("swOUT_key")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swOUT_key")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swOUT_key", tmp))
}


###########################OUTSETUP.IN########################################

#' Class \code{swOUT}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swOUT}}.
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
#' @slot outputSeparator A character string. Currently, only \var{"\\t"} is
#'   functional.
#' @slot timeSteps An integer matrix with rows for each output key and columns
#'   for each possible output time period. See details.
#'
#' @details Output can be generated for four different time steps:
#'   daily (\var{DY}), weekly (\var{WK}), monthly (\var{MO}), and yearly
#'   (\var{YR}) periods.
#'  We have two options to specify time steps: \itemize{
#'    \item The same time step(s) for every output; this option corresponds
#'        to specifying a line with \code{TIMESTEP ...} in the \pkg{SOILWAT2}
#'        input file \var{outsetup.in}. The matrix in slot \var{timeSteps}
#'        should have \var{SW_OUTNKEYS} rows and \var{SW_OUTNPERIODS}
#'        columns of which \var{use_OUTNPERIODS} contain identical values in
#'        each used row.
#'    \item A different time step for each output; however, only one time
#'        step per output variable can be specified. this option corresponds to
#'        specifying the time step in the column \var{PERIOD} in the
#'        \pkg{SOILWAT2} input file \var{outsetup.in}.
#' }
#' Elements that are turned off have the value \code{eSW_NoTime = 999L}.
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swOUT")
#' x <- new("swOUT")
#' x <- swOUT()
#'
#' @name swOUT-class
#' @export
setClass(
  "swOUT",
  slot = c(
    outputSeparator = "character",
    timeSteps = "matrix"
  ),
  contains = "swOUT_key",
  prototype = list(
    outputSeparator = NA_character_,
    # timeSteps:
    #   * 999 must be rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]]
    #   * nrows = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
    #   * ncols = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
    timeSteps = array(999, dim = c(32L, 4L))
  )
)


setValidity(
  "swOUT",
  function(object) {
    val <- TRUE

    if (length(object@outputSeparator) != 1L) {
      msg <- "@outputSeparator needs to be of length 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(dim(object@timeSteps)) != 2L) {
      msg <- "@timeSteps must be a 2-dimensional matrix"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      nrow(object@timeSteps) !=
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
    ) {
      msg <- "@timeSteps must be a matrix with 'SW_OUTNKEYS' rows"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      ncol(object@timeSteps) !=
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
    ) {
      msg <- "@timeSteps must be a matrix with 'SW_OUTNPERIODS' columns"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    # timeSteps is base0
    ok <- c(
      rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]],
      seq_len(rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]) - 1L
    )

    if (!all(object@timeSteps %in% ok)) {
      msg <- paste(
        "@timeSteps values must be within SW_OUTNPERIODS or be",
        "equal to eSW_NoTime"
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)


#' @rdname swOUT-class
#' @export
swOUT <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "output")
  sns <- setdiff(slotNames("swOUT"), inheritedSlotNames("swOUT"))
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swOUT")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- setdiff(names(dots), inheritedSlotNames("swOUT"))

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call(
    "new",
    args = c(
      "swOUT",
      if ("swOUT_key" %in% dns) {
        swOUT_key(dots[["swOUT_key"]])
      } else {
        do.call(swOUT_key, dots)
      },
      tmp
    )
  )
}


#' @rdname sw_upgrade
#' @export
setMethod(
  "sw_upgrade",
  signature = "swOUT",
  definition = function(object, verbose = FALSE) {
    #--- Compare available and expected number of outkeys
    n_exp <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
    n_has <- nrow(object@timeSteps)


    #--- Identify upgrade(s)
    # Maintenance:
    #   update `do_upgrade` when `n_exp` changes or new upgrades required!
    do_upgrade <- c(
      from_v230 = n_has == 30L && n_exp %in% 31L:32L,
      from_v310 = n_has == 31L && n_exp == 32L
    )

    do_upgrade <- do_upgrade[do_upgrade]

    if (any(do_upgrade)) {
      target <- swOUT()
      stopifnot(nrow(target) == n_exp)


      #--- Loop over upgrades sequentially
      for (k in seq_along(do_upgrade)) {

        if (verbose) {
          message(
            "Upgrading object of class `swOUT`: ",
            shQuote(names(do_upgrade)[k])
          )
        }

        # Maintenance: update `switch` when `n_exp` changes!
        id_new <- switch(
          EXPR = names(do_upgrade)[k],
          from_v230 = n_exp,
          from_v310 = 28L,
          stop(
            "Upgrade ", shQuote(names(do_upgrade)[k]),
            " is not implemented for class `swOUT`."
          )
        )


        #--- Upgrade `timeSteps`
        tmp <- object@timeSteps

        # Grab available values or default
        has_missing <- apply(
          tmp,
          MARGIN = 1L,
          function(object) {
            any(object == rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]])
          }
        )
        id <- which(!has_missing)
        tmp_new <- if (length(id) > 0) {
          tmp[id[[1L]], , drop = FALSE]
        } else {
          target@timeSteps[id_new, , drop = FALSE]
        }

        object@timeSteps <- rbind(
          if (id_new > 1L) {
            tmp[1L:(id_new - 1L), , drop = FALSE]
          },
          tmp_new,
          if (id_new <= n_has) {
            tmp[id_new:n_has, , drop = FALSE]
          }
        )

        #--- Upgrade `swOUT_key`s
        object@mykey <- target@mykey

        list_keys <- c(
          "myobj", "sumtype", "use", "first_orig", "last_orig", "outfile"
        )

        for (sn in list_keys) {
          tmp <- slot(object, sn)
          slot(object, sn) <- c(
            if (id_new > 1L) {
              tmp[1L:(id_new - 1L)]
            },
            slot(target, sn)[id_new],
            if (id_new <= n_has) {
              tmp[id_new:n_has]
            }
          )
        }

      }


      #--- Check validity and return
      validObject(object)
    }

    object
  }
)


#' @rdname swOUT-class
#' @export
setMethod("get_swOUT", "swOUT", function(object) object)

#' @rdname swOUT-class
#' @export
setMethod("swOUT_TimeStep", "swOUT", function(object) object@timeSteps)

#' @rdname swOUT-class
#' @export
setMethod(
  "swOUT_OutputSeparator",
  "swOUT",
  function(object) object@outputSeparator
)

#' @rdname swOUT-class
#' @export
setReplaceMethod(
  "set_swOUT",
  signature = "swOUT",
  function(object, value) {
    object <- value
    validObject(object)
    object
  }
)


#' @rdname swOUT-class
#' @examples
#' x <- swOUT()
#' activate_swOUT_OutKey(x) <- c("VWCMATRIC", "HYDRED")
#'
#' @export
setReplaceMethod(
  "activate_swOUT_OutKey",
  signature = "swOUT",
  function(object, value) {
    ids <- which(rSW2_glovars[["kSOILWAT2"]][["OutKeys"]] %in% value)

    if (length(ids) < length(value)) {
      tmp <- !(value %in% c(rSW2_glovars[["kSOILWAT2"]][["OutKeys"]], "LOG"))
      if (any(tmp)) {
        warning(
          "Outkeys ",
          paste0(shQuote(value[tmp]), collapse = "/"),
          " are not available."
        )
      }
    }

    if (length(ids) > 0) {
      eSW_NoTime <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]]
      tmp_use <- slot(object, "use")

      # Activate OutKeys by specifying output time periods (if not already set)
      ids_ts <- which(apply(
        slot(object, "timeSteps")[ids, , drop = FALSE],
        MARGIN = 1,
        function(x) all(x == eSW_NoTime)
      ))

      if (length(ids_ts) > 0) {
        # Guess output time periods from already activated OutKeys
        tmp <- unique(as.vector(slot(object, "timeSteps")))
        used_ts <- tmp[!(tmp %in% eSW_NoTime)]
        ts_toset <- matrix(
          eSW_NoTime,
          nrow = length(ids_ts),
          ncol = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
        )
        ts_toset[, seq_along(used_ts)] <- rep(used_ts, each = length(ids_ts))
        slot(object, "timeSteps")[ids[ids_ts], ] <- ts_toset
      }

      # Activate OutKeys by setting slot `use`
      slot(object, "use")[ids] <- TRUE
    }

    validObject(object)
    object
  }
)


#' @rdname swOUT-class
#' @examples
#' x <- swOUT()
#' deactivate_swOUT_OutKey(x) <- c("VWCMATRIC", "HYDRED")
#'
#' @export
setReplaceMethod(
  "deactivate_swOUT_OutKey",
  signature = "swOUT",
  function(object, value) {
    ids <- which(rSW2_glovars[["kSOILWAT2"]][["OutKeys"]] %in% value)

    if (length(ids) < length(value)) {
      tmp <- !(value %in% c(rSW2_glovars[["kSOILWAT2"]][["OutKeys"]], "LOG"))
      if (any(tmp)) {
        warning(
          "Outkeys ",
          paste0(shQuote(value[tmp]), collapse = "/"),
          " are not available."
        )
      }
    }

    if (length(ids) > 0) {
      # Deactivate OutKeys by setting output time periods to `eSW_NoTime`
      slot(object, "timeSteps")[ids, ] <-
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]]

      # Deactivate OutKeys by setting slot `use`
      slot(object, "use")[ids] <- FALSE
    }

    validObject(object)
    object
  }
)



#' @rdname swOUT-class
#' @export
setReplaceMethod(
  "swOUT_TimeStep",
  signature = "swOUT",
  function(object, value) {
    object@timeSteps <- value
    validObject(object)
    object
  }
)

#' Set time steps to the same set of values for each output key.
#' @examples
#' x <- swOUT()
#' swOUT_TimeStepsForEveryKey(x) <- c(2, 3)
#' identical(
#'   unique(sort(as.vector(swOUT_TimeStep(x)))),
#'   as.integer(c(2, 3, 999)) # 999 represents 'eSW_NoTime'
#' )
#'
#' @rdname swOUT-class
#' @export
setReplaceMethod(
  "swOUT_TimeStepsForEveryKey",
  signature = "swOUT",
  function(object, value) {
    stopifnot(
      length(value) <= rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
    )

    # Create empty matrix
    temp <- matrix(
      data = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]],
      nrow = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]],
      ncol = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
    )

    # Fill matrix with requested values
    temp[, seq_along(value)] <- rep(
      as.integer(value),
      each = rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNKEYS"]]
    )

    # Set unused output keys to no-time
    temp[!slot(object, "use"), ] <-
      rSW2_glovars[["kSOILWAT2"]][["kINT"]][["eSW_NoTime"]]

    object@timeSteps <- temp
    validObject(object)

    object
  }
)

#' @rdname swOUT-class
#' @export
setReplaceMethod(
  "swOUT_OutputSeparator",
  signature = "swOUT",
  function(object, value) {
    object@outputSeparator <- as.character(value)
    validObject(object)
    object
  }
)


# used by swReadLines
KEY <- c("WTHR", "TEMP", "PRECIP", "SOILINFILT", "RUNOFF", "ALLH2O", "VWCBULK",
  "VWCMATRIC", "SWCBULK", "SWA", "SWABULK", "SWAMATRIC", "SWPMATRIC",
  "SURFACEWATER", "TRANSP", "EVAPSOIL", "EVAPSURFACE", "INTERCEPTION",
  "LYRDRAIN", "HYDRED", "ET", "AET", "PET", "WETDAY", "SNOWPACK", "DEEPSWC",
  "SOILTEMP", "FROZEN", "ALLVEG", "ESTABL")
OutSum <- c("off", "sum", "avg", "fnl") # only used for 'swReadLines'
#Remember this models the C code so index starts at 0 not 1
timePeriods <- c("dy", "wk", "mo", "yr")


#' @rdname swOUT-class
#' @export
# nolint start
setMethod(
  "swReadLines",
  signature = c(object="swOUT",file="character"),
  function(object,file) {
    print("TODO: method 'swReadLines' for class 'swOUT' is not up-to-date; hard-coded indices are incorrect")

    infiletext <- readLines(con = file)
    if(temp<-strsplit(infiletext[41],split=" ")[[1]][2] == "t") {
      object@outputSeparator="\t"
    } else if(temp == "s") {
      object@outputSeparator=" "
    } else if(temp == "c"){
      object@outputSeparator=","
    } else {
      object@outputSeparator="\t"
    }

    if (infiletext[42] == "") {
      useTimeStep = FALSE
    } else {
      useTimeStep = TRUE
      temp<-strsplit(x=infiletext[42], split=" ")[[1]][-1]
      object@timeSteps = as.integer(
        sapply(
          1:length(temp),
          FUN=function(i) which(temp[i] == timePeriods)
        )-1
      )
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
  }
)
# nolint end
