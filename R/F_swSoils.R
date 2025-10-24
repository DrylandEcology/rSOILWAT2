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

###############################################################SOILS###########

#' List names of currently implemented soil properties
#' @return A vector of names of soil properties.
#' @export
soilLayer_dataColumns <- function() {
  c(
    "depth_cm",
    "bulkDensity_g/cm^3",
    "gravel_content",
    "sand_frac",
    "clay_frac",
    "som_frac",
    "impermeability_frac",
    "soilTemp_c",
    "EvapBareSoil_frac",
    # TrCo_<veg>: see key2veg and rSW2_glovars[["kSOILWAT2"]][["VegTypeNames2"]]
    "TrCo_treeNL",
    "TrCo_treeBL",
    "TrCo_shrub",
    "TrCo_forbs",
    "TrCo_grassC3",
    "TrCo_grassC4"
  )
}

nSoilVars <- length(soilLayer_dataColumns())


#' Class \code{"swSoils"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swSoils}}.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swSoils")
#' x <- new("swSoils")
#' x <- swSoils()
#'
#' @name swSoils-class
#' @export
setClass(
  "swSoils",
  slots = c(
    Layers = "matrix",
    SWRCp = "matrix",
    omSWRCp = "matrix"
  ),
  prototype = list(
    Layers = array(
      NA_real_,
      dim = c(0L, nSoilVars),
      dimnames = list(NULL, soilLayer_dataColumns())
    ),
    SWRCp = array(
      NA_real_,
      dim = c(0L, 6L),
      dimnames = list(
        NULL,
        paste0("Param", seq_len(6L))
      )
    ),
    omSWRCp = array(
      NA_real_,
      dim = c(2L, 6L),
      dimnames = list(
        NULL,
        paste0("Param", seq_len(6L))
      )
    )
  )
)


setValidity(
  "swSoils",
  function(object) {
    val <- TRUE
    tmpL <- dim(object@Layers)
    tmpp <- dim(object@SWRCp)
    tmpom <- dim(object@omSWRCp)
    dtol1 <- 1. + tmpL[[1L]] * rSW2_glovars[["tol"]]

    varsExpected <- soilLayer_dataColumns()

    #--- Check "Layers"
    if (tmpL[[2L]] != length(varsExpected)) {
      msg <- paste(
        "@Layers must have exactly",
        length(varsExpected),
        "columns corresponding to",
        toString(varsExpected)
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
      return(val) # return early to avoid subscriptOutOfBoundsError
    }

    depths <- object@Layers[, 1L]

    if (
      !(
        all(is.na(depths)) ||
          all(depths > 0.) &&
          !anyNA(
            rSW2utils::check_monotonic_increase(
              depths,
              MARGIN = 2L,
              strictly = TRUE
            )
          )
      )
    ) {
      msg <- paste(
        "@Layers[, 'depth_cm'] must be positive, strictly increasing depths",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }


    tmp <- object@Layers[, c(3L:7L, 9L:nSoilVars), drop = FALSE]
    res <- apply(tmp, 2L, function(x) all(is.na(x)) || all(x >= 0., x <= dtol1))
    if (!all(res)) {
      msg <- paste(
        "@Layers values of gravel, sand, clay, som, impermeability,",
        "and evco and all trcos must be between 0 and 1",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    tmp <- colSums(object@Layers[, 9L:nSoilVars, drop = FALSE])
    if (!all(is.na(tmp) | tmp <= dtol1)) {
      msg <- paste(
        "@Layers values of profile sums of evco and each trcos must be",
        "between 0 and 1",
        "(or all NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    #--- Check "SWRCp"
    # `SW_SIT_init_run()` will call function to check validity of SWRCp values
    if (tmpp[1L] != tmpL[1L]) {
      msg <- paste(
        "@SWRCp must have exactly the same number of soil layers (rows)",
        "as @Layers."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (
      tmpp[2L] != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]]
    ) {
      msg <- paste(
        "@SWRCp must have exactly",
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]],
        "columns."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (tmpom[1L] != 2L) {
      msg <- paste(
        "@omSWRCp must have exactly two layers."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
    if (
      tmpom[2L] != rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]]
    ) {
      msg <- paste(
        "@omSWRCp must have exactly",
        rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SWRC_PARAM_NMAX"]],
        "columns."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swSoils-class
#' @export
swSoils <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "soils")
  sns <- slotNames("swSoils")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swSoils")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slot `Layers` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if ("Layers" %in% dns) {
    # Guarantee names
    dimnames(dots[["Layers"]]) <- list(NULL, colnames(def@Layers))
    ntmp <- nrow(dots[["Layers"]])
  } else {
    def@Layers <- def@Layers[1, , drop = FALSE]
    def@Layers[] <- NA_real_
    ntmp <- 1
  }

  # We don't set values for slot `SWRCp` if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if ("SWRCp" %in% dns) {
    # Guarantee names
    dimnames(dots[["SWRCp"]]) <- list(NULL, colnames(def@SWRCp))
  } else {
    def@SWRCp <- def@SWRCp[rep.int(1, ntmp), , drop = FALSE]
    def@SWRCp[] <- NA_real_
  }

  if ("omSWRCp" %in% dns) {
    # Guarantee names
    dimnames(dots[["omSWRCp"]]) <- dimnames(def@omSWRCp)
  } else {
    def@omSWRCp <- def@omSWRCp[1:2, , drop = FALSE]
    def@omSWRCp[] <- NA_real_
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swSoils", tmp))
}


#' @rdname sw_upgrade
#'
#' @param soilLayers A two-dimensional object representing soil layers in
#' rows and soil properties in columns,
#' see slot `"Layers"` of `["swSoils-class"]`.
#' @param template_soilLayerProperties A vector of standard names of
#' soil properties, see [soilLayer_dataColumns()].
#'
#' @return For [upgrade_soilLayers()]:
#' an updated `soilLayers` matrix with requested columns
#' (a new `"som_frac"` is initialized to the default value of 0).
#'
#' @examples
#' upgrade_soilLayers(
#'   data.frame(
#'     sand_frac = runif(2),
#'     clay_frac = runif(2),
#'     dummy = runif(2),
#'     transpGrass_frac = runif(2)
#'   )
#' )
#' soils <- slot(rSOILWAT2::sw_exampleData, "soils")
#' upgrade_soilLayers(slot(soils, "Layers"))
#' upgrade_soilLayers(slot(soils, "Layers")[, 1:8L, drop = FALSE])
#'
#' @md
#' @export
upgrade_soilLayers <- function(
  soilLayers,
  template_soilLayerProperties = soilLayer_dataColumns()
) {
  soilLayers <- data.matrix(soilLayers)

  template_data <- array(
    dim = c(nrow(soilLayers), length(template_soilLayerProperties)),
    dimnames = list(NULL, template_soilLayerProperties)
  )

  #--- Copy values of shared variables
  cns <- intersect(template_soilLayerProperties, colnames(soilLayers))
  if (length(cns) < 1L) stop("Required variables not found.", call. = FALSE)
  template_data[, cns] <- soilLayers[, cns, drop = FALSE]

  #--- Set some variables to zero
  idsMapVegTypes1to2 <- mapVegTypes("2from1")
  varsTrCo <- paste0("TrCo_", names(idsMapVegTypes1to2))

  varsToZero <- intersect(c("som_frac", varsTrCo), template_soilLayerProperties)
  cns <- setdiff(varsToZero, colnames(soilLayers))
  if (length(cns) > 0L) {
    template_data[, cns] <- 0
  }

  #--- Copy trco values from old to new veg types
  varsTrCoOld <- paste0("transp", c("Grass", "Shrub", "Tree", "Forb"), "_frac")
  idsTrCoUsed <- intersect(
    which(
      varsTrCo[idsMapVegTypes1to2 > 0L] %in% template_soilLayerProperties
    ),
    which(
      varsTrCoOld[idsMapVegTypes1to2] %in% colnames(soilLayers)
    )
  )
  if (length(idsTrCoUsed) > 0L) {
    template_data[, varsTrCo[idsMapVegTypes1to2 > 0L][idsTrCoUsed]] <-
      soilLayers[, varsTrCoOld[idsMapVegTypes1to2][idsTrCoUsed], drop = FALSE]
  }

  template_data
}

#' @rdname sw_upgrade
#' @export
setMethod(
  "sw_upgrade",
  signature = "swSoils",
  definition = function(object, verbose = FALSE) {
    needsUpgrade <-
      !all(soilLayer_dataColumns() %in% colnames(object@Layers)) ||
      inherits(try(object@SWRCp, silent = TRUE), "try-error") ||
      inherits(try(object@omSWRCp, silent = TRUE), "try-error")

    if (needsUpgrade) {
      if (verbose) {
        message("Upgrading object of class `swSoils`.")
      }
      object@Layers <- upgrade_soilLayers(object@Layers)
      object <- suppressWarnings(swSoils(object))
    }

    object
  }
)


#' @rdname swSoils-class
#' @export
setMethod("get_swSoils", "swSoils", function(object) object)

#' @rdname swSoils_Layers
setMethod("swSoils_Layers", "swSoils", function(object) object@Layers)

#' @rdname swSoils_SWRCp
setMethod("swSoils_SWRCp", "swSoils", function(object) object@SWRCp)

#' @rdname swSoils_omSWRCp
setMethod("swSoils_omSWRCp", "swSoils", function(object) object@omSWRCp)

#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swSoils", value = "swSoils"),
  function(object, value) {
    colnames(value@Layers) <- colnames(object@Layers)
    colnames(value@SWRCp) <- colnames(object@SWRCp)
    dimnames(value@omSWRCp) <- dimnames(object@omSWRCp)
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swSoils-class
#' @export
setReplaceMethod(
  "set_swSoils",
  signature = c(object = "swSoils", value = "list"),
  function(object, value) {
    idl <- if (utils::hasName(value, "Layers")) "Layers" else 1
    idp <- if (utils::hasName(value, "SWRCp")) "SWRCp" else 2
    ido <- if (utils::hasName(value, "omSWRCp")) "omSWRCp" else 3
    colnames(value[[idl]]) <- colnames(object@Layers)
    colnames(value[[idp]]) <- colnames(object@SWRCp)
    dimnames(value[[ido]]) <- dimnames(object@omSWRCp)
    object@Layers <- data.matrix(value[[idl]])
    object@SWRCp <- data.matrix(value[[idp]])
    object@omSWRCp <- data.matrix(value[[ido]])
    validObject(object)
    object
  }
)

#' @rdname swSoils_Layers
setReplaceMethod(
  "swSoils_Layers",
  signature = "swSoils",
  function(object, value) {
    colnames(value) <- colnames(object@Layers)
    object@Layers <- data.matrix(value)
    # Note: validity check fails if number of soil layers disagrees with
    # number of of soil layers of SWRC parameters
    # --> see method for "swInputData" that can automatically resizes SWRCp
    validObject(object)
    object
  }
)


#' @rdname swSoils_SWRCp
setReplaceMethod(
  "swSoils_SWRCp",
  signature = "swSoils",
  function(object, value) {
    colnames(value) <- colnames(object@SWRCp)
    object@SWRCp <- data.matrix(value)
    validObject(object)
    object
  }
)

#' @rdname swSoils_omSWRCp
setReplaceMethod(
  "swSoils_omSWRCp",
  signature = "swSoils",
  function(object, value) {
    dimnames(value) <- dimnames(object@omSWRCp)
    object@omSWRCp <- data.matrix(value)
    validObject(object)
    object
  }
)


reset_SWRCp <- function(SWRCp, new_nrow = 1L) {
  array(
    data = NA_real_,
    dim = c(new_nrow, ncol(SWRCp)),
    dimnames = list(NULL, colnames(SWRCp))
  )
}

reset_omSWRCp <- function(omSWRCp) {
  array(
    data = NA_real_,
    dim = c(2L, ncol(omSWRCp)),
    dimnames = dimnames(omSWRCp)
  )
}
