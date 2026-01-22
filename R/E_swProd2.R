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


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018);
#   Zach Kramer (2017)
###############################################################################

# veg2_names must be identical to key2veg[]
# and to rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]
veg2_names <- c("treeNL", "treeBL", "shrub", "forbs", "grassC3", "grassC4")
lc2_names <- c(veg2_names, "Bare Ground")
nvegs2 <- length(veg2_names)


#' Class \code{"swProd2"}
#'
#' Class \code{"swProd2"} is superseding class \code{"swProd"} starting with
#' \pkg{rSOILWAT2} v6.5.0 and \var{SOILWAT2} v8.3.0.
#'
#' The matrix slots of class \code{"swProd2"} have now consistently
#' plant functional types as rows and variables as columns
#' (instead of a mix as with class \code{"swProd"}).
#'
#' The new plant functional types have different names and are now sorted
#' consistently, see \code{\link{mapVegTypes}}.
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swProd2}}.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Arguments to the helper constructor function.
#'  Dots can either contain objects to copy into slots of that class
#'  (must be named identical to the corresponding slot) or
#'  be one object of that class (in which case it will be copied and
#'  any missing slots will take their default values).
#'  If dots are missing, then corresponding values of
#'  \code{rSOILWAT2::sw_exampleData}
#'  (i.e., the \pkg{SOILWAT2} "testing" defaults) are copied.
#' @param vegtype The name or index of the vegetation type.
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swProd2")
#' x <- new("swProd2")
#' x <- swProd2()
#'
#' @name swProd2-class
#' @export
setClass(
  "swProd2",
  slots = c(
    veg_method = "integer",
    nYearsDynamicShort = "integer",
    nYearsDynamicLong = "integer",
    Composition = "numeric",
    Albedo = "numeric",
    CanopyHeight = "matrix",
    VegetationInterceptionParameters = "matrix",
    LitterInterceptionParameters = "matrix",
    EsTpartitioning_param = "numeric",
    Es_param_limit = "numeric",
    Shade = "matrix",
    HydraulicRedistribution_use = "logical",
    HydraulicRedistribution = "matrix",
    CriticalSoilWaterPotential = "numeric",
    CO2Coefficients = "matrix",
    vegYear = "integer",
    isBiomAsIf100Cover = "logical",
    MonthlyVeg = "list"
  ),
  prototype = list(
    veg_method = NA_integer_,
    nYearsDynamicShort = NA_integer_,
    nYearsDynamicLong = NA_integer_,
    Composition = stats::setNames(rep(NA_real_, nvegs2 + 1L), lc2_names),
    Albedo = stats::setNames(rep(NA_real_, nvegs2 + 1L), lc2_names),
    CanopyHeight = array(
      NA_real_,
      dim = c(nvegs2, 5L),
      dimnames = list(
        veg2_names, c("xinflec", "yinflec", "range", "slope", "height_cm")
      )
    ),
    VegetationInterceptionParameters = array(
      NA_real_,
      dim = c(nvegs2, 2L),
      dimnames = list(veg2_names, c("kSmax", "kdead"))
    ),
    LitterInterceptionParameters = array(
      NA_real_,
      dim = c(nvegs2, 1L),
      dimnames = list(veg2_names, "kSmax")
    ),
    EsTpartitioning_param = stats::setNames(rep(NA_real_, nvegs2), veg2_names),
    Es_param_limit = stats::setNames(rep(NA_real_, nvegs2), veg2_names),
    Shade = array(
      NA_real_,
      dim = c(nvegs2, 6L),
      dimnames = list(
        veg2_names,
        c(
          "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec",
          "yinflec", "range", "slope"
        )
      )
    ),
    HydraulicRedistribution_use = stats::setNames(rep(NA, nvegs2), veg2_names),
    HydraulicRedistribution = array(
      NA_real_,
      dim = c(nvegs2, 3L),
      dimnames = list(
        veg2_names,
        c("MaxCondRoot", "SoilWaterPotential50", "ShapeCond")
      )
    ),
    CriticalSoilWaterPotential = stats::setNames(
      rep(NA_real_, nvegs2), veg2_names
    ),
    CO2Coefficients = array(
      NA_real_,
      dim = c(nvegs2, 4L),
      dimnames = list(
        veg2_names,
        c("Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2")
      )
    ),
    vegYear = NA_integer_,
    isBiomAsIf100Cover = NA,
    MonthlyVeg = stats::setNames(
      lapply(
        veg2_names,
        function(k) {
          array(
            NA_real_,
            dim = c(12L, 4L),
            dimnames = list(
              c(
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
              ),
              c("Litter", "Biomass", "Live_pct", "LAI_conv")
            )
          )
        }
      ),
      veg2_names
    )
  )
)



setValidity(
  "swProd2",
  function(object) {
    val <- TRUE
    nvegs2 <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]

    if (length(object@veg_method) != 1L) {
      msg <- "@veg_method must have 1 value."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@nYearsDynamicShort) != 1L &&
        (is.na(object@nYearsDynamicShort) || object@nYearsDynamicShort >= 0L)
    ) {
      msg <- "@nYearsDynamicShort must have 1 value that is NA, 0, or positive."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@nYearsDynamicLong) != 1L &&
        (is.na(object@nYearsDynamicLong) || object@nYearsDynamicLong >= 0L)
    ) {
      msg <- "@nYearsDynamicLong must have 1 value that is NA, 0, or positive."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Composition) != 1L + nvegs2 ||
        !all(is.na(object@Composition) | (object@Composition >= 0. &
            object@Composition <= 1.))
    ) {
      msg <- paste(
        "@Composition must have 1 + NVEGTYPES values",
        "between 0 and 1 or NA."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Albedo) != 1L + nvegs2 ||
        !all(is.na(object@Albedo) | (object@Albedo >= 0. & object@Albedo <= 1.))
    ) {
      msg <- "@Albedo must have 1 + NVEGTYPES values between 0 and 1 or NA."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CanopyHeight)
    if (!identical(temp, c(nvegs2, 5L))) {
      msg <- "@CanopyHeight must be a NVEGTYPES x 5 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@VegetationInterceptionParameters)
    if (!identical(temp, c(nvegs2, 2L))) {
      msg <- "@VegetationInterceptionParameters must be a NVEGTYPES x 4 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@LitterInterceptionParameters)
    if (!identical(temp, c(nvegs2, 1L))) {
      msg <- "@LitterInterceptionParameters must be a NVEGTYPES x 1 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@EsTpartitioning_param) != nvegs2) {
      msg <- "@EsTpartitioning_param must have NVEGTYPES values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Es_param_limit) != nvegs2 ||
        !all(is.na(object@Es_param_limit) | object@Es_param_limit >= 0.)
    ) {
      msg <- "@Es_param_limit must have NVEGTYPES non-negative values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@Shade)
    if (!identical(temp, c(nvegs2, 6L))) {
      msg <- "@Shade must be a NVEGTYPES x 6 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@HydraulicRedistribution_use) != nvegs2) {
      msg <- "@HydraulicRedistribution_use must have NVEGTYPES values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@HydraulicRedistribution)
    if (!identical(temp, c(nvegs2, 3L))) {
      msg <- "@HydraulicRedistribution must be a NVEGTYPES x 3 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@CriticalSoilWaterPotential) != nvegs2 ||
        !all(
          is.na(object@CriticalSoilWaterPotential) |
            object@Composition[seq_len(nvegs2)] < 1e-6 |
            object@CriticalSoilWaterPotential < 0.
        )
    ) {
      msg <- "@CriticalSoilWaterPotential must have NVEGTYPES negative values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CO2Coefficients)
    if (!identical(temp, c(nvegs2, 4L))) {
      msg <- "@CO2Coefficients must be a NVEGTYPES x 4 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@vegYear) != 1L) {
      msg <- "@vegYear length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@isBiomAsIf100Cover) != 1L) {
      msg <- "@isBiomAsIf100Cover length != 1."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@MonthlyVeg) != nvegs2 ||
      !all(
        vapply(
          object@MonthlyVeg,
          function(x) identical(dim(x), c(12L, 4L)) && all(x >= 0 | is.na(x)),
          FUN.VALUE = NA
        )
      )
    ) {
      msg <- paste(
        "@MonthlyVeg must be a list with NVEGTYPES elements of a",
        "12x4 matrix with values larger than 0 (or NA)."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      !all(
        vapply(
          object@MonthlyVeg,
          function(x) all(x[, 3L] <= 1 | is.na(x[, 3L])),
          FUN.VALUE = NA
        )
      )
    ) {
      msg <- "@MonthlyVeg[, 'Live_pct'] must be <= 1 or NA."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    val
  }
)

#' @rdname swProd2-class
#' @export
swProd2 <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "prod2")
  sns <- slotNames("swProd2")
  dots <- list(...)
  if (length(dots) == 1L && inherits(dots[[1]], "swProd2")) {
    # If dots are one object of this class, then convert to list of its slots
    dots <- attributes(unclass(dots[[1]]))
  }
  dns <- names(dots)

  # We don't set values for slot `Composition`; this is to prevent simulation
  # runs with accidentally incorrect values
  if (!("Composition" %in% dns)) {
    def@Composition[] <- NA_real_
  }

  # Guarantee names
  gdns <- c(
    "CanopyHeight", "VegetationInterceptionParameters",
    "LitterInterceptionParameters", "HydraulicRedistribution",
    "CO2Coefficients"
  )

  for (g in gdns) if (g %in% dns) {
    dimnames(dots[[g]]) <- dimnames(slot(def, g))
  }

  if ("MonthlyVeg" %in% dns) {
    for (kveg in veg2_names) {
      dimnames(dots[["MonthlyVeg"]][[kveg]]) <- dimnames(
        slot(def, "MonthlyVeg")[[kveg]]
      )
    }
  }

  # Copy from SOILWAT2 "testing" (defaults), but dot arguments take precedence
  tmp <- lapply(
    sns,
    function(sn) if (sn %in% dns) dots[[sn]] else slot(def, sn)
  )
  names(tmp) <- sns

  do.call("new", args = c("swProd2", tmp))
}


#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "swProd2",
  definition = function(object, verbose = FALSE) {
    tmp <- try(validObject(object), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swProd2`.")
      }
      object <- suppressWarnings(swProd2(object))
    }

    object
  }
)

#' Create a swProd2 object using values from outdated swProd object
#'
#' @examples
#' x <- swProd2FromProd1(swProd())
#' validObject(x)
#'
#' @noRd
swProd2FromProd1 <- function(prodv1) {
  if (!inherits(prodv1, "swProd")) {
    stop(
      "Expected argument of class 'swProd' but found class '",
      class(prodv1)[[1L]],
      "'.",
      call. = FALSE
    )
  }

  res <- new("swProd2")

  #--- Initialize to 0 (used by the new types)
  for (sn in slotNames(res)) {
    st <- typeof(slot(res, sn))
    if (identical(st, "list")) {
      for (k in seq_along(slot(res, sn))) {
        slot(res, sn)[[k]][] <- as(0, typeof(slot(res, sn)[[k]]))
      }
    } else {
      slot(res, sn)[] <- as(0, st)
    }
  }

  #--- Assign values from old to new veg types
  ids <- mapVegTypes("2from1", order = "rSOILWAT2")
  idsnz <- which(ids > 0L)

  res@veg_method <- prodv1@veg_method

  if ("nYearsDynamicShort" %in% slotNames(prodv1)) {
    res@nYearsDynamicShort <- prodv1@nYearsDynamicShort
  }

  if ("nYearsDynamicLong" %in% slotNames(prodv1)) {
    res@nYearsDynamicLong <- prodv1@nYearsDynamicLong
  }

  res@Composition[c(idsnz, 7L)] <- prodv1@Composition[c(ids, 5L)]

  res@Albedo[c(idsnz, 7L)] <- prodv1@Albedo[c(ids, 5L)]

  res@CanopyHeight[idsnz, ] <- t(prodv1@CanopyHeight[, ids, drop = FALSE])

  res@VegetationInterceptionParameters[idsnz, ] <- t(
    prodv1@VegetationInterceptionParameters[, ids, drop = FALSE]
  )

  res@LitterInterceptionParameters[idsnz, ] <-
    prodv1@LitterInterceptionParameters[, ids]

  res@EsTpartitioning_param[idsnz] <- prodv1@EsTpartitioning_param[ids]

  res@Es_param_limit[idsnz] <- prodv1@Es_param_limit[ids]

  res@Shade[idsnz, ] <- t(prodv1@Shade[, ids, drop = FALSE])

  res@HydraulicRedistribution_use[idsnz] <-
    prodv1@HydraulicRedistribution_use[ids]

  res@HydraulicRedistribution[idsnz, ] <- t(
    prodv1@HydraulicRedistribution[, ids, drop = FALSE]
  )

  res@CriticalSoilWaterPotential[idsnz] <-
    prodv1@CriticalSoilWaterPotential[ids]

  res@CO2Coefficients[idsnz, ] <- prodv1@CO2Coefficients[ids, , drop = FALSE]

  res@vegYear <- prodv1@vegYear

  res@isBiomAsIf100Cover <- prodv1@isBiomAsIf100Cover

  # Different sequence of types in prodv1@MonthlyVeg
  ids2 <- mapVegTypes("2from1", order = "SOILWAT2")
  ids2nz <- which(ids2 > 0L)

  for (k in ids2nz) {
    res@MonthlyVeg[[k]] <- prodv1@MonthlyVeg[[ids2[[k]]]]
  }

  res
}


#--- swProd2 methods ------
#' @rdname swProd2-class
#' @seealso [`get_swProd`]
#' @md
#' @export
setMethod("get_swProd", "swProd2", function(object) object)

#' @rdname swProd2-class
#' @export
get_swProd2 <- function(object) get_swProd(object)

#' @rdname swProd2-class
#' @export
setMethod("swProd_Composition", "swProd2", function(object) object@Composition)

#' @rdname swProd2-class
#' @export
setMethod("swProd_Albedo", "swProd2", function(object) object@Albedo)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_CanopyHeight",
  "swProd2",
  function(object) object@CanopyHeight
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_VegInterParam",
  "swProd2",
  function(object) object@VegetationInterceptionParameters
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_LitterInterParam",
  "swProd2",
  function(object) object@LitterInterceptionParameters
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_EsTpartitioning_param",
  "swProd2",
  function(object) object@EsTpartitioning_param
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_Es_param_limit",
  "swProd2",
  function(object) object@Es_param_limit
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_Shade",
  "swProd2",
  function(object) object@Shade
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_HydrRedstro_use",
  "swProd2",
  function(object) object@HydraulicRedistribution_use
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_HydrRedstro",
  "swProd2",
  function(object) object@HydraulicRedistribution
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_CritSoilWaterPotential",
  "swProd2",
  function(object) object@CriticalSoilWaterPotential
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_CO2Coefficients",
  "swProd2",
  function(object) object@CO2Coefficients
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd2", vegtype = "numeric"),
  function(object, vegtype) {
    vegtype <- as.integer(vegtype)
    stopifnot(
      isTRUE(vegtype > 0L),
      vegtype <= rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]
    )
    object@MonthlyVeg[[vegtype]]
  }
)

#' @rdname swProd2-class
#' @export
setMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd2", vegtype = "character"),
  function(object, vegtype) {
    id <- grep(
      vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes2"]]),
      ignore.case = TRUE
    )
    if (length(id) == 0L) {
      id <- grep(
        vegtype,
        rSW2_glovars[["kSOILWAT2"]][["VegTypeNames2"]],
        ignore.case = TRUE
      )
    }
    if (length(id) != 1L) {
      stop(shQuote(vegtype), " is not a known vegetation type.", call. = FALSE)
    }
    object@MonthlyVeg[[id]]
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "set_swProd",
  signature = "swProd2",
  function(object, value) {
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_Composition",
  signature = "swProd2",
  function(object, value) {
    # Check that x in swProd_Composition() <- x has correct length
    # Note: check does not work with subset, e.g., swProd_Composition()[1] <- x
    stopifnot(length(value) %in% c(1L, length(object@Composition)))
    object@Composition[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_Albedo",
  signature = "swProd2",
  function(object, value) {
    object@Albedo[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_CanopyHeight",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@CanopyHeight)
    object@CanopyHeight <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_VegInterParam",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@VegetationInterceptionParameters)
    object@VegetationInterceptionParameters <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_LitterInterParam",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@LitterInterceptionParameters)
    object@LitterInterceptionParameters <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_EsTpartitioning_param",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@EsTpartitioning_param)
    object@EsTpartitioning_param <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_Es_param_limit",
  signature = "swProd2",
  function(object, value) {
    object@Es_param_limit[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_Shade",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@Shade)
    object@Shade <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro_use",
  signature = "swProd2",
  function(object, value) {
    object@HydraulicRedistribution_use[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@HydraulicRedistribution)
    object@HydraulicRedistribution <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_CritSoilWaterPotential",
  signature = "swProd2",
  function(object, value) {
    object@CriticalSoilWaterPotential[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_CO2Coefficients",
  signature = "swProd2",
  function(object, value) {
    dimnames(value) <- dimnames(object@CO2Coefficients)
    object@CO2Coefficients <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd2", vegtype = "numeric", value = "matrix"),
  function(object, vegtype, value) {
    vegtype <- as.integer(vegtype)
    stopifnot(
      isTRUE(vegtype > 0L),
      vegtype <= rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]
    )
    dimnames(value) <- dimnames(object@MonthlyVeg[[vegtype]])
    object@MonthlyVeg[[vegtype]] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd2-class
#' @export
setReplaceMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd2", vegtype = "character", value = "matrix"),
  function(object, vegtype, value) {
    id <- grep(
      vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes2"]]),
      ignore.case = TRUE
    )
    if (length(id) == 0L) {
      id <- grep(
        vegtype,
        rSW2_glovars[["kSOILWAT2"]][["VegTypeNames2"]],
        ignore.case = TRUE
      )
    }
    if (length(id) != 1L) {
      stop(shQuote(vegtype), " is not a known vegetation type.", call. = FALSE)
    }
    swProd_MonProd_veg(object, id) <- value
    object
  }
)
