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

veg_names <- c("Grasses", "Shrubs", "Trees", "Forbs")
lc_names <- c(veg_names, "Bare Ground")

#' Class \code{"swProd"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swProd}}.
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
#' showClass("swProd")
#' x <- new("swProd")
#' x <- swProd()
#'
#' @name swProd-class
#' @export
setClass(
  "swProd",
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
    # 5 should be 1 + rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]
    Composition = stats::setNames(rep(NA_real_, 5), lc_names),
    Albedo = stats::setNames(rep(NA_real_, 5), lc_names),
    CanopyHeight = array(
      NA_real_,
      dim = c(5L, 4L),
      dimnames = list(
        c("xinflec", "yinflec", "range", "slope", "height_cm"),
        veg_names
      )
    ),
    VegetationInterceptionParameters = array(
      NA_real_,
      dim = c(2L, 4L),
      dimnames = list(
        c("kSmax", "kdead"),
        veg_names
      )
    ),
    LitterInterceptionParameters = array(
      NA_real_,
      dim = c(1L, 4L),
      dimnames = list(
        "kSmax",
        veg_names
      )
    ),
    EsTpartitioning_param = stats::setNames(rep(NA_real_, 4L), veg_names),
    Es_param_limit = stats::setNames(rep(NA_real_, 4L), veg_names),
    Shade = array(
      NA_real_,
      dim = c(6L, 4L),
      dimnames = list(
        c(
          "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec",
          "yinflec", "range", "slope"
        ),
        veg_names
      )
    ),
    HydraulicRedistribution_use = stats::setNames(rep(NA, 4L), veg_names),
    HydraulicRedistribution = array(
      NA_real_,
      dim = c(3L, 4L),
      dimnames = list(
        c("MaxCondRoot", "SoilWaterPotential50", "ShapeCond"),
        veg_names
      )
    ),
    CriticalSoilWaterPotential = stats::setNames(rep(NA_real_, 4L), veg_names),
    CO2Coefficients = array(
      NA_real_,
      dim = c(4L, 4L),
      dimnames = list(
        veg_names,
        c("Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2")
      )
    ),
    vegYear = NA_integer_,
    isBiomAsIf100Cover = NA,
    MonthlyVeg = stats::setNames(
      lapply(
        veg_names,
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
      veg_names
    )
  )
)



setValidity(
  "swProd",
  function(object) {
    val <- TRUE
    nvegs <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]

  if (length(object@veg_method) != 1L) {
    msg <- "@veg_method must have 1 value."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

    if (
      length(object@Composition) != 1L + nvegs ||
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
      length(object@Albedo) != 1L + nvegs ||
        !all(is.na(object@Albedo) | (object@Albedo >= 0. & object@Albedo <= 1.))
    ) {
      msg <- "@Albedo must have 1 + NVEGTYPES values between 0 and 1 or NA."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CanopyHeight)
    if (!identical(temp, c(5L, nvegs))) {
      msg <- "@CanopyHeight must be a 5xNVEGTYPES matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@VegetationInterceptionParameters)
    if (!identical(temp, c(2L, nvegs))) {
      msg <- "@VegetationInterceptionParameters must be a 4xNVEGTYPES matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@LitterInterceptionParameters)
    if (!identical(temp, c(1L, nvegs))) {
      msg <- "@LitterInterceptionParameters must be a 1xNVEGTYPES matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@EsTpartitioning_param) != nvegs) {
      msg <- "@EsTpartitioning_param must have NVEGTYPES values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Es_param_limit) != nvegs ||
        !all(is.na(object@Es_param_limit) | object@Es_param_limit >= 0.)
    ) {
      msg <- "@Es_param_limit must have NVEGTYPES non-negative values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@Shade)
    if (!identical(temp, c(6L, nvegs))) {
      msg <- "@Shade must be a 6xNVEGTYPES matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@HydraulicRedistribution_use) != nvegs) {
      msg <- "@HydraulicRedistribution_use must have NVEGTYPES values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@HydraulicRedistribution)
    if (!identical(temp, c(3L, nvegs))) {
      msg <- "@HydraulicRedistribution must be a 3xNVEGTYPES matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@CriticalSoilWaterPotential) != nvegs ||
        !all(
          is.na(object@CriticalSoilWaterPotential) |
            object@CriticalSoilWaterPotential < 0.
        )
    ) {
      msg <- "@CriticalSoilWaterPotential must have NVEGTYPES negative values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CO2Coefficients)
    if (!identical(temp, c(4L, nvegs))) {
      msg <- "@CO2Coefficients must be a 4xNVEGTYPES matrix."
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
      length(object@MonthlyVeg) != nvegs ||
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

#' @rdname swProd-class
#' @export
swProd <- function(...) {
  def <- slot(rSOILWAT2::sw_exampleData, "prod")
  sns <- slotNames("swProd")
  dots <- list(...)
  if (length(dots) == 1 && inherits(dots[[1]], "swProd")) {
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
    for (kveg in veg_names) {
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

  do.call("new", args = c("swProd", tmp))
}


#' @rdname sw_upgrade
setMethod(
  "sw_upgrade",
  signature = "swProd",
  definition = function(object, verbose = FALSE) {
    tmp <- try(validObject(object), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (verbose) {
        message("Upgrading object of class `swProd`.")
      }
      object <- suppressWarnings(swProd(object))
    }

    object
  }
)


#' @rdname swProd-class
#' @export
setMethod("get_swProd", "swProd", function(object) object)

#' @rdname swProd-class
#' @export
setMethod("swProd_Composition", "swProd", function(object) object@Composition)

#' @rdname swProd-class
#' @export
setMethod("swProd_Albedo", "swProd", function(object) object@Albedo)

#' @rdname swProd-class
#' @export
setMethod("swProd_CanopyHeight", "swProd", function(object) object@CanopyHeight)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_VegInterParam",
  "swProd",
  function(object) object@VegetationInterceptionParameters
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_LitterInterParam",
  "swProd",
  function(object) object@LitterInterceptionParameters
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_EsTpartitioning_param",
  "swProd",
  function(object) object@EsTpartitioning_param
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_Es_param_limit",
  "swProd",
  function(object) object@Es_param_limit
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_Shade",
  "swProd",
  function(object) object@Shade
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_HydrRedstro_use",
  "swProd",
  function(object) object@HydraulicRedistribution_use
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_HydrRedstro",
  "swProd",
  function(object) object@HydraulicRedistribution
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_CritSoilWaterPotential",
  "swProd",
  function(object) object@CriticalSoilWaterPotential
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_CO2Coefficients",
  "swProd",
  function(object) object@CO2Coefficients
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "numeric"),
  function(object, vegtype) object@MonthlyVeg[[as.integer(vegtype)]]
)

#' @rdname swProd-class
#' @export
setMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "character"),
  function(object, vegtype) {
    id_vegtype <- grep(
      vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]),
      ignore.case = TRUE
    )
    object@MonthlyVeg[[id_vegtype]]
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "set_swProd",
  signature = "swProd",
  function(object, value) {
    object <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_Composition",
  signature = "swProd",
  function(object, value) {
    object@Composition[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_Albedo",
  signature = "swProd",
  function(object, value) {
    object@Albedo[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_CanopyHeight",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@CanopyHeight)
    object@CanopyHeight <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_VegInterParam",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@VegetationInterceptionParameters)
    object@VegetationInterceptionParameters <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_LitterInterParam",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@LitterInterceptionParameters)
    object@LitterInterceptionParameters <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_EsTpartitioning_param",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@EsTpartitioning_param)
    object@EsTpartitioning_param <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_Es_param_limit",
  signature = "swProd",
  function(object, value) {
    object@Es_param_limit[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_Shade",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@Shade)
    object@Shade <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro_use",
  signature = "swProd",
  function(object, value) {
    object@HydraulicRedistribution_use[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_HydrRedstro",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@HydraulicRedistribution)
    object@HydraulicRedistribution <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_CritSoilWaterPotential",
  signature = "swProd",
  function(object, value) {
    object@CriticalSoilWaterPotential[] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_CO2Coefficients",
  signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@CO2Coefficients)
    object@CO2Coefficients <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "numeric", value = "matrix"),
  function(object, vegtype, value) {
    id_vegtype <- as.integer(vegtype)
    dimnames(value) <- dimnames(object@MonthlyVeg[[id_vegtype]])
    object@MonthlyVeg[[id_vegtype]] <- value
    validObject(object)
    object
  }
)

#' @rdname swProd-class
#' @export
setReplaceMethod(
  "swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "character", value = "matrix"),
  function(object, vegtype, value) {
    id_vegtype <- grep(
      vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]),
      ignore.case = TRUE
    )
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
  }
)
