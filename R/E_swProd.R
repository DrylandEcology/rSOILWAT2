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


#' Vegetation (plant functional) types
#'
#' @param version A character string indicating the vegetation type version.
#' @param order A character string indicating the order of vegetation types
#' as relevant for `SOILWAT2` or `rSOILWAT2` (relevant only for type `"v1"`).
#'
#' @return `namesVegTypes()`: a named character vector that contains the
#' requested vegetation types.
#'
#' @section Details:
#' Vegetation type version `"v2"` was introduced with `rSOILWAT2` `v6.5.0`.
#'
#' @section Notes:
#' Names of vegetation type version `"v1"` varied and the order of types was
#' inconsistent.
#'
#' @examples
#' namesVegTypes("v1")
#' namesVegTypes("v2")
#'
#' @name VegTypes
#' @md
#' @export
namesVegTypes <- function(
    version = c("v2", "v1"),
  order = c("rSOILWAT2", "SOILWAT2")
) {
  ids <- switch(
    EXPR = match.arg(order),
    rSOILWAT2 = c(4L, 2L, 1L, 3L),
    SOILWAT2 = seq_along(rSW2_glovars[["kSOILWAT2"]][["VegTypeNames1"]])
  )
  switch(
    EXPR = match.arg(version),
    v1 = rSW2_glovars[["kSOILWAT2"]][["VegTypeNames1"]][ids],
    v2 = rSW2_glovars[["kSOILWAT2"]][["VegTypeNames2"]]
  )
}


#' @param request A character string defining the request to map one version
#' of vegetation types from a previous version.
#'
#' @return `mapVegTypes()`: indices that (approximately) map a new
#' from the old vegetation types, i.e.,
#' a named vector where 0 indicates no match.
#'
#' @examples
#' mapVegTypes()
#' stopifnot(names(mapVegTypes("2from1")) == namesVegTypes("v2"))
#'
#' cbind(
#'   v2 = namesVegTypes("v2")[mapVegTypes("2from1") > 0L],
#'   v1 = namesVegTypes("v1")[mapVegTypes("2from1")]
#' )
#'
#' @rdname VegTypes
#' @md
#' @export
mapVegTypes <- function(request = "2from1") {
  request <- match.arg(request)

  switch(
    EXPR = request,
    "2from1" = c(
      treeNL = 3L,
      treeBL = 0L,
      shrub = 2L,
      forbs = 4L,
      grassC3 = 1L,
      grassC4 = 0L
    )
  )
}

# veg1_names corresponds to old key2veg[] (< SOILWAT v8.3.0)
# and to rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPESv1"]]
veg1_names <- c("Grasses", "Shrubs", "Trees", "Forbs")
lc1_names <- c(veg1_names, "Bare Ground")
nvegs1 <- length(veg1_names)

#' Class \code{"swProd"}
#'
#' Class \code{"swProd"} is superseded by class \code{"swProd2"} starting with
#' \pkg{rSOILWAT2} v6.5.0 and \var{SOILWAT2} v8.3.0.
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
    Composition = stats::setNames(rep(NA_real_, nvegs1 + 1L), lc1_names),
    Albedo = stats::setNames(rep(NA_real_, nvegs1 + 1L), lc1_names),
    CanopyHeight = array(
      NA_real_,
      dim = c(5L, nvegs1),
      dimnames = list(
        c("xinflec", "yinflec", "range", "slope", "height_cm"),
        veg1_names
      )
    ),
    VegetationInterceptionParameters = array(
      NA_real_,
      dim = c(2L, nvegs1),
      dimnames = list(
        c("kSmax", "kdead"),
        veg1_names
      )
    ),
    LitterInterceptionParameters = array(
      NA_real_,
      dim = c(1L, nvegs1),
      dimnames = list(
        "kSmax",
        veg1_names
      )
    ),
    EsTpartitioning_param = stats::setNames(rep(NA_real_, nvegs1), veg1_names),
    Es_param_limit = stats::setNames(rep(NA_real_, nvegs1), veg1_names),
    Shade = array(
      NA_real_,
      dim = c(6L, nvegs1),
      dimnames = list(
        c(
          "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec",
          "yinflec", "range", "slope"
        ),
        veg1_names
      )
    ),
    HydraulicRedistribution_use = stats::setNames(rep(NA, nvegs1), veg1_names),
    HydraulicRedistribution = array(
      NA_real_,
      dim = c(3L, nvegs1),
      dimnames = list(
        c("MaxCondRoot", "SoilWaterPotential50", "ShapeCond"),
        veg1_names
      )
    ),
    CriticalSoilWaterPotential = stats::setNames(
      rep(NA_real_, nvegs1), veg1_names
    ),
    CO2Coefficients = array(
      NA_real_,
      dim = c(nvegs1, 4L),
      dimnames = list(
        veg1_names,
        c("Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2")
      )
    ),
    vegYear = NA_integer_,
    isBiomAsIf100Cover = NA,
    MonthlyVeg = stats::setNames(
      lapply(
        veg1_names,
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
      veg1_names
    )
  )
)



setValidity(
  "swProd",
  function(object) {
    val <- TRUE
    nvegs1 <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPESv1"]]

    if (length(object@veg_method) != 1L) {
      msg <- "@veg_method must have 1 value."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Composition) != 1L + nvegs1 ||
        !all(is.na(object@Composition) | (object@Composition >= 0. &
            object@Composition <= 1.))
    ) {
      msg <- paste(
        "@Composition must have 1 + NVEGTYPESv1 values",
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
      length(object@Albedo) != 1L + nvegs1 ||
        !all(is.na(object@Albedo) | (object@Albedo >= 0. & object@Albedo <= 1.))
    ) {
      msg <- paste(
        "@Albedo must have 1 + NVEGTYPESv1 values between 0 and 1 or NA."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CanopyHeight)
    if (!identical(temp, c(5L, nvegs1))) {
      msg <- "@CanopyHeight must be a 5 x NVEGTYPESv1 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@VegetationInterceptionParameters)
    if (!identical(temp, c(2L, nvegs1))) {
      msg <- paste(
        "@VegetationInterceptionParameters must be a 2 x NVEGTYPESv1 matrix."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@LitterInterceptionParameters)
    if (!identical(temp, c(1L, nvegs1))) {
      msg <- paste(
        "@LitterInterceptionParameters must be a 1 x NVEGTYPESv1 matrix."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@EsTpartitioning_param) != nvegs1) {
      msg <- "@EsTpartitioning_param must have NVEGTYPESv1 values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (
      length(object@Es_param_limit) != nvegs1 ||
        !all(is.na(object@Es_param_limit) | object@Es_param_limit >= 0.)
    ) {
      msg <- "@Es_param_limit must have NVEGTYPESv1 non-negative values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@Shade)
    if (!identical(temp, c(6L, nvegs1))) {
      msg <- "@Shade must be a 6 x NVEGTYPESv1 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@HydraulicRedistribution_use) != nvegs1) {
      msg <- "@HydraulicRedistribution_use must have NVEGTYPESv1 values."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@HydraulicRedistribution)
    if (!identical(temp, c(3L, nvegs1))) {
      msg <- "@HydraulicRedistribution must be a 3 x NVEGTYPESv1 matrix."
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    if (length(object@CriticalSoilWaterPotential) != nvegs1 ||
        !all(
          is.na(object@CriticalSoilWaterPotential) |
            object@CriticalSoilWaterPotential < 0.
        )
    ) {
      msg <- paste(
        "@CriticalSoilWaterPotential must have NVEGTYPESv1",
        "negative values."
      )
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    temp <- dim(object@CO2Coefficients)
    if (!identical(temp, c(nvegs1, 4L))) {
      msg <- "@CO2Coefficients must be a NVEGTYPESv1 x 4 matrix."
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
      length(object@MonthlyVeg) != nvegs1 ||
      !all(
        vapply(
          object@MonthlyVeg,
          function(x) identical(dim(x), c(12L, 4L)) && all(x >= 0 | is.na(x)),
          FUN.VALUE = NA
        )
      )
    ) {
      msg <- paste(
        "@MonthlyVeg must be a list with NVEGTYPESv1 elements of a",
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

# swProd() default values with rSOILWAT2 v6.4.0
swProdv640 <- function() {
  new(
    "swProd",
    veg_method = 0L,
    nYearsDynamicShort = 3L,
    nYearsDynamicLong = 30L,
    Composition = stats::setNames(rep(NA_real_, nvegs1 + 1L), lc1_names),
    Albedo = stats::setNames(c(0.167, 0.143, 0.106, 0.167, 0.15), lc1_names),
    CanopyHeight = structure(
      c(
        300, 29.5, 85, 0.002, 0,
        0, 5, 100, 0.003, 50,
        0, 5, 3000, 8e-05, 1200,
        300, 29.5, 85, 0.002, 0
      ),
      dim = c(5L, 4L),
      dimnames = list(
        c("xinflec", "yinflec", "range", "slope", "height_cm"),
        veg1_names
      )
    ),
    VegetationInterceptionParameters = structure(
      c(1, 1, 2.6, 0.1, 2, 0.01, 1, 0.5),
      dim = c(2L, 4L),
      dimnames = list(c("kSmax", "kdead"), veg1_names)
    ),
    LitterInterceptionParameters = structure(
      c(0.113, 0.113, 0.29, 0.113),
      dim = c(1L, 4L),
      dimnames = list("kSmax", veg1_names)
    ),
    EsTpartitioning_param = stats::setNames(c(1, 1, 0.41, 1), veg1_names),
    Es_param_limit = stats::setNames(c(999, 999, 2099, 999), veg1_names),
    Shade = structure(
      c(
        0.3, 150, 300, 12, 34, 0.002,
        0.3, 150, 300, 12, 34, 0.002,
        0.3, 150, 0, 0, 2, 2e-04,
        0.3, 150, 300, 12, 34, 0.002
      ),
      dim = c(6L, 4L),
      dimnames = list(
        c(
          "ShadeScale",
          "ShadeMaximalDeadBiomass",
          "tanfuncXinflec",
          "yinflec",
          "range",
          "slope"
        ),
        veg1_names
      )
    ),
    HydraulicRedistribution_use = stats::setNames(
      rep(TRUE, nvegs1), veg1_names
    ),
    HydraulicRedistribution = structure(
      c(
        -0.2328, 10, 3.22,
        -0.2328, 10, 3.22,
        -0.2328, 10, 3.22,
        -0.2328, 10, 3.22
      ),
      dim = c(3L, 4L),
      dimnames = list(
        c("MaxCondRoot", "SoilWaterPotential50", "ShapeCond"),
        veg1_names
      )
    ),
    CriticalSoilWaterPotential = stats::setNames(
      c(-3.5, -3.9, -2, -2), veg1_names
    ),
    CO2Coefficients = structure(
      c(
        0.1319, 0.1319, 0.1319, 0.1319,
        0.3442, 0.3442, 0.3442, 0.3442,
        25.158, 25.158, 25.158, 25.158,
        -0.548, -0.548, -0.548, -0.548
      ),
      dim = c(4L, 4L),
      dimnames = list(
        veg1_names,
        c("Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2")
      )
    ),
    vegYear = 1995L,
    isBiomAsIf100Cover = TRUE,
    MonthlyVeg = list(
      Trees = structure(
        c(
          rep(2000, 12L),
          rep(15000, 12L),
          rep(0.083, 12L),
          rep(500, 12L)
        ),
        dim = c(12L, 4L),
        dimnames = list(
          month.name, c("Litter", "Biomass", "Live_pct", "LAI_conv")
        )
      ),
      Shrubs = structure(
        c(
          85.4, 88.2, 95.3, 100.5, 166.4, 186, 177.1, 212.2, 157.4, 124.9, 110.4, 104.3, # nolint: line_length_linter.
          210, 212, 228, 272, 400, 404, 381, 352, 286, 235, 218, 214,
          0.06, 0.08, 0.2, 0.33, 0.57, 0.55, 0.5, 0.46, 0.32, 0.15, 0.08, 0.06,
          372, 372, 372, 372, 372, 372, 372, 372, 372, 372, 372, 372
        ),
        dim = c(12L, 4L),
        dimnames = list(
          month.name, c("Litter", "Biomass", "Live_pct", "LAI_conv")
        )
      ),
      Forbs = structure(
        c(
          75, 80, 85, 90, 50, 50, 50, 55, 60, 65, 70, 75,
          150, 150, 150, 170, 190, 220, 250, 220, 190, 180, 170, 160,
          0, 0, 0.1, 0.2, 0.4, 0.6, 0.4, 0.6, 0.4, 0.2, 0.1, 0,
          300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300
        ),
        dim = c(12L, 4L),
        dimnames = list(
          month.name, c("Litter", "Biomass", "Live_pct", "LAI_conv")
        )
      ),
      Grasses =
        structure(
          c(
            75, 80, 85, 90, 50, 50, 50, 55, 60, 65, 70, 75,
            150, 150, 150, 170, 190, 220, 250, 220, 190, 180, 170, 160,
            0, 0, 0.1, 0.2, 0.4, 0.6, 0.4, 0.6, 0.4, 0.2, 0.1, 0,
            300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300
          ),
          dim = c(12L, 4L),
          dimnames = list(
            month.name, c("Litter", "Biomass", "Live_pct", "LAI_conv")
          )
        )
    )
  )
}

#' @rdname swProd-class
#' @export
swProd <- function(...) {
  def <- swProdv640()
  sns <- slotNames("swProd")
  dots <- list(...)
  if (length(dots) == 1L && inherits(dots[[1]], "swProd")) {
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
    for (kveg in veg1_names) {
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
  function(object, verbose = FALSE) {
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
