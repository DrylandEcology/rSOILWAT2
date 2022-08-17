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


#' Class \code{"swProd"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swProd}}.
#' @param .Object An object of class \code{\linkS4class{swProd}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#' @param vegtype The name or index of the vegetation type.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swProd")
#' x <- new("swProd")
#'
#' @name swProd-class
#' @export
setClass("swProd", slots = c(
  veg_method = "integer",
  Composition = "numeric", Albedo = "numeric",
  CanopyHeight = "matrix",
  VegetationInterceptionParameters = "matrix",
  LitterInterceptionParameters = "matrix",
  EsTpartitioning_param = "numeric", Es_param_limit = "numeric",
  Shade = "matrix",
  HydraulicRedistribution_use = "logical", HydraulicRedistribution = "matrix",
  CriticalSoilWaterPotential = "numeric", CO2Coefficients = "matrix",
  MonthlyVeg = "list"))


swProd_validity <- function(object) {
  val <- TRUE
  nvegs <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]

  if (length(object@veg_method) != 1 ||
    !all(is.na(object@veg_method) | (object@veg_method >= 0) & object@veg_method <= 1)) {
    msg <- "@veg_method must have 1 values between 0 and 1."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@Composition) != 1 + nvegs ||
    !all(is.na(object@Composition) | (object@Composition >= 0 &
        object@Composition <= 1))) {
    msg <- "@Composition must have 1 + NVEGTYPES values between 0 and 1 or NA."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@Albedo) != 1 + nvegs ||
    !all(is.na(object@Albedo) | (object@Albedo >= 0 & object@Albedo <= 1))) {
    msg <- "@Albedo must have 1 + NVEGTYPES values between 0 and 1 or NA."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@CanopyHeight)
  if (identical(temp, c(5, nvegs))) {
    msg <- "@CanopyHeight must be a 5xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@VegetationInterceptionParameters)
  if (identical(temp, c(2, nvegs))) {
    msg <- "@VegetationInterceptionParameters must be a 4xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@LitterInterceptionParameters)
  if (identical(temp, c(1, nvegs))) {
    msg <- "@LitterInterceptionParameters must be a 1xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@EsTpartitioning_param) != nvegs ||
    !all(is.finite(object@EsTpartitioning_param))) {
    msg <- "@EsTpartitioning_param must have NVEGTYPES finite values."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@Es_param_limit) != nvegs ||
      !all(object@Es_param_limit >= 0)) {
    msg <- "@Es_param_limit must have NVEGTYPES non-negative values."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@Shade)
  if (identical(temp, c(6, nvegs))) {
    msg <- "@Shade must be a 6xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@HydraulicRedistribution_use) != nvegs) {
    msg <- "@HydraulicRedistribution_use must have NVEGTYPES values."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@HydraulicRedistribution)
  if (identical(temp, c(3, nvegs))) {
    msg <- "@HydraulicRedistribution must be a 3xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@CriticalSoilWaterPotential) != nvegs ||
    !all(object@CriticalSoilWaterPotential < 0)) {
    msg <- "@CriticalSoilWaterPotential must have NVEGTYPES negative values."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  temp <- dim(object@CO2Coefficients)
  if (identical(temp, c(4, nvegs))) {
    msg <- "@CO2Coefficients must be a 4xNVEGTYPES matrix."
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (length(object@MonthlyVeg) != nvegs ||
    any(sapply(object@MonthlyVeg,
      function(x) !identical(dim(x), c(12L, 4L))))) {
    msg <- paste("@MonthlyVeg must be a list with NVEGTYPES elements of a",
      "12x4 matrix.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swProd", swProd_validity)

#' @rdname swProd-class
#' @export
setMethod("initialize", signature = "swProd", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "prod")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  if(!("veg_method") %in% dns) {
      def@veg_method[] <- NA_integer_
  }

  # We don't set values for slot `Composition`; this is to prevent simulation
  # runs with accidentally incorrect values
  if (!("Composition" %in% dns)) {
    def@Composition[] <- NA_real_
  }

  # Guarantee dimnames of dots arguments
  gdns <- c("CanopyHeight", "VegetationInterceptionParameters",
    "LitterInterceptionParameters", "HydraulicRedistribution",
    "CO2Coefficients", "MonthlyVeg")

  for (g in gdns) if (g %in% dns) {
    dimnames(dots[[g]]) <- dimnames(slot(def, g))
  }

  # Initialize values
  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }

  if (FALSE) {
    # not needed because no relevant inheritance
    .Object <- callNextMethod(.Object, ...)
  }

  validObject(.Object)
  .Object
})


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
setMethod("swProd_VegInterParam", "swProd",
  function(object) object@VegetationInterceptionParameters)

#' @rdname swProd-class
#' @export
setMethod("swProd_LitterInterParam", "swProd",
  function(object) object@LitterInterceptionParameters)

#' @rdname swProd-class
#' @export
setMethod("swProd_EsTpartitioning_param", "swProd",
  function(object) object@EsTpartitioning_param)

#' @rdname swProd-class
#' @export
setMethod("swProd_Es_param_limit", "swProd",
  function(object) object@Es_param_limit)

#' @rdname swProd-class
#' @export
setMethod("swProd_Shade", "swProd", function(object) object@Shade)

#' @rdname swProd-class
#' @export
setMethod("swProd_HydrRedstro_use", "swProd",
  function(object) object@HydraulicRedistribution_use)

#' @rdname swProd-class
#' @export
setMethod("swProd_HydrRedstro", "swProd",
  function(object) object@HydraulicRedistribution)

#' @rdname swProd-class
#' @export
setMethod("swProd_CritSoilWaterPotential", "swProd",
  function(object) object@CriticalSoilWaterPotential)

#' @rdname swProd-class
#' @export
setMethod("swProd_CO2Coefficients", "swProd",
  function(object) object@CO2Coefficients)

#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "numeric"),
  function(object, vegtype) object@MonthlyVeg[[as.integer(vegtype)]])

#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "character"),
  function(object, vegtype) {
    id_vegtype <- grep(vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]), ignore.case = TRUE)
    object@MonthlyVeg[[id_vegtype]]
})

#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_grass", "swProd", function(object) {
  object@MonthlyVeg[[1 +
      rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_GRASS"]]]]
})
#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_shrub", "swProd", function(object) {
  object@MonthlyVeg[[1 +
      rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_SHRUB"]]]]
})
#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_tree", "swProd", function(object) {
  object@MonthlyVeg[[1 +
      rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_TREES"]]]]
})
#' @rdname swProd-class
#' @export
setMethod("swProd_MonProd_forb", "swProd", function(object) {
  object@MonthlyVeg[[1 +
      rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_FORBS"]]]]
})

#' @rdname swProd-class
#' @export
setReplaceMethod("set_swProd", signature = "swProd", function(object, value) {
  object <- value
  validObject(object)
  object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_Composition", signature = "swProd",
  function(object, value) {
    object@Composition[] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_Albedo", signature = "swProd",
  function(object, value) {
    object@Albedo[] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_CanopyHeight", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@CanopyHeight)
    object@CanopyHeight <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_VegInterParam", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@VegetationInterceptionParameters)
    object@VegetationInterceptionParameters <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_LitterInterParam", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@LitterInterceptionParameters)
    object@LitterInterceptionParameters <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_EsTpartitioning_param", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@EsTpartitioning_param)
    object@EsTpartitioning_param <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_Es_param_limit", signature = "swProd",
  function(object, value) {
    object@Es_param_limit[] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_Shade", signature = "swProd", function(object, value) {
  dimnames(value) <- dimnames(object@Shade)
  object@Shade <- value
  validObject(object)
  object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_HydrRedstro_use", signature = "swProd",
  function(object, value) {
    object@HydraulicRedistribution_use[] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_HydrRedstro", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@HydraulicRedistribution)
    object@HydraulicRedistribution <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_CritSoilWaterPotential", signature = "swProd",
  function(object, value) {
    object@CriticalSoilWaterPotential[] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_CO2Coefficients", signature = "swProd",
  function(object, value) {
    dimnames(value) <- dimnames(object@CO2Coefficients)
    object@CO2Coefficients <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "numeric", value = "matrix"),
  function(object, vegtype, value) {
    id_vegtype <- as.integer(vegtype)
    dimnames(value) <- dimnames(object@MonthlyVeg[[id_vegtype]])
    object@MonthlyVeg[[id_vegtype]] <- value
    validObject(object)
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_veg",
  signature = c(object = "swProd", vegtype = "character", value = "matrix"),
  function(object, vegtype, value) {
    id_vegtype <- grep(vegtype,
      names(rSW2_glovars[["kSOILWAT2"]][["VegTypes"]]), ignore.case = TRUE)
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_grass", signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_GRASS"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_shrub", signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_SHRUB"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
})

#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_tree", signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_TREES"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
})
#' @rdname swProd-class
#' @export
setReplaceMethod("swProd_MonProd_forb", signature = "swProd",
  function(object, value) {
    id_vegtype <- 1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_FORBS"]]
    swProd_MonProd_veg(object, id_vegtype) <- value
    object
})


#' @rdname swProd-class
#' @export
# nolint start
setMethod("swReadLines", signature = c(object = "swProd", file = "character"),
  function(object, file) {
    print("TODO: method 'swReadLines' is not up-to-date; hard-coded indices are incorrect")
    infiletext <- readLines(con = file)
    object@Composition = readNumerics(infiletext[6],5)
    object@Albedo = readNumerics(infiletext[11],5)
    object@CanopyHeight[1,] = readNumerics(infiletext[21],4)
    object@CanopyHeight[2,] = readNumerics(infiletext[22],4)
    object@CanopyHeight[3,] = readNumerics(infiletext[23],4)
    object@CanopyHeight[4,] = readNumerics(infiletext[24],4)
    object@CanopyHeight[5,] = readNumerics(infiletext[25],4)
    object@VegetationInterceptionParameters[1,] = readNumerics(infiletext[30],4)
    object@VegetationInterceptionParameters[2,] = readNumerics(infiletext[31],4)
    object@LitterInterceptionParameters[1,] = readNumerics(infiletext[38],4)
    object@EsTpartitioning_param = readNumerics(infiletext[46],4)
    object@Es_param_limit = readNumerics(infiletext[51],4)
    object@Shade[1,] = readNumerics(infiletext[56],4)
    object@Shade[2,] = readNumerics(infiletext[57],4)
    object@Shade[3,] = readNumerics(infiletext[58],4)
    object@Shade[4,] = readNumerics(infiletext[59],4)
    object@Shade[5,] = readNumerics(infiletext[60],4)
    object@Shade[6,] = readNumerics(infiletext[61],4)
    object@HydraulicRedistribution_use = as.logical(as.integer(readNumerics(infiletext[66],4)))
    object@HydraulicRedistribution[1,] = readNumerics(infiletext[67],4)
    object@HydraulicRedistribution[2,] = readNumerics(infiletext[68],4)
    object@HydraulicRedistribution[3,] = readNumerics(infiletext[69],4)
    object@CriticalSoilWaterPotential = readNumerics(infiletext[74],4)
    for(i in 1:4)  object@CO2Coefficients[i, ] = readNumerics(infiletext[79 + i], 4)
    for(i in 1:12) object@MonthlyVeg[[1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_GRASS"]]]][i, ] = readNumerics(infiletext[94+i],4)
    for(i in 1:12) object@MonthlyVeg[[1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_SHRUB"]]]][i, ] = readNumerics(infiletext[109+i],4)
    for(i in 1:12) object@MonthlyVeg[[1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_TREES"]]]][i, ] = readNumerics(infiletext[124+i],4)
    for(i in 1:12) object@MonthlyVeg[[1 + rSW2_glovars[["kSOILWAT2"]][["VegTypes"]][["SW_FORBS"]]]][i, ] = readNumerics(infiletext[139+i],4)
    return(object)
})
# nolint end
