
#' Calculate derived variables from output
#'
#' @param x An object of class \code{\linkS4class{swOutput}}.
#' @param timestep A character string. One of the \pkg{rSOILWAT2} time steps.
#'
#' @name get_derived_output
NULL

#' Calculate transpiration from output
#'
#' @inheritParams get_derived_output
#'
#' @return A numeric vector of transpiration [mm] for each time step.
#'
#' @examples
#' sw_out <- sw_exec(inputData = rSOILWAT2::sw_exampleData)
#' get_transpiration(sw_out, "Month")
#'
#' @export
get_transpiration <- function(x, timestep = c("Day", "Week", "Month", "Year")) {
  timestep <- match.arg(timestep)

  res <- NULL

  tmp <- slot(slot(x, "AET"), timestep)

  if (all(dim(tmp) > 0)) {
    res <- tmp[, "tran_cm"]

  } else {
    tmp <- slot(slot(x, "TRANSP"), timestep)

    if (all(dim(tmp) > 0)) {
      ids <- grep("transp_total_Lyr", colnames(tmp), fixed = TRUE)
      res <- apply(tmp[, ids, drop = FALSE], MARGIN = 1, FUN = sum)

    } else {
      stop(
        "Simulation run without producing transpiration output: ",
        "consider turning on output keys 'AET' or 'TRANSP'."
      )
    }
  }

  # convert [cm] to [mm]
  10 * res
}

#' Calculate evaporation from output
#'
#' @inheritParams get_derived_output
#'
#' @return A numeric vector of evaporation [mm] for each time step.
#'
#' @examples
#' sw_out <- sw_exec(inputData = rSOILWAT2::sw_exampleData)
#' get_evaporation(sw_out, "Month")
#'
#' @export
get_evaporation <- function(x, timestep = c("Day", "Week", "Month", "Year")) {
  timestep <- match.arg(timestep)

  res <- NULL

  tmp <- slot(slot(x, "AET"), timestep)

  if (all(dim(tmp) > 0)) {
    res <- tmp[, "evapotr_cm"] - tmp[, "tran_cm"]

  } else {
    tmp1 <- slot(slot(x, "EVAPSURFACE"), timestep)
    tmp2 <- slot(slot(x, "EVAPSOIL"), timestep)
    tmp3 <- slot(slot(x, "PRECIP"), timestep)

    if (all(dim(tmp1) > 0, dim(tmp2) > 0, dim(tmp3) > 0)) {
      ids <- grep("Lyr", colnames(tmp2), fixed = TRUE)
      res <-
        # evaporation from surface water (canopy, litter, ponded)
        tmp1[, "evap_total"] +
        # evaporation from bare soil
        apply(tmp2[, ids, drop = FALSE], 1, sum) +
        # evaporation from snow (sublimation)
        tmp3[, "snowloss"]

    } else {
      stop(
        "Simulation run without producing evaporation output: ",
        "consider turning on output keys 'AET' or ",
        "'EVAPSURFACE', 'EVAPSOIL', and 'PRECIP'."
      )
    }
  }

  # convert [cm] to [mm]
  10 * res
}
