
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
  tmp <- slot(slot(x, "TRANSP"), timestep)

  # convert [cm] to [mm]
  10. * apply(
    X = tmp[, grep("transp_total_Lyr", colnames(tmp)), drop = FALSE],
    MARGIN = 1,
    FUN = sum
  )
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

  tmp <- slot(slot(x, "EVAPSURFACE"), timestep)
  Etotalsurf <- tmp[, "evap_total"]

  tmp <- slot(slot(x, "EVAPSOIL"), timestep)
  Esoil <- apply(tmp[, grep("Lyr", colnames(tmp)), drop = FALSE], 1, sum)

  Esnow <- slot(slot(x, "PRECIP"), timestep)[, "snowloss"]

  # convert [cm] to [mm]
  10 * (Etotalsurf + Esoil + Esnow)
}
