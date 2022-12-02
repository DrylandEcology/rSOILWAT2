
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


#' Extract soil temperature
#'
#' @inheritParams get_derived_output
#' @param levels A character string indicating which
#'   within-day minimum, average, or maximum values are requested.
#' @param surface A logical value. Output surface soil temperature as first
#'   column in returned matrices;
#'   equivalent to requesting a `0` via `soillayers`.
#' @param soillayers An integer vector of requested soil layers.
#'   `NULL` returns soil temperature from all available soil layers;
#'   `NA` does not return soil temperature;
#'   including a `0` is equivalent to setting `surface` to `TRUE`.
#' @param verbose A logical value. Issue warnings if requested `levels` are
#'   not available.
#'
#' @return A named list of length `levels` where elements are
#' numeric matrices of soil temperature `[C]` with time steps as rows
#' and (requested) soil surface and soil layers as columns.
#'
#' @section Notes:
#' Requested `soillayers` that are not available in `x` are ignored
#' (with a warning if `verbose`).
#'
#' @section Notes:
#' Requested `levels` that are not available
#' (e.g., `"min"` before `rSOILWAT2` `v5.3.0`)
#' are replaced by values from the average level
#' (with a warning if `verbose`).
#'
#' @examples
#' sw_out <- sw_exec(inputData = rSOILWAT2::sw_exampleData)
#' get_soiltemp(sw_out, "Month")
#' get_soiltemp(sw_out, "Month", "avg")
#' get_soiltemp(sw_out, "Month", "avg", surface = TRUE, soillayers = NA)
#' get_soiltemp(sw_out, "Month", "avg", soillayers = 0)
#' get_soiltemp(sw_out, "Month", "avg", surface = FALSE, soillayers = NULL)
#' get_soiltemp(sw_out, "Month", "avg", surface = FALSE, soillayers = c(1, 3))
#' get_soiltemp(sw_out, "Month", "avg", surface = FALSE, soillayers = c(1, 30))
#'
#' @md
#' @export
get_soiltemp <- function(
  x,
  timestep = c("Day", "Week", "Month", "Year"),
  levels = c("min", "avg", "max"),
  surface = TRUE,
  soillayers = NULL,
  verbose = FALSE
) {
  timestep <- match.arg(timestep)
  levels <- match.arg(levels, several.ok = TRUE)

  #--- Deal with`soillayers`: NA, NULL, or integer vector
  if (!isTRUE(is.na(soillayers)) && !is.null(soillayers)) {
    soillayers <- sort(unique(as.integer(soillayers)))
  }

  #--- Deal with `0` for surface in `soillayers`
  if (0L %in% soillayers) {
    surface <- TRUE
    tmp <- setdiff(soillayers, 0L)
    soillayers <- if (length(tmp) > 0) tmp else NA
  }

  #--- Version of output object
  is_ge_v5.3.0 <- check_version(x, "5.3.0")

  #--- Load surface temperatures if requested
  req_sf <- as.logical(surface)[1]

  if (req_sf) {
    tmp_sf <- slot(slot(x, sw_out_flags()["sw_temp"]), timestep)

    if (nrow(tmp_sf) == 0) {
      stop(
        "Simulation run without producing soil surface temperature output: ",
        "consider turning on output key ",
        shQuote(sw_out_flags()["sw_temp"]),
        "."
      )
    }

    cns_sf <- if (is_ge_v5.3.0) {
      # rSOILWAT2 since v5.3.0: `surfaceTemp_min/avg/max_C`
      grep("surfaceTemp_[[:alpha:]]{3}_C", colnames(tmp_sf), value = TRUE)
    } else {
      # rSOILWAT2 before v5.3.0: `surfaceTemp_C`
      "surfaceTemp_C"
    }
  }


  #--- Load soil temperatures at depth if requested
  req_sl <- !isTRUE(is.na(soillayers)) || is.null(soillayers)

  if (req_sl) {
    tmp_sl <- slot(slot(x, sw_out_flags()["sw_soiltemp"]), timestep)

    if (nrow(tmp_sl) == 0) {
      stop(
        "Simulation run without producing soil temperature output: ",
        "consider turning on output key ",
        shQuote(sw_out_flags()["sw_soiltemp"]),
        "."
      )
    }

    # rSOILWAT2 before v5.3.0: `Lyr_1`, ...
    # rSOILWAT2 since v5.3.0: `Lyr_1_max_C`, `Lyr_1_min_C`, `Lyr_1_avg_C`, ...
    if (is.null(soillayers)) {
      cns_sl <- grep("Lyr_", colnames(tmp_sl), fixed = TRUE, value = TRUE)
    } else {
      tmp <- lapply(
        soillayers,
        function(k) {
          # match `Lyr_1_max_C`, not `Lyr_10_max_C`, for k = 1 if v(x) >= v5.3.0
          # match `Lyr_1`, not `Lyr_10`, for k = 1 if v(x) < v5.3.0
          grep(
            paste0("Lyr_", k, if (is_ge_v5.3.0) "_" else "$"),
            colnames(tmp_sl),
            fixed = is_ge_v5.3.0,
            value = TRUE
          )
        }
      )

      cns_sl <- unlist(tmp)
      req_has_sl <- lengths(tmp) > 0

      if (verbose) {
        if (length(soillayers) != sum(req_has_sl)) {
          warning("Some requested `soillayers` are not available.")
        }
      }

      soillayers <- soillayers[req_has_sl]
      if (length(soillayers) == 0) {
        # None of the requested layers are available
        soillayers <- NA
        req_sl <- FALSE
      }
    }
  }


  #--- Extract requested levels and depths
  res <- lapply(
    levels,
    function(lvl) {
      res_sf <- if (req_sf) {
        cns_sf_lvl <- grep(
          paste0("surfaceTemp_", lvl, "_C"),
          cns_sf,
          value = TRUE
        )

        if (is_ge_v5.3.0 && length(cns_sf_lvl) > 0) {
          tmp_sf[, cns_sf_lvl, drop = FALSE] # rSOILWAT2 since v5.3.0
        } else {
          # Repeat average for each requested level
          if (verbose) {
            warning(
              shQuote(lvl),
              " soil surface temperature not available: average used instead."
            )
          }
          tmp_sf[, cns_sf, drop = FALSE] # rSOILWAT2 before 5.3.0
        }
      }

      res_sl <- if (req_sl) {
        cns_sl_lvl <- grep(
          paste0("Lyr_[[:digit:]]+_", lvl, "_C"),
          cns_sl,
          value = TRUE
        )

        if (is_ge_v5.3.0 && length(cns_sl_lvl) > 0) {
          tmp_sl[, cns_sl_lvl, drop = FALSE] # rSOILWAT2 since v5.3.0
        } else {
          # Repeat average for each requested level
          if (verbose) {
            warning(
              shQuote(lvl),
              " soil temperature not available: average used instead."
            )
          }
          tmp_sl[, cns_sl, drop = FALSE] # rSOILWAT2 before 5.3.0
        }
      }

      cbind(res_sf, res_sl)
    }
  )

  names(res) <- levels
  res
}
