
#' Calculate derived variables from output
#'
#' @param x An object of class \code{\linkS4class{swOutput}}.
#' @param timestep A character string. One of the \pkg{rSOILWAT2} time steps.
#' @param keep_time A logical value. Include time information in the returned
#'   object.
#'
#' @name get_derived_output
NULL


#' Output column indices with time information
#'
#' @inheritParams get_derived_output
#'
#' @examples
#' time_columns("Month")
#'
#' @export
time_columns <- function(timestep = c("Day", "Week", "Month", "Year")) {
  switch(
    EXPR = match.arg(timestep),
    Year = 1L,
    Month = ,
    Week = ,
    Day = 1L:2L
  )
}

#' Number of time steps in output
#'
#' @inheritParams get_derived_output
#'
#' @examples
#' nrow_output(sw_exec(rSOILWAT2::sw_exampleData), "Month")
#'
#' @export
nrow_output <- function(x, timestep = c("Day", "Week", "Month", "Year")) {
  slot(
    x,
    switch(
      EXPR = match.arg(timestep),
      Day = "dy_nrow",
      Week = "wk_nrow",
      Month = "mo_nrow",
      Year = "yr_nrow"
    )
  )
}


#' Calculate transpiration from output
#'
#' @inheritParams get_derived_output
#'
#' @return A numeric vector of transpiration [mm] for each time step or
#'   a numeric matrix if `keep_time`.
#'
#' @examples
#' sw_out <- sw_exec(inputData = rSOILWAT2::sw_exampleData)
#' get_transpiration(sw_out, "Month")
#' get_transpiration(sw_out, "Month", keep_time = TRUE)
#'
#' @export
get_transpiration <- function(
  x,
  timestep = c("Day", "Week", "Month", "Year"),
  keep_time = FALSE
) {
  timestep <- match.arg(timestep)

  res <- NULL

  tmp <- slot(slot(x, "AET"), timestep)

  if (all(dim(tmp) > 0) && "tran_cm" %in% colnames(tmp)) {
    # "tran_cm" output was added with SOILWAT2 v6.2.0 and rSOILWAT2 v5.0.0
    res <- tmp[, "tran_cm"]

    if (keep_time) {
      res_time <- tmp[, time_columns(timestep), drop = FALSE]
    }

  } else {
    tmp <- slot(slot(x, "TRANSP"), timestep)
    ids <- grep("transp_total_Lyr", colnames(tmp), fixed = TRUE)

    if (all(dim(tmp) > 0) && length(ids) > 0) {
      res <- rowSums(tmp[, ids, drop = FALSE])

      if (keep_time) {
        res_time <- tmp[, time_columns(timestep), drop = FALSE]
      }

    } else {
      stop(
        "Simulation run without producing transpiration output: ",
        "consider turning on output keys 'AET' or 'TRANSP'."
      )
    }
  }

  if (keep_time) {
    cbind(
      res_time,
      T_mm = 10. * res
    )
  } else {
    # convert [cm] to [mm]
    10. * res
  }
}


#' Calculate evaporation from output
#'
#' @inheritParams get_derived_output
#'
#' @return A numeric vector of evaporation [mm] for each time step or
#'   a numeric matrix if `keep_time`.
#'
#' @examples
#' sw_out <- sw_exec(inputData = rSOILWAT2::sw_exampleData)
#' get_evaporation(sw_out, "Month")
#'
#' @export
get_evaporation <- function(
  x,
  timestep = c("Day", "Week", "Month", "Year"),
  keep_time = FALSE
) {
  timestep <- match.arg(timestep)

  res <- NULL

  tmp <- slot(slot(x, "AET"), timestep)

  if (
    all(dim(tmp) > 0) && all(c("evapotr_cm", "tran_cm") %in% colnames(tmp))
  ) {
    # "tran_cm" output was added with SOILWAT2 v6.2.0 and rSOILWAT2 v5.0.0
    res <- tmp[, "evapotr_cm"] - tmp[, "tran_cm"]

    if (keep_time) {
      res_time <- tmp[, time_columns(timestep), drop = FALSE]
    }

  } else {
    tmp1 <- slot(slot(x, "EVAPSURFACE"), timestep)
    tmp2 <- slot(slot(x, "EVAPSOIL"), timestep)
    tmp3 <- slot(slot(x, "PRECIP"), timestep)
    ids <- grep("Lyr", colnames(tmp2), fixed = TRUE)

    if (
      all(
        dim(tmp1) > 0,
        dim(tmp2) > 0,
        dim(tmp3) > 0,
        length(ids) > 0,
        "evap_total" %in% colnames(tmp1),
        "snowloss" %in% colnames(tmp3)
      )
    ) {
      res <-
        # evaporation from surface water (canopy, litter, ponded)
        tmp1[, "evap_total"] +
        # evaporation from bare soil
        rowSums(tmp2[, ids, drop = FALSE]) +
        # evaporation from snow (sublimation)
        tmp3[, "snowloss"]

      if (keep_time) {
        res_time <- tmp1[, time_columns(timestep), drop = FALSE]
      }

    } else {
      stop(
        "Simulation run without producing evaporation output: ",
        "consider turning on output keys 'AET' or ",
        "'EVAPSURFACE', 'EVAPSOIL', and 'PRECIP'."
      )
    }
  }

  if (keep_time) {
    cbind(
      res_time,
      E_mm = 10. * res
    )
  } else {
    # convert [cm] to [mm]
    10. * res
  }
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
  keep_time = FALSE,
  verbose = FALSE
) {
  timestep <- match.arg(timestep)
  levels <- match.arg(levels, several.ok = TRUE)

  res_time <- NULL

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

    if (keep_time) {
      res_time <- tmp_sf[, time_columns(timestep), drop = FALSE]
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

    if (keep_time && is.null(res_time)) {
      res_time <- tmp_sl[, time_columns(timestep), drop = FALSE]
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

      if (verbose && length(soillayers) != sum(req_has_sl)) {
        warning("Some requested `soillayers` are not available.")
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

      cbind(if (keep_time) res_time, res_sf, res_sl)
    }
  )

  names(res) <- levels
  res
}



#' Extract or calculate soil moisture
#'
#' @inheritParams get_derived_output
#' @param type A character string selecting type of soil moisture.
#' @param swInput An object of class [swInputData-class].
#' @param widths_cm A numeric vector of soil layer widths (units `[cm]`).
#' @param fcoarse A numeric vector of coarse fragments per soil layer
#'   (units `[volume fraction]`).
#'
#' @section Details:
#' Information on soil layer `widths` and coarse fragments `fcoarse`
#' are only used if requested type of soil moisture is
#' not available and has to be calculated from a different type.
#' `widths` and `fcoarse` may be provided directly or via `swInput`
#' from which the information is extracted (see examples).
#'
#' @return A data frame with requested soil moisture;
#' rows represent time steps and columns represent soil layers.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#'
#' sw_out <- sw_exec(inputData = sw_in)
#' res1 <- get_soilmoisture(sw_out, "Month", type = "swc")
#'
#' deactivate_swOUT_OutKey(sw_in) <- sw_out_flags()[["sw_swcbulk"]]
#' sw_out <- sw_exec(inputData = sw_in)
#' res2 <- get_soilmoisture(sw_out, "Month", type = "swc", swInput = sw_in)
#' all.equal(res1, res2)
#'
#' res3 <- get_soilmoisture(
#'   sw_out,
#'   timestep = "Month",
#'   type = "swc",
#'   widths = diff(c(0., swSoils_Layers(sw_in)[, "depth_cm"])),
#'   fcoarse = swSoils_Layers(sw_in)[, "gravel_content"]
#' )
#' all.equal(res1, res3)
#'
#' @md
#' @export
get_soilmoisture <- function(
  x,
  timestep = c("Day", "Week", "Month", "Year"),
  type = c("swc", "vwc_bulk", "vwc_matric"),
  swInput = NULL,
  widths_cm = NULL,
  fcoarse = NULL,
  keep_time = FALSE
) {
  timestep <- match.arg(timestep)
  type <- match.arg(type)

  out_flag <- switch(
    EXPR = type,
    swc = sw_out_flags()[["sw_swcbulk"]],
    vwc_bulk = sw_out_flags()[["sw_vwcbulk"]],
    vwc_matric = sw_out_flags()[["sw_vwcmatric"]]
  )


  res <- NULL
  msg <- NULL

  tmp <- slot(slot(x, out_flag), timestep)
  icols <- -time_columns(timestep)


  if (nrow(tmp) > 0) {
    res <- tmp[, icols, drop = FALSE]

    if (keep_time) {
      res_time <- tmp[, time_columns(timestep), drop = FALSE]
    }

  } else {
    #--- Requested soil moisture output not stored in simulation output `x`

    # Check if any of the other soil moisture types are available
    tmp_swc <- slot(slot(x, sw_out_flags()[["sw_swcbulk"]]), timestep)
    tmp_vwcbulk <- slot(slot(x, sw_out_flags()[["sw_vwcbulk"]]), timestep)
    tmp_vwcmatric <- slot(slot(x, sw_out_flags()[["sw_vwcmatric"]]), timestep)

    has_swc <- nrow(tmp_swc) > 0L
    has_vwcbulk <- nrow(tmp_vwcbulk) > 0L
    has_vwcmatric <- nrow(tmp_vwcmatric) > 0L

    if (any(has_swc, has_vwcbulk, has_vwcmatric)) {
      # Determine whether we have enough soil information for calculations
      has_soil <-
        inherits(swInput, "swInputData") ||
        !any(is.null(widths_cm), is.null(fcoarse))

      if (has_soil) {
        if (is.null(widths_cm)) {
          widths_cm <- diff(c(0., swSoils_Layers(swInput)[, "depth_cm"]))
        }

        one_minus_fcoarse <- 1. - if (is.null(fcoarse)) {
          swSoils_Layers(swInput)[, "gravel_content"]
        } else {
          fcoarse
        }

        if (type == "swc") {
          if (has_vwcbulk) {
            # calculate swc as depth * vwc_bulk
            res <- sweep(
              tmp_vwcbulk[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = widths_cm,
              FUN = "*"
            )
            if (keep_time) {
              res_time <- tmp_vwcbulk[, time_columns(timestep), drop = FALSE]
            }

          } else if (has_vwcmatric) {
            # calculate swc as depth * vwc_matric * (1 - fcoarse)
            res <- sweep(
              tmp_vwcmatric[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = widths_cm * one_minus_fcoarse,
              FUN = "*"
            )
            if (keep_time) {
              res_time <- tmp_vwcmatric[, time_columns(timestep), drop = FALSE]
            }
          }

        } else if (type == "vwc_bulk") {
          if (has_swc) {
            # calculate vwc_bulk as swc / depth
            res <- sweep(
              tmp_swc[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = widths_cm,
              FUN = "/"
            )
            if (keep_time) {
              res_time <- tmp_swc[, time_columns(timestep), drop = FALSE]
            }

          } else if (has_vwcmatric) {
            # calculate vwc_bulk as vwc_matric * (1 - fcoarse)
            res <- sweep(
              tmp_vwcmatric[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = one_minus_fcoarse,
              FUN = "*"
            )
            if (keep_time) {
              res_time <- tmp_vwcmatric[, time_columns(timestep), drop = FALSE]
            }
          }

        } else if (type == "vwc_matric") {
          if (has_swc) {
            # calculate vwc_matric as swc / (depth * (1 - fcoarse))
            res <- sweep(
              tmp_swc[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = widths_cm * one_minus_fcoarse,
              FUN = "/"
            )
            if (keep_time) {
              res_time <- tmp_swc[, time_columns(timestep), drop = FALSE]
            }

          } else if (has_vwcbulk) {
            # calculate vwc_matric as vwc_bulk / (1 - fcoarse)
            res <- sweep(
              tmp_vwcbulk[, icols, drop = FALSE],
              MARGIN = 2L,
              STATS = one_minus_fcoarse,
              FUN = "/"
            )
            if (keep_time) {
              res_time <- tmp_vwcbulk[, time_columns(timestep), drop = FALSE]
            }
          }
        }

      } else {
        msg <- paste(
          "Simulation run without requested soil moisture output:",
          "converting available to requested output requires",
          "`swInput` or, alternatively, `widths_cm` and `fcoarse`."
        )
      }


    } else {
      msg <- paste(
        "Simulation run without producing soil moisture output:",
        "consider turning output on for at least one of",
        "'SWCBULK', 'VWCBULK', or 'VWCMATRIC'."
      )
    }
  }

  if (!is.null(msg)) {
    stop(msg)
  }

  if (keep_time) {
    cbind(res_time, res)
  } else {
    res
  }
}
