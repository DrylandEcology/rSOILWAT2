########################
#------ SMTR functions

# Based on references provided by Chambers, J. C., D. A. Pyke, J. D. Maestas, M.
# Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer,
# and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce
# Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush
# Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen.
# Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky
# Mountain Research Station, Fort Collins, CO.
#

#' Categories of soil temperature regimes and soil moisture regimes
#'
#' @section Definitions: Soil temperature and moisture regimes are defined in
#'   SSS 2014. Our operationalization is explained in the vignette
#'   \var{SoilMoistureRegimes_SoilTemperatureRegimes}.
#'
#' @references Soil Survey Staff. 2014. Keys to soil taxonomy, 12th ed., USDA
#'   Natural Resources Conservation Service, Washington, DC.
#' @examples
#'  vignette("SoilMoistureRegimes_SoilTemperatureRegimes",
#'    package = "rSOILWAT2")
#' @name STMR
NULL

#' Soil temperature regime categories
#' @rdname STMR
#' @export
STR_names <- function() {
  c("Hyperthermic", "Thermic", "Mesic", "Frigid", "Cryic", "Gelic")
}

#' Soil moisture regime categories
#' @rdname STMR
#' @export
SMR_names <- function() {
  c("Anhydrous", "Aridic", "Xeric", "Ustic", "Udic", "Perudic", "Aquic")
}

#' Soil moisture regime categories including qualifiers
#' @rdname STMR
#' @export
SMRq_names <- function() {
  c("Extreme-Aridic", "Typic-Aridic", "Weak-Aridic", #Aridic
    "Dry-Xeric", "Typic-Xeric", # Xeric
    "Typic-Tempustic", "Xeric-Tempustic", "Wet-Tempustic", "Aridic-Tropustic",
    "Typic-Tropustic", "Udic-Ustic", # Ustic
    "Typic-Udic", "Dry-Tropudic", "Dry-Tempudic") # Udic
}

#' Soil temperature regime: based on Soil Survey Staff 2014 (Key to Soil
#' Taxonomy): p.31 we ignore distinction between iso- and not iso-
STR_logic <- function(MAST, MSST, SatSoilSummer_days, has_permafrost,
  has_Ohorizon) {

  temp <- STR_names()
  Tregime <- rep(0L, length(temp))
  names(Tregime) <- temp

  if (anyNA(MAST) || anyNA(has_permafrost)) {
    Tregime[] <- NA
    return(Tregime)
  }

  if (MAST >= 22) {
      Tregime["Hyperthermic"] <- 1L
  } else if (MAST >= 15) {
      Tregime["Thermic"] <- 1L
  } else if (MAST >= 8) {
      Tregime["Mesic"] <- 1L

  } else if (MAST > 0 && !has_permafrost) {
    if (any(anyNA(SatSoilSummer_days), anyNA(has_Ohorizon), anyNA(MSST))) {
      Tregime[c("Cryic", "Frigid")] <- NA
      return(Tregime)
    }

    # mineral soils
    if (SatSoilSummer_days > 0) {
      # "soil is saturated with water during some part of summer"
      if (has_Ohorizon) {
        # TODO: should be: 'O-horizon' OR 'histic epipedon'
        if (MSST < 6) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      } else {
        if (MSST < 13) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      }

    } else {
      # "not saturated with water during some part of the summer"
      if (has_Ohorizon) {
        if (MSST < 8) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      } else {
        if (MSST < 15) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      }
    }
    # TODO: else organic soils: cryic if mean(T50jja) > 0 C and < 6 C

  } else if (MAST <= 0 || has_permafrost) {
    # limit should be 1 C for Gelisols
    Tregime["Gelic"] <- 1L
  }

  Tregime
}


#' Soil moisture regime: Soil Survey Staff 2014 (Key to Soil Taxonomy): p.28-31
SMR_logic <- function(ACS_COND1, ACS_COND2, ACS_COND3, MCS_COND0,
  MCS_COND1, MCS_COND2, MCS_COND2_1, MCS_COND2_2, MCS_COND2_3, MCS_COND3,
  MCS_COND3_1, MCS_COND4, MCS_COND5, MCS_COND6, MCS_COND6_1, MCS_COND7,
  MCS_COND8, MCS_COND9, MCS_COND10, has_permafrost) {

  temp <- c(SMR_names(), SMRq_names())
  Sregime <- rep(0L, length(temp))
  names(Sregime) <- temp

  # Anhydrous condition: Soil Survey Staff 2010: p.16
  # == Soil Survey Staff 2014: p.18
  # we ignore test for 'ice-cemented permafrost' and 'rupture-resistance class'
  if (any(anyNA(ACS_COND1), anyNA(ACS_COND2), anyNA(ACS_COND3))) {
    Sregime["Anhydrous"] <- NA

  } else if (ACS_COND1 && ACS_COND2 && ACS_COND3) {
    Sregime["Anhydrous"] <- 1L
  }

  # We ignore 'Aquic' because we have no information on soil oxygen content
  if (any(anyNA(MCS_COND0), anyNA(MCS_COND1), anyNA(MCS_COND2),
    anyNA(MCS_COND2_1), anyNA(MCS_COND2_2), anyNA(MCS_COND2_3),
    anyNA(MCS_COND3), anyNA(MCS_COND3_1), anyNA(MCS_COND4), anyNA(MCS_COND5),
    anyNA(MCS_COND6), anyNA(MCS_COND6_1), anyNA(MCS_COND7), anyNA(MCS_COND8),
    anyNA(MCS_COND9), anyNA(MCS_COND10), anyNA(has_permafrost))) {

    Sregime[-which("Anhydrous" == names(Sregime))] <- NA
    return(Sregime)
  }

  if (MCS_COND0) {
    # Perudic soil moisture regime
    Sregime["Perudic"] <- 1L

  } else if (MCS_COND1 && MCS_COND2) {
    # Aridic soil moisture regime; The limits set for soil temperature
    # exclude from these soil moisture regimes soils in the very cold and dry
    # polar regions and in areas at high elevations. Such soils are considered
    # to have anhydrous condition
    Sregime["Aridic"] <- 1L

    # Qualifier for aridic SMR
    if (MCS_COND10) {
      Sregime["Extreme-Aridic"] <- 1L
    } else if (MCS_COND2_3) {
      # NOTE: COND2_3: assumes that 'MaxContDaysAnyMoistCumAbove8' is
      # equivalent to jNSM variable 'ncpm[2]'
      Sregime["Typic-Aridic"] <- 1L
    } else {
      Sregime["Weak-Aridic"] <- 1L
    }

  } else if (!MCS_COND6 && MCS_COND9 && !MCS_COND4 && MCS_COND5) {
    # Xeric soil moisture regime
    Sregime["Xeric"] <- 1L

    #Qualifier for xeric SMR
    if (MCS_COND6_1) {
      # NOTE: this conditional assumes that 'DryDaysConsecSummer' is equivalent
      # to jNSM variable 'nccd'
      Sregime["Dry-Xeric"] <- 1L
    } else {
      Sregime["Typic-Xeric"] <- 1L
    }

  } else if (MCS_COND3 && (!MCS_COND4 && MCS_COND5 && MCS_COND6 ||
      (MCS_COND4 || !MCS_COND5))) {

    # Udic soil moisture regime - #we ignore test for 'three- phase system'
    # during T50 > 5
    Sregime["Udic"] <- 1L

    # Qualifier for udic SMR
    if (MCS_COND3_1) {
      Sregime["Typic-Udic"] <- 1L
    } else if (!MCS_COND5) {
      Sregime["Dry-Tropudic"] <- 1L
    } else {
      Sregime["Dry-Tempudic"] <- 1L
    }

  } else if (!has_permafrost && !MCS_COND3 &&
      ((MCS_COND4 || !MCS_COND5) && (MCS_COND7 || MCS_COND8) ||
      !MCS_COND4 && MCS_COND5 && !MCS_COND1 &&
      (MCS_COND9 && MCS_COND6 || !MCS_COND9))) {

    # Ustic soil moisture regime
    Sregime["Ustic"] <- 1L

    # Qualifier for ustic SMR
    if (MCS_COND5) {
      if (!MCS_COND9) {
        # NOTE: this conditional assumes that 'MoistDaysConsecWinter' is
        # equivalent to jNSM variable 'nccm'
        Sregime["Typic-Tempustic"] <- 1L
      } else if (!MCS_COND6) {
        # NOTE: this conditional assumes that 'DryDaysConsecSummer' is
        # equivalent to jNSM variable 'nccd'
        Sregime["Xeric-Tempustic"] <- 1L
      } else {
        Sregime["Wet-Tempustic"] <- 1L
      }
    } else {
      # NOTE: COND2_1 and COND2_2: assume that 'MaxContDaysAnyMoistCumAbove8'
      # is equivalent to jNSM variable 'ncpm[2]'
      if (MCS_COND2_1) {
        Sregime["Aridic-Tropustic"] <- 1L
      } else if (MCS_COND2_2) {
        Sregime["Typic-Tropustic"] <- 1L
      } else {
        Sregime["Udic-Ustic"] <- 1L
      }
    }
  }

  Sregime
}



#' Calculate soil moisture and soil temperature regimes and underlying
#' conditions
#'
#' Calculations are based on SSS (2014, 2015) and explained in detail in the
#' \code{vignette(topic = "SoilMoistureRegimes_SoilTemperatureRegimes",
#' package = "rSOILWAT2")}.
#'
#' @param sim_in An object of class \code{\linkS4class{swInputData}}. The
#'   \pkg{rSOILWAT2} simulation input.
#' @param sim_out An object of class \code{\linkS4class{swOutput}}. The
#'   \pkg{rSOILWAT2} simulation output. If \code{NULL} then \code{sim_agg}
#'   must be provided instead.
#' @param sim_agg A named list. The prepared \pkg{rSOILWAT2} simulation output.
#'   If \code{NULL} then \code{sim_out} must be provided so that the elements
#'   of \code{sim_agg} can be determined internally. The list contains:
#'   \var{"soiltemp.dy.all"}, \var{"soiltemp.yr.all"}, \var{"soiltemp.mo.all"},
#'   \var{"vwcmatric.dy.all"}, \var{"swpmatric.dy.all"}, \var{"prcp.yr"},
#'   \var{"prcp.mo"}, \var{"pet.mo"}, and \var{"temp.mo"}. Note:
#'   if \code{sim_agg} has already been calculated for other reasons, then
#'   passing \code{sim_agg} may be faster than passing \code{sim_out},
#'   which (re-)calculates \code{sim_agg}.
#' @param soil_TOC A numeric vector. Total soil organic matter in g C / kg soil
#'   for each soil layer. If \code{NULL}, then internally set to 0.
#' @param has_soil_temperature A logical value. Set to \code{TRUE}, if soil
#'   temperature values have been simulated.
#' @param opt_SMTR A named list. Parameters for the calculation of the regimes:
#'   \itemize{
#'     \item \code{aggregate_at}: A character string. Determines the approach
#'       for regime determination; we recommend the value of "conditions".
#'       See details.
#'     \item \code{crit_agree_frac}: A numeric value. The aggregation agreement
#'       level (e.g., 0.5 = majority; 1 = all); we recommend a value of 0.9.
#'     \item \code{use_normal}: A logical value. If \code{TRUE}, then
#'       "normal years" as defined by (Soil Survey Staff 2014: p.29) are
#'       determined (note: should be at least a time period of 30 years);
#'       if \code{FALSE}, then all years are used for the calculation of
#'       regimes. We recommend to determine "normal years".
#'     \item \code{SWP_dry}: A numeric value. Soil water potential (MPa)
#'       below which soils are considered dry; we recommend a value of -1.5 MPa.
#'     \item \code{SWP_sat}: A numeric value. Soil water potential (MPa)
#'       above which soils are considered saturated; we recommend a value
#'       of -0.033 MPa.
#'     \item \code{impermeability}: A numeric value. The value above which
#'       the code considers a soil layer to be impermeable. We recommend a
#'       value of 0.9.
#'   }
#' @param simTime1 A list with named elements. Calculated internally
#'   if \code{NULL}; alternatively, it can be generated by a call to the
#'   function \code{\link{setup_time_simulation_run}}.
#' @param simTime2 A list with named elements. Calculated internally
#'   if \code{NULL}; alternatively, it can be generated by a call to the
#'   function \code{\link{simTiming_ForEachUsedTimeUnit}}.
#' @param verbose A logical value. If \code{TRUE} more messages are printed.
#' @param msg_tag A character string. Tag that is pre-appended to each verbose
#'   message.
#'
#' @section Details: The argument \code{aggregate_at} determines at which level
#'   aggregations to regimes are carried out: \itemize{
#'   \item \code{data}: Determine conditions and regimes based on aggregated
#'     mean soil moisture/temperature values.
#'   \item \code{conditions} Determine conditions based on time-series values
#'     of soil moisture/temperature values; determine regimes based on
#'     aggregated mean conditions.
#'   \item \code{regime}: Determine conditions and regimes based on
#'     time-series values of soil moisture/temperature values.
#' }
#'
#' @return A list with the following elements: \itemize{
#'   \item \code{regimes_done}: if successful calculation of soil moisture and
#'     soil temperature regimes then \code{TRUE} otherwise \code{FALSE}
#'   \item \code{has_simulated_SoilTemp}: if soil temperature was simulated
#'     then \code{1} otherwise \code{0}
#'   \item \code{has_realistic_SoilTemp}: if simulated soil temperature values
#'     are realistic then \code{1} otherwise \code{0}
#'   \item \code{has_Ohorizon}: if soil profile may have an O-horizon, then
#'     \code{TRUE} otherwise \code{FALSE}
#'   \item \code{Lanh_depth}: a numeric vector of length two
#'   \item \code{MCS_depth}: a numeric vector of length two
#'   \item \code{Fifty_depth}: a numeric value
#'   \item \code{permafrost_yrs}: number of years with permafrost conditions
#'   \item \code{SMR_normalyears}: years considered "normal"
#'   \item \code{SMR_normalyears_N}: number of years considered "normal"
#'   \item \code{cond_annual}: a numeric matrix with annual values of
#'     underlying conditions used to determine soil moisture and soil
#'     temperature regimes
#'   \item \code{STR}: soil temperature regimes
#'   \item \code{SMR}: soil moisture regimes
#' }
#'
#' @references Soil Survey Staff (2014). Keys to soil taxonomy,
#'   12th ed. USDA Natural Resources Conservation Service, Washington, DC.
#' @references Soil Survey Staff (2015). Illustrated guide to soil
#' taxonomy. USDA Natural Resources Conservation Service, National Soil Survey
#' Center, Lincoln, Nebraska.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' sw_out <- sw_exec(inputData = sw_in)
#' SMTR <- calc_SMTRs(sim_in = sw_in, sim_out = sw_out)
#'
#' @export
calc_SMTRs <- function(
  sim_in, sim_out = NULL, sim_agg = NULL, soil_TOC = NULL,
  has_soil_temperature = TRUE,
  opt_SMTR = list(
    aggregate_at = "conditions",
    crit_agree_frac = 0.9,
    use_normal = TRUE,
    SWP_dry = -1.5,
    SWP_sat = -0.033,
    impermeability = 0.9),
  simTime1 = NULL, simTime2 = NULL, verbose = FALSE, msg_tag = NULL) {


  #--- Check arguments
  opt_SMTR[["aggregate_at"]] <- match.arg(opt_SMTR[["aggregate_at"]],
    c("conditions", "data", "regime"))

  has_soil_temperature <- has_soil_temperature &&
    swSite_SoilTemperatureFlag(sim_in)

  soildat <- swSoils_Layers(sim_in)
  req_soilvars <- c("depth_cm", "sand_frac", "clay_frac", "impermeability_frac")
  stopifnot(req_soilvars %in% colnames(soildat))
  if (is.null(soil_TOC)) {
    soil_TOC <- rep(0, NROW(soildat))
  }
  soildat <- cbind(soildat[, req_soilvars, drop = FALSE], soil_TOC = soil_TOC)
  if (verbose) {
    layers_depth_old <- soildat[, "depth_cm"]
  }

  if (is.null(sim_agg)) {
    # we need simulation output object instead
    sim_agg <- new.env(parent = baseenv())
    if (is.null(sim_out)) {
      stop("We need simulation output either `sim_out` or already ",
        "aggregated `sim_agg`.")
    }
  }

  # Get time sequence information
  years <- seq(swYears_StartYear(sim_in), swYears_EndYear(sim_in))

  st1_elem_names <- c("useyrs", "no.useyr", "no.usemo", "index.useyr",
    "index.usemo", "index.usedy")

  is_simTime1_good <- !is.null(simTime1) &&
    all(!sapply(st1_elem_names, function(en) is.null(simTime1[[en]])))

  if (is_simTime1_good) {
    st1 <- simTime1
  } else {
    st1 <- setup_time_simulation_run(sim_time =
        list(spinup_N = 0, startyr = years[1], endyr = years[length(years)]))
  }

  st2_elem_names <- c("month_ForEachUsedDay", "doy_ForEachUsedDay",
    "doy_ForEachUsedDay_NSadj", "month_ForEachUsedDay_NSadj",
    "year_ForEachUsedDay_NSadj", "month_ForEachUsedMonth",
    "month_ForEachUsedMonth_NSadj", "yearno_ForEachUsedMonth",
    "yearno_ForEachUsedMonth_NSadj")

  is_simTime2_good <- !is.null(simTime2) &&
    all(!sapply(st2_elem_names, function(en) is.null(simTime2[[en]])))

  if (is_simTime2_good) {
    st2 <- simTime2
  } else {
    st2 <- simTiming_ForEachUsedTimeUnit(useyrs = years,
      sim_tscales = c("daily", "monthly", "yearly"),
      latitude = swSite_IntrinsicSiteParams(sim_in)[["Latitude"]],
      account_NorthSouth = TRUE)
  }


  #--- Prepare result containers
  SMTR <- list()
  SMTR[["regimes_done"]] <- FALSE
  SMTR[["has_simulated_SoilTemp"]] <- NA
  SMTR[["has_realistic_SoilTemp"]] <- NA
  SMTR[["has_Ohorizon"]] <- NA

  SMTR[["MCS_depth"]] <- SMTR[["Lanh_depth"]] <- rep(NA, 2)
  SMTR[["Fifty_depth"]] <- NA
  SMTR[["permafrost_yrs"]] <- NA
  SMTR[["SMR_normalyears"]] <- NA
  SMTR[["SMR_normalyears_N"]] <- 0

  icols0 <- c("MATLanh", "MAT50", "T50jja", "T50djf",
    "CSPartSummer", "meanTair_Tsoil50_offset_C")
  icols1a <- c("ACS_COND1", "ACS_COND2", "ACS_COND3")
  icols1 <- c(icols1a, "ACS_HalfDryDaysCumAbove0C", "ACS_SoilAbove0C")
  icols2 <- c("T50_at0C", "Lanh_Dry_Half", "ACS_COND3_Test")
  icols3 <- c("COND0",
    "DryDaysCumAbove5C", "SoilAbove5C", "COND1",
    "MaxContDaysAnyMoistCumAbove8", "COND2", "COND2_1", "COND2_2",
    "COND2_3",
    "DryDaysCumAny", "COND3", "COND3_1",
    "COND4",
    "AbsDiffSoilTemp_DJFvsJJA", "COND5",
    "DryDaysConsecSummer", "COND6", "COND6_1",
    "MoistDaysCumAny", "COND7",
    "MoistDaysConsecAny", "COND8",
    "MoistDaysConsecWinter", "COND9",
    "AllDryDaysCumAny", "COND10")
  icols4 <- c("T50_at5C", "T50_at8C", "MCS_Moist_All", "COND1_Test",
    "COND2_Test")
  temp <- c(icols0, icols1, icols2, icols3, icols4)
  stopifnot(length(temp) == 45)

  SMTR[["cond_annual"]] <- matrix(NA, nrow = st1[["no.useyr"]],
    ncol = length(temp), dimnames = list(NULL, temp))

  temp <- STR_names()
  SMTR[["STR"]] <- matrix(0, nrow = 1, ncol = length(temp),
    dimnames = list(NULL, temp))
  temp <- c(SMR_names(), SMRq_names())
  SMTR[["SMR"]] <- matrix(0, nrow = 1, ncol = length(temp),
    dimnames = list(NULL, temp))


  # Check that soil temperature are realistic: use 100 C as upper
  # limit from Garratt, J.R. (1992). Extreme maximum land surface
  # temperatures. Journal of Applied Meteorology, 31, 1096-1105.
  if (has_soil_temperature) {
    SMTR[["has_simulated_SoilTemp"]] <- 1

    if (!exists("soiltemp.dy.all", where = sim_agg)) {
      sim_agg[["soiltemp.dy.all"]] <- list(val =
          slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_soiltemp"]), "Day"))
    }

    ihead <- 1:2

    if (!anyNA(sim_agg[["soiltemp.dy.all"]][["val"]]) &&
        all(sim_agg[["soiltemp.dy.all"]][["val"]][, -ihead] < 100)) {

      SMTR[["has_realistic_SoilTemp"]] <- 1

      if (!exists("soiltemp.yr.all", where = sim_agg)) {
        sim_agg[["soiltemp.yr.all"]] <- list(val =
          slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_soiltemp"]), "Year"))
      }
      if (!exists("soiltemp.mo.all", where = sim_agg)) {
        sim_agg[["soiltemp.mo.all"]] <- list(val =
          slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_soiltemp"]), "Month"))
      }
      if (!exists("vwcmatric.dy.all", where = sim_agg)) {
        sim_agg[["vwcmatric.dy.all"]] <- list(val =
          slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_vwcmatric"]), "Day"))
      }
      if (!exists("swpmatric.dy.all", where = sim_agg)) {
        sim_agg[["swpmatric.dy.all"]] <- list(val = cbind(
          sim_agg[["vwcmatric.dy.all"]][["val"]][, ihead],
          VWCtoSWP(sim_agg[["vwcmatric.dy.all"]][["val"]][, -ihead],
            sand = soildat[, "sand_frac"], clay = soildat[, "clay_frac"])))
      }

      if (!exists("prcp.yr", where = sim_agg)) {
        temp <- 10 * slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_precip"]),
          "Year")
        sim_agg[["prcp.yr"]] <- list(ppt =
          temp[st1[["index.useyr"]], 2, drop = FALSE])
      }
      if (!exists("prcp.mo", where = sim_agg)) {
        temp <- 10 * slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_precip"]),
          "Month")
        sim_agg[["prcp.mo"]] <- list(ppt =
            temp[st1[["index.usemo"]], 3, drop = FALSE])
      }
      if (!exists("pet.mo", where = sim_agg)) {
        temp <- 10 * slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_pet"]),
          "Month")
        sim_agg[["pet.mo"]] <- list(val = temp[st1[["index.usemo"]], 3])
      }
      if (!exists("temp.mo", where = sim_agg)) {
        temp <- 10 * slot(slot(sim_out, rSW2_glovars[["swof"]]["sw_temp"]),
          "Month")
        sim_agg[["temp.mo"]] <- list(mean = temp[st1[["index.usemo"]], 5])
      }

      # Prepare data
      # Water year:
      #  for each day of the simulation, i.e., October 1st is day 1 of the
      #  water year in the northern hemisphere and April 1st is day 1 of the
      #  water year in the southern hemisphere.
      wateryear_ForEachUsedDay_NSadj <- st2[["year_ForEachUsedDay_NSadj"]] +
        ifelse(st2[["doy_ForEachUsedDay_NSadj"]] > 273, 1, 0)

      # eliminate last (potentially) incomplete) year
      wyears <- (temp <- unique(wateryear_ForEachUsedDay_NSadj))[-length(temp)]

      if (opt_SMTR[["use_normal"]]) {
        #--- Normal years for soil moisture regimes
        # (Soil Survey Staff 2014: p.29)
        # Should have a time period of 30 years to determine normal years
        #   - Annual precipitation that is plus or minus one standard
        #     precipitation
        #   - and Mean monthly precipitation that is plus or minus one
        #     standard deviation of the long-term monthly precipitation for
        #     8 of the 12 months
        if (st1[["no.useyr"]] < 30) {
          print(paste0(msg_tag, ": has only", st1[["no.useyr"]], "years ",
            "of data; determination of normal years for NRCS soil moisture ",
            "regimes should be based on >= 30 years."))
        }

        MAP <- c(mean(sim_agg[["prcp.yr"]][["ppt"]]),
          sd(sim_agg[["prcp.yr"]][["ppt"]]))
        normal1 <- as.vector(
          (sim_agg[["prcp.yr"]][["ppt"]] >= MAP[1] - MAP[2]) &
          (sim_agg[["prcp.yr"]][["ppt"]] <= MAP[1] + MAP[2]))
        MMP <- tapply(sim_agg[["prcp.mo"]][["ppt"]],
          st2[["month_ForEachUsedMonth_NSadj"]],
          function(x) c(mean(x), sd(x)))
        MMP <- matrix(unlist(MMP), nrow = 2, ncol = 12)
        normal2 <- tapply(sim_agg[["prcp.mo"]][["ppt"]],
          st2[["yearno_ForEachUsedMonth_NSadj"]],
          function(x) sum((x >= MMP[1, ] - MMP[2, ]) &
              (x <= MMP[1, ] + MMP[2, ])) >= 8)

        st_NRCS <- list(
          yr_used = yr_used <- wyears[normal1 & normal2],
          i_yr_used = findInterval(yr_used, wyears))

      } else {
        st_NRCS <- list(
          yr_used = st1[["useyrs"]],
          i_yr_used = findInterval(st1[["useyrs"]], wyears))
      }

      i_dy_used <- wateryear_ForEachUsedDay_NSadj %in% st_NRCS[["yr_used"]]
      temp <- seq_len(st1[["no.usemo"]])
      i_mo_used <- temp[rep(wyears, each = 12) %in% st_NRCS[["yr_used"]]]
      dtemp <- table(wateryear_ForEachUsedDay_NSadj[i_dy_used], dnn = NULL)

      st_NRCS <- c(st_NRCS, list(
        N_yr_used = length(st_NRCS[["yr_used"]]),
        i_dy_used = i_dy_used,
        N_dy_used = sum(i_dy_used),
        i_mo_used = i_mo_used,
        days_per_yr_used = as.integer(dtemp)))

      SMTR[["SMR_normalyears"]] <- st_NRCS[["yr_used"]]
      SMTR[["SMR_normalyears_N"]] <- st_NRCS[["N_yr_used"]]

      soiltemp_nrsc <- list(
        yr = list(data = {
            temp <- st1[["index.useyr"]][st_NRCS[["i_yr_used"]]]
            sim_agg[["soiltemp.yr.all"]][["val"]][temp, , drop = FALSE]},
          nheader = 1),
        mo = list(data = {
            temp <- st1[["index.usemo"]][st_NRCS[["i_mo_used"]]]
            sim_agg[["soiltemp.mo.all"]][["val"]][temp, , drop = FALSE]},
          nheader = 2),
        dy = list(data = {
            temp <- st1[["index.usedy"]][st_NRCS[["i_dy_used"]]]
            sim_agg[["soiltemp.dy.all"]][["val"]][temp, , drop = FALSE]},
          nheader = 2)
      )
      vwc_dy_nrsc <- sim_agg[["vwcmatric.dy.all"]]

      if (opt_SMTR[["aggregate_at"]] == "data") {
        # Aggregate SOILWAT2 output to mean conditions before determining
        # conditions and regimes
        soiltemp_nrsc <- list(
          yr = list(data =
              matrix(colMeans(soiltemp_nrsc[["yr"]][["data"]]), nrow = 1),
            nheader = soiltemp_nrsc[["yr"]][["nheader"]]),
          mo = list(data = {
              temp <- st2[["month_ForEachUsedMonth"]][st_NRCS[["i_mo_used"]]]
              aggregate(soiltemp_nrsc[["mo"]][["data"]],
                by = list(temp), mean)[, -1]},
            nheader = soiltemp_nrsc[["mo"]][["nheader"]]),
          dy = list(data = {
              temp <- st2[["doy_ForEachUsedDay"]][st_NRCS[["i_dy_used"]]]
              aggregate(soiltemp_nrsc[["dy"]][["data"]],
                by = list(temp), mean)[, -1]},
            nheader = soiltemp_nrsc[["dy"]][["nheader"]])
        )
        vwc_dy_nrsc <- lapply(sim_agg[["vwcmatric.dy.all"]],
          function(x) {
            temp1 <- st1[["index.usedy"]][st_NRCS[["i_dy_used"]]]
            temp2 <- st2[["doy_ForEachUsedDay"]][st_NRCS[["i_dy_used"]]]
            aggregate(as.matrix(x)[temp1, ], list(temp2), mean)[, -1]
          })

        temp <- dim(vwc_dy_nrsc[["val"]])[1]
        st_NRCS <- c(st_NRCS, list(
          index_usedy = seq_len(temp),
          month_ForMonth = rSW2_glovars[["st_mo"]],
          yearno_ForMonth = rep(1, 12),
          doy_ForDay = seq_len(temp)
        ))
        # adjust st_NRCS for the aggregation
        st_NRCS <- utils::modifyList(st_NRCS, list(
          yr_used = 1,
          N_yr_used = 1,
          i_yr_used = 1,
          i_mo_used = rSW2_glovars[["st_mo"]],
          i_dy_used = rep(TRUE, temp),
          N_dy_used = temp,
          days_per_yr_used = temp))

        wateryear_ForEachUsedDay_NSadj <- rep(1, temp)
        wyears <- 1

      } else {
        # Determine regimes based on time-series output and then determine
        # conditions and regime
        st_NRCS <- c(st_NRCS, list(
          index_usedy = st1[["index.usedy"]][st_NRCS[["i_dy_used"]]],
          month_ForMonth =
            st2[["month_ForEachUsedMonth_NSadj"]][st_NRCS[["i_mo_used"]]],
          yearno_ForMonth =
            st2[["yearno_ForEachUsedMonth_NSadj"]][st_NRCS[["i_mo_used"]]],
          doy_ForDay = st2[["doy_ForEachUsedDay_NSadj"]][st_NRCS[["i_dy_used"]]]
        ))
      }

      #--- Required soil layers
      # 50 cm soil depth or impermeable layer (whichever is shallower;
      # Soil Survey Staff 2014: p.31)
      imp_depth <- max(soildat[, "depth_cm"])
      temp <- which(soildat[, "impermeability_frac"] >=
          opt_SMTR[["impermeability"]])
      # Interpret maximum soil depth as possible impermeable layer
      if (length(temp) > 0) {
        imp_depth <- min(min(soildat[temp, "depth_cm"]), imp_depth)
      }
      SMTR[["Fifty_depth"]] <- min(50, imp_depth)

      # Definition of MCS (Soil Survey Staff 2014: p.29):
      # "The moisture control section (MCS) of a soil: the depth to which a
      # dry (tension of more than 1500 kPa, but not air-dry) soil will be
      # moistened by 2.5 cm of water within 24 hours. The lower boundary is
      # the depth to which a dry soil will be moistened by 7.5 cm of water
      # within 48 hours."
      layers_width <- getLayersWidth(soildat[, "depth_cm"])
      sand_temp <- weighted.mean(soildat[, "sand_frac"], layers_width)
      clay_temp <- weighted.mean(soildat[, "clay_frac"], layers_width)
      # Practical depth definition of MCS
      #  - 10 to 30 cm below the soil surface if the particle-size class of
      #    the soil is fine-loamy, coarse-silty, fine-silty, or clayey
      #  - 20 to 60 cm if the particle-size class is coarse-loamy
      #  - 30 to 90 cm if the particle-size class is sandy.
      SMTR[["MCS_depth"]] <- if (clay_temp >= 0.18) {
          c(10, 30)
        } else if (sand_temp < 0.15) {
          c(10, 30)
        } else if (sand_temp >= 0.50) {
          c(30, 90)
        } else c(20, 60)
      # "If 7.5 cm of water moistens the soil to a densic, lithic, paralithic,
      # or petroferric contact or to a petrocalcic or petrogypsic horizon or a
      # duripan, the contact or the upper boundary of the cemented horizon
      # constitutes the lower boundary of the soil moisture control section.
      # If a soil is moistened to one of these contacts or horizons by 2.5 cm
      # of water, the soil moisture control section is the boundary of the
      # contact itself. The control section of such a soil is considered moist
      # if the contact or upper boundary of the cemented horizon has a thin
      # film of water. If that upper boundary is dry, the control section is
      # considered dry."

      SMTR[["MCS_depth"]] <- adjustLayer_byImp(depths = SMTR[["MCS_depth"]],
        imp_depth = imp_depth, sdepths = soildat[, "depth_cm"])

      # Soil layer 10-70 cm used for anhydrous layer definition;
      # adjusted for impermeable layer
      SMTR[["Lanh_depth"]] <- adjustLayer_byImp(depths = c(10, 70),
        imp_depth = imp_depth, sdepths = soildat[, "depth_cm"])

      # Permafrost (Soil Survey Staff 2014: p.28) is defined as a "thermal
      # condition in which a material (including soil material) remains
      # below 0 C for 2 or more years in succession"
      temp <- sim_agg[["soiltemp.yr.all"]][["val"]][
        st1[["index.useyr"]], -1, drop = FALSE]
      SMTR[["permafrost_yrs"]] <- max(apply(temp, 2, function(x) {
        temp <- rle(x < 0)
        if (any(temp$values)) max(temp$lengths[temp$values]) else 0L
      }))

      has_notenough_normalyears <- FALSE
      if (SMTR[["SMR_normalyears_N"]] > 0) {
        SMTR[["cond_annual"]] <- SMTR[["cond_annual"]][
          st_NRCS[["i_yr_used"]], , drop = FALSE]

        # Set soil depths and intervals accounting for shallow soil profiles:
        # Soil Survey Staff 2014: p.31)
        ##Calculate soil temperature at necessary depths using a weighted mean
        i_depth50 <- findInterval(SMTR[["Fifty_depth"]], soildat[, "depth_cm"])
        calc50 <- !(SMTR[["Fifty_depth"]] == soildat[i_depth50, "depth_cm"])
        if (calc50) {
          weights50 <- calc_weights_from_depths(i_depth50,
            target_cm = SMTR[["Fifty_depth"]], soildat[, "depth_cm"])
          soildat <- t(add_layer_to_soil(t(soildat), i_depth50, weights50))
          i_depth50 <- findInterval(SMTR[["Fifty_depth"]],
            soildat[, "depth_cm"])

          soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) {
            temp <- add_layer_to_soil(st[["data"]],
              il = st[["nheader"]] + i_depth50, w = weights50)
            list(data = temp, nheader = st[["nheader"]])
          })

          vwc_dy_nrsc[["val"]] <- add_layer_to_soil(vwc_dy_nrsc[["val"]],
            il = 2 + i_depth50, w = weights50)
          rm(weights50)
        }

        i_MCS <- findInterval(SMTR[["MCS_depth"]], soildat[, "depth_cm"])
        calcMCS <- !(SMTR[["MCS_depth"]] == soildat[i_MCS, "depth_cm"])
        if (any(calcMCS)) for (k in which(calcMCS)) {
          weightsMCS <- calc_weights_from_depths(i_MCS[k],
            target_cm = SMTR[["MCS_depth"]][k], soildat[, "depth_cm"])
          soildat <- t(add_layer_to_soil(t(soildat), i_MCS[k], weightsMCS))
          i_MCS <- findInterval(SMTR[["MCS_depth"]], soildat[, "depth_cm"])

          soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) {
            temp <- add_layer_to_soil(st[["data"]],
              il = st[["nheader"]] + i_MCS[k], w = weightsMCS)
            list(data = temp, nheader = st[["nheader"]])
          })

          vwc_dy_nrsc[["val"]] <- add_layer_to_soil(vwc_dy_nrsc[["val"]],
            il = 2 + i_MCS[k], w = weightsMCS)
          rm(weightsMCS)
        }

        i_Lanh <- findInterval(SMTR[["Lanh_depth"]], soildat[, "depth_cm"])
        calcLanh <- !(SMTR[["Lanh_depth"]] == soildat[i_Lanh, "depth_cm"])
        if (any(calcLanh)) for (k in which(calcLanh)) {
          weightsLanh <- calc_weights_from_depths(i_Lanh[k],
            target_cm = SMTR[["Lanh_depth"]][k], soildat[, "depth_cm"])
          soildat <- t(add_layer_to_soil(t(soildat), i_Lanh[k], weightsLanh))
          i_Lanh <- findInterval(SMTR[["Lanh_depth"]], soildat[, "depth_cm"])

          soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) {
            temp <- add_layer_to_soil(st[["data"]],
              il = st[["nheader"]] + i_Lanh[k], w = weightsLanh)
            list(data = temp, nheader = st[["nheader"]])
          })

          vwc_dy_nrsc[["val"]] <- add_layer_to_soil(vwc_dy_nrsc[["val"]],
            il = 2 + i_Lanh[k], w = weightsLanh)
          rm(weightsLanh)
        }

        soilLayers_N_NRCS <- dim(soildat)[1]
        soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) st[["data"]])

        swp_recalculate <- calc50 || any(calcMCS) || any(calcLanh)
        if (swp_recalculate && verbose) {
            print(paste0(msg_tag, ": interpolated soil layers for NRCS soil ",
              "regimes because of insufficient soil layers: ",
              "required would be {",
                paste(sort(unique(c(SMTR[["Fifty_depth"]], SMTR[["MCS_depth"]],
                  SMTR[["Lanh_depth"]]))), collapse = ", "),
              "} and available are {",
                paste(layers_depth_old, collapse = ", "), "}"))
        }

        swp_dy_nrsc <- if (swp_recalculate ||
          opt_SMTR[["aggregate_at"]] == "data") {
            temp <- VWCtoSWP(vwc_dy_nrsc[["val"]][, -ihead, drop = FALSE],
              sand = soildat[, "sand_frac"], clay = soildat[, "clay_frac"])
            temp[st_NRCS[["index_usedy"]], , drop = FALSE]
          } else {
            sim_agg[["swpmatric.dy.all"]][["val"]][
              st_NRCS[["index_usedy"]], -ihead, drop = FALSE]
          }

        #MCS (Soil Survey Staff 2014: p.29)
        #What soil layer info used for MCS
        i_MCS <- identify_soillayers(SMTR[["MCS_depth"]],
          sdepth = soildat[, "depth_cm"])
        #Repeat for Anhydrous soil layer moisture delineation
        i_Lanh <- identify_soillayers(SMTR[["Lanh_depth"]],
          sdepth = soildat[, "depth_cm"])

        #mean soil temperature in Lahn depths (10 - 70 cm)
        SMTR[["cond_annual"]][, "MATLanh"] <- apply(
          soiltemp_nrsc[["yr"]][, 1 + i_Lanh, drop = FALSE], 1,
          weighted.mean, w = soildat[i_Lanh, "depth_cm"])

        #---Calculate variables
        crit_agree <- opt_SMTR[["crit_agree_frac"]] * st_NRCS[["N_yr_used"]]

        #mean soil temperatures at 50cm depth
        SMTR[["cond_annual"]][, "MAT50"] <-
          soiltemp_nrsc[["yr"]][, 1 + i_depth50]
        temp <- soiltemp_nrsc[["mo"]][, 2 + i_depth50][
          st_NRCS[["month_ForMonth"]] %in% 6:8]
        SMTR[["cond_annual"]][, "T50jja"] <- apply(
          matrix(temp, ncol = st_NRCS[["N_yr_used"]]), 2, mean)
        temp <- soiltemp_nrsc[["mo"]][, 2 + i_depth50][
          st_NRCS[["month_ForMonth"]] %in% c(12, 1:2)]
        SMTR[["cond_annual"]][, "T50djf"] <- apply(
          matrix(temp, ncol = st_NRCS[["N_yr_used"]]), 2, mean)
        T50 <- soiltemp_nrsc[["dy"]][, 2 + i_depth50]
        # offset between soil and air temperature
        fc <- sim_agg[["temp.mo"]][["mean"]][st_NRCS[["i_mo_used"]]] -
          soiltemp_nrsc[["mo"]][, 2 + i_depth50]
        SMTR[["cond_annual"]][, "meanTair_Tsoil50_offset_C"] <- tapply(fc,
          st_NRCS[["yearno_ForMonth"]], mean)

        # CSPartSummer: "Is the soil saturated with water during some part of
        # the summer June1 ( = regular doy 244) - Aug31 ( = regular doy 335)"
        isummer <- st_NRCS[["doy_ForDay"]] >= 244 &
          st_NRCS[["doy_ForDay"]] <= 335
        SMTR[["cond_annual"]][, "CSPartSummer"] <- vapply(st_NRCS[["yr_used"]],
          function(yr) {
            temp <- swp_dy_nrsc[wateryear_ForEachUsedDay_NSadj[
              st_NRCS[["i_dy_used"]]] == yr & isummer, , drop = FALSE]
            temp <- apply(temp, 1, function(x) all(x >= opt_SMTR[["SWP_sat"]]))
            rtemp <- rle(temp)
            if (any(rtemp$values)) max(rtemp$lengths[rtemp$values]) else 0
          }, FUN.VALUE = NA_real_)

        # "saturated with water for X cumulative days in normal years"
        days_saturated_layers <- vapply(st_NRCS[["yr_used"]],
          function(yr) {
            temp <- swp_dy_nrsc[wateryear_ForEachUsedDay_NSadj[
              st_NRCS[["i_dy_used"]]] == yr, , drop = FALSE]
            apply(temp, 2, function(x) sum(x >= opt_SMTR[["SWP_sat"]]))
          }, FUN.VALUE = rep(NA_real_, soilLayers_N_NRCS))

        if (!is.matrix(days_saturated_layers)) {
          days_saturated_layers <- matrix(days_saturated_layers,
            nrow = soilLayers_N_NRCS, ncol = st_NRCS[["N_yr_used"]])
        }

        somCOND0 <- t(days_saturated_layers) >= 30
        #if (opt_SMTR[["aggregate_at"]] == "conditions") {
        temp <- matrix(colSums(somCOND0), nrow = 1, ncol = soilLayers_N_NRCS)
        somCOND0 <- temp >= crit_agree
        #}

        # Organic versus mineral soil material per layer
        # units(TOC) = g C / kg soil
        organic_carbon_wfraction <- soildat[, "soil_TOC"] / 1000

        is_mineral_layer <- (!somCOND0 & organic_carbon_wfraction < 0.2) |
          (somCOND0 &
            (soildat[, "clay_frac"] >= 0.6 & organic_carbon_wfraction < 0.18) |
            (organic_carbon_wfraction < 0.12 + 0.1 * soildat[, "clay_frac"]))

        # determine presence of O horizon
        # TODO: guess (critical levels 'crit_Oh' are made up/not based on data):
        #       O-horizon if 50% trees or 75% shrubs or lots of litter
        crit_Oh <- c(0.5, 0.75, 0.8)
        veg_comp <- swProd_Composition(sim_in)[1:4]

        temp <- cbind(swProd_MonProd_grass(sim_in)[, "Litter"],
          swProd_MonProd_shrub(sim_in)[, "Litter"],
          swProd_MonProd_tree(sim_in)[, "Litter"],
          swProd_MonProd_forb(sim_in)[, "Litter"])

        veg_litter <- mean(apply(sweep(temp, 2, veg_comp, "*"), 1, sum))
        crit_litter <- crit_Oh[3] *
          sum(swProd_Es_param_limit(sim_in) * veg_comp)

        SMTR[["has_Ohorizon"]] <- (veg_litter >= crit_litter) &&
          if (!is.finite(is_mineral_layer[1])) {
            veg_comp["Trees"] > crit_Oh[1] || veg_comp["Shrubs"] > crit_Oh[2]
          } else {
            !is_mineral_layer[1]
          }

        #--- Soil temperature regime: based on Soil Survey Staff 2014
        # (Key to Soil Taxonomy): p.31
        # here, we ignore distinction between iso- and not iso-
        icol <- c("MAT50", "T50jja", "CSPartSummer")
        stCONDs <- SMTR[["cond_annual"]][, icol, drop = FALSE]
        if (opt_SMTR[["aggregate_at"]] == "conditions") {
          temp <- colMeans(stCONDs)
          temp["CSPartSummer"] <-
            temp["CSPartSummer"] > opt_SMTR[["crit_agree_frac"]]
          stCONDs <- matrix(temp, nrow = 1, ncol = length(icol),
            dimnames = list(NULL, icol))
        }
        has_permafrost <- SMTR[["permafrost_yrs"]] >= 2

        SMTR[["STR"]] <- t(apply(stCONDs, 1, function(x)
          STR_logic(MAST = x["MAT50"], MSST = x["T50jja"],
            SatSoilSummer_days = x["CSPartSummer"],
            has_permafrost = has_permafrost,
            has_Ohorizon = SMTR[["has_Ohorizon"]])))


        if (SMTR[["SMR_normalyears_N"]] > 2) {
          # Structures used Lanh delineation
          # Days are moists in half of the Lanh soil depth (not soil layers!)
          n_Lanh <- length(i_Lanh)
          width_Lanh <- diff(c(0, soildat[, "depth_cm"]))[i_Lanh]
          if (FALSE) {
            stopifnot(sum(width_Lanh) ==
              SMTR[["Lanh_depth"]][2] - SMTR[["Lanh_depth"]][1])
          }
          temp <- swp_dy_nrsc[, i_Lanh, drop = FALSE] > opt_SMTR[["SWP_dry"]]
          temp <- temp * matrix(width_Lanh, nrow = st_NRCS[["N_dy_used"]],
            ncol = length(i_Lanh), byrow = TRUE)
          temp <- .rowSums(temp, m = st_NRCS[["N_dy_used"]], n = n_Lanh)
          Lanh_Dry_Half <- temp <= sum(width_Lanh) / 2

          #Conditions for Anhydrous soil delineation
          ACS_CondsDF_day <- data.frame(
            Years = rep(st_NRCS[["yr_used"]], st_NRCS[["days_per_yr_used"]]),
            T50_at0C = T50 > 0, # days where T @ 50 is > 0 C
            Lanh_Dry_Half = Lanh_Dry_Half
          )
          ACS_CondsDF_yrs <- data.frame(
            Years = st_NRCS[["yr_used"]],
            MAT50 = SMTR[["cond_annual"]][, "MAT50"],
            MATLanh = SMTR[["cond_annual"]][, "MATLanh"]
          )

          # Mean Annual soil temperature is less than or equal to 0C
          ACS_CondsDF_yrs$ACS_COND1 <- ACS_CondsDF_yrs$MAT50 <= 0
          # Soil temperature in the Lahn Depth is never greater than 5
          ACS_CondsDF_day$ACS_COND2_Test <- apply(
            soiltemp_nrsc[["dy"]][, 1 + i_Lanh, drop = FALSE], 1,
            function(st) all(st < 5))
          ACS_CondsDF_yrs$ACS_COND2 <- with(ACS_CondsDF_day,
            tapply(ACS_COND2_Test, Years, all))
          # In the Lahn Depth, 1/2 of soil dry > 1/2 CUMULATIVE days when
          # Mean Annual ST > 0C
          ACS_CondsDF_day$ACS_COND3_Test <- with(ACS_CondsDF_day,
            Lanh_Dry_Half == T50_at0C)
          ACS_CondsDF_yrs$ACS_HalfDryDaysCumAbove0C <- with(ACS_CondsDF_day,
            tapply(ACS_COND3_Test, Years, sum))
          ACS_CondsDF_yrs$ACS_SoilAbove0C <- with(ACS_CondsDF_day,
            tapply(T50_at0C, Years, sum))
          # TRUE = Half of soil layers are dry greater than half the days
          #   where MAST > 0 C
          ACS_CondsDF_yrs$ACS_COND3 <- with(ACS_CondsDF_yrs,
            ACS_HalfDryDaysCumAbove0C > .5 * ACS_SoilAbove0C)

          ACS_CondsDF3 <- as.matrix(ACS_CondsDF_yrs[, icols1a, drop = FALSE])
          if (opt_SMTR[["aggregate_at"]] == "conditions") {
            temp <- matrix(colSums(ACS_CondsDF3, na.rm = TRUE), nrow = 1,
              ncol = length(icols1a), dimnames = list(NULL, icols1a))
            ACS_CondsDF3 <- temp >= crit_agree
          } else {
            dimnames(ACS_CondsDF3)[[2]] <- icols1a
          }

          #--- Structures used for MCS delineation
          MCS_CondsDF_day <- data.frame(
            Years = rep(st_NRCS[["yr_used"]], st_NRCS[["days_per_yr_used"]]),
            DOY = st_NRCS[["doy_ForDay"]],
            T50_at5C = T50 > 5, # days where T @ 50cm exceeds 5C
            T50_at8C = T50 > 8, # days where T @ 50cm exceeds 8C
            MCS_Moist_All = {
                temp <- swp_dy_nrsc[, i_MCS, drop = FALSE] >
                  opt_SMTR[["SWP_dry"]]
                apply(temp, 1, all)
              },
            MCS_Dry_All = {
                temp <- swp_dy_nrsc[, i_MCS, drop = FALSE] <
                  opt_SMTR[["SWP_dry"]]
                apply(temp, 1, all)
              }
          )

          MCS_CondsDF_yrs <- data.frame(
            Years = st_NRCS[["yr_used"]],
            MAT50 = SMTR[["cond_annual"]][, "MAT50"],
            T50jja = SMTR[["cond_annual"]][, "T50jja"],
            T50djf = SMTR[["cond_annual"]][, "T50djf"]
          )

          #COND0 - monthly PET < PPT
          temp <- sim_agg[["prcp.mo"]][["ppt"]] - sim_agg[["pet.mo"]][["val"]]
          MCS_CondsDF_yrs$COND0 <- if (opt_SMTR[["aggregate_at"]] == "data") {
              all(tapply(temp, st2[["month_ForEachUsedMonth"]], mean) > 0)
            } else {
              temp <- tapply(temp > 0, st2[["yearno_ForEachUsedMonth"]], all)
              temp[st_NRCS[["i_yr_used"]]]
            }

          # COND1 - Dry in ALL parts for more than half of the CUMULATIVE days
          # per year when the soil temperature at a depth of 50cm is above 5C
          MCS_CondsDF_day$COND1_Test <- with(MCS_CondsDF_day,
            MCS_Dry_All & T50_at5C)  #TRUE = where are both these conditions met
          MCS_CondsDF_yrs$DryDaysCumAbove5C <- with(MCS_CondsDF_day,
            tapply(COND1_Test, Years, sum))
          MCS_CondsDF_yrs$SoilAbove5C <- with(MCS_CondsDF_day,
            tapply(T50_at5C, Years, sum))
          #TRUE =Soils are dry greater than 1/2 cumulative days/year
          MCS_CondsDF_yrs$COND1 <- with(MCS_CondsDF_yrs,
            DryDaysCumAbove5C > .5 * SoilAbove5C)

          # Cond2 - Moist in SOME or all parts for less than 90 CONSECUTIVE
          # days when the the soil temperature at a depth of 50cm is above 8C
          MCS_CondsDF_day$COND2_Test <- with(MCS_CondsDF_day,
            !MCS_Dry_All & T50_at8C)#TRUE = where are both these conditions met
          MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 <- with(MCS_CondsDF_day,
            tapply(COND2_Test, Years, max_duration)) # Maximum consecutive days
          # TRUE = moist less than 90 consecutive days during >8 C soils,
          # FALSE = moist more than 90 consecutive days
          MCS_CondsDF_yrs$COND2 <-
            MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 90
          MCS_CondsDF_yrs$COND2_1 <-
            MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 180
          MCS_CondsDF_yrs$COND2_2 <-
            MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 270
          MCS_CondsDF_yrs$COND2_3 <-
            MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 <= 45

          # COND3 - MCS is Not dry in ANY part as long as 90 CUMULATIVE days -
          # Can't be dry longer than 90 cum days
          # Number of days where any soils are dry:
          MCS_CondsDF_yrs$DryDaysCumAny <- with(MCS_CondsDF_day,
            tapply(!MCS_Moist_All, Years, sum))
          # TRUE = Not Dry for as long 90 cumlative days,
          # FALSE = Dry as long as as 90 Cumlative days
          MCS_CondsDF_yrs$COND3 <- MCS_CondsDF_yrs$DryDaysCumAny < 90
          MCS_CondsDF_yrs$COND3_1 <- MCS_CondsDF_yrs$DryDaysCumAny < 30

          # COND4 - The means annual soil temperature at 50cm is < or > 22C
          # TRUE - Greater than 22, False - Less than 22
          MCS_CondsDF_yrs$COND4 <- MCS_CondsDF_yrs$MAT50 >= 22

          # COND5 - The absolute difference between the temperature in winter
          # @ 50cm and the temperature in summer @ 50cm is > or < 6
          MCS_CondsDF_yrs$AbsDiffSoilTemp_DJFvsJJA <- with(MCS_CondsDF_yrs,
            abs(T50djf - T50jja))
          # TRUE - Greater than 6, FALSE - Less than 6
          MCS_CondsDF_yrs$COND5 <- MCS_CondsDF_yrs$AbsDiffSoilTemp_DJFvsJJA >= 6

          # COND6 - Dry in ALL parts LESS than 45 CONSECUTIVE days in the 4
          # months following the summer solstice
          # Consecutive days of dry soil after summer solsitice
          temp <- with(MCS_CondsDF_day[MCS_CondsDF_day$DOY %in% c(172:293), ],
            tapply(MCS_Dry_All, Years, max_duration))
          ids <- match(MCS_CondsDF_yrs[, "Years"], as.integer(names(temp)),
            nomatch = 0)
          MCS_CondsDF_yrs[ids > 0, "DryDaysConsecSummer"] <- temp[ids]
          # TRUE = dry less than 45 consecutive days
          MCS_CondsDF_yrs$COND6 <- MCS_CondsDF_yrs$DryDaysConsecSummer < 45
          MCS_CondsDF_yrs$COND6_1 <- MCS_CondsDF_yrs$DryDaysConsecSummer > 90

          # COND7 - MCS is MOIST in SOME parts for more than 180 CUMULATIVE days
          # Number of days where any soils are moist:
          MCS_CondsDF_yrs$MoistDaysCumAny <- with(MCS_CondsDF_day,
            tapply(!MCS_Dry_All, Years, sum))
          # TRUE = Not Dry or Moist for as long 180 cumlative days
          MCS_CondsDF_yrs$COND7 <- MCS_CondsDF_yrs$MoistDaysCumAny > 180

          # COND8 - MCS is MOIST in SOME parts for more than 90 CONSECUTIVE days
          # Consecutive days of Moist soil:
          MCS_CondsDF_yrs$MoistDaysConsecAny <- with(MCS_CondsDF_day,
            tapply(!MCS_Dry_All, Years, max_duration))
          # TRUE = Moist more than 90 Consecutive Days
          MCS_CondsDF_yrs$COND8 <- MCS_CondsDF_yrs$MoistDaysConsecAny > 90

          # COND9 - Moist in ALL parts MORE than 45 CONSECUTIVE days in the 4
          # months following the winter solstice
          # Consecutive days of moist soil after winter solsitice:
          itemp <- MCS_CondsDF_day$DOY %in% c(355:365, 1:111)
          temp <- with(MCS_CondsDF_day[itemp, ],
              tapply(MCS_Moist_All, Years, max_duration))
          ids <- match(MCS_CondsDF_yrs[, "Years"], as.integer(names(temp)),
            nomatch = 0)
          MCS_CondsDF_yrs[ids > 0, "MoistDaysConsecWinter"] <- temp[ids]
          # TRUE = moist more than 45 consecutive days
          MCS_CondsDF_yrs$COND9 <- MCS_CondsDF_yrs$MoistDaysConsecWinter > 45

          # COND10 - MCS is Dry in ALL layers for more or equal to 360 days
          # Number of days where all soils are dry:
          MCS_CondsDF_yrs$AllDryDaysCumAny <- with(MCS_CondsDF_day,
            tapply(MCS_Dry_All, Years, sum))
          MCS_CondsDF_yrs$COND10 <- MCS_CondsDF_yrs$AllDryDaysCumAny >= 360

          icol <- c("COND0", "COND1", "COND2", "COND2_1", "COND2_2", "COND2_3",
            "COND3", "COND3_1", "COND4", "COND5", "COND6", "COND6_1", "COND7",
            "COND8", "COND9", "COND10")
          icol_new <- paste0("MCS_", icol)
          MCS_CondsDF3 <- as.matrix(MCS_CondsDF_yrs[, icol, drop = FALSE])
          if (opt_SMTR[["aggregate_at"]] == "conditions") {
            temp <- matrix(colSums(MCS_CondsDF3, na.rm = TRUE),
              nrow = 1, ncol = length(icol), dimnames = list(NULL, icol_new))
            MCS_CondsDF3 <- temp >= crit_agree
          } else {
            dimnames(MCS_CondsDF3)[[2]] <- icol_new
          }


          #---Soil moisture regime: Soil Survey Staff 2014
          # (Key to Soil Taxonomy): p.28-31
          SMTR[["SMR"]] <- t(apply(cbind(ACS_CondsDF3, MCS_CondsDF3), 1,
            function(x) do.call(SMR_logic, args = c(as.list(x),
              list(has_permafrost = has_permafrost)))))

          SMTR[["cond_annual"]][, icols1] <- as.matrix(
            ACS_CondsDF_yrs[, icols1])
          SMTR[["cond_annual"]][, icols2] <- as.matrix(
            aggregate(ACS_CondsDF_day[, icols2],
              by = list(ACS_CondsDF_day$Years), mean)[, -1])

          SMTR[["cond_annual"]][, icols3] <- as.matrix(
            MCS_CondsDF_yrs[, icols3])
          SMTR[["cond_annual"]][, icols4] <- as.matrix(
            aggregate(MCS_CondsDF_day[, icols4],
              by = list(MCS_CondsDF_day$Years), mean)[, -1])

          SMTR[["regimes_done"]] <- TRUE

        } else {
          has_notenough_normalyears <- TRUE
          SMTR[["SMR"]][] <- NA
        }

      } else {
        SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
        has_notenough_normalyears <- TRUE
      }

      if (has_notenough_normalyears) {
        if (verbose) {
          print(paste0(msg_tag, ": number of normal years is ",
            SMTR[["SMR_normalyears_N"]], " which is insufficient to calculate ",
            "NRCS soil moisture",
            if (SMTR[["SMR_normalyears_N"]] <= 0) "/temperature",
            " regimes."))
        }
      }

    } else {
      if (verbose) {
        print(paste0(msg_tag, ": has unrealistic soil temperature values: ",
          "NRCS soil moisture/temperature regimes not calculated."))
      }
      SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
      SMTR[["has_realistic_SoilTemp"]] <- 0
    }

  } else {
    if (verbose) {
      print(paste0(msg_tag, ": soil temperature module turned off but ",
        "required for NRCS Soil Moisture/Temperature Regimes."))
    }
    SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
    SMTR[["has_simulated_SoilTemp"]] <- 0
  }

  SMTR
}


#' Table 1 in Chambers et al. 2014
#'
#' @references Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant,
#'   C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and
#'   A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce
#'   Impacts of Invasive Annual Grasses and Altered Fire Regimes on the
#'   Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale
#'   Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture,
#'   Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
Chambers2014_Table1 <- function() {
  temp1 <- 2.54 * 10 * matrix(c(14, Inf, 12, 22, 12, 16, 6, 12, 8, 12),
    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("MAP_low", "MAP_high")))

  temp2 <- matrix(
    c("Cryic", "Xeric", "ModeratelyHigh", "High",
      "Frigid", "Xeric", "ModeratelyHigh", "Moderate",
      "Mesic", "Xeric", "Moderate", "ModeratelyLow",
      "Frigid", "Aridic", "Low", "Moderate",
      "Mesic", "Aridic", "Low", "Low"),

    ncol = 4, byrow = TRUE, dimnames = list(NULL,
      c("STR", "SMR", "Resilience", "Resistance")))

  data.frame(temp1, temp2, stringsAsFactors = FALSE)
}


#' Determine resilience & resistance classes (sensu Chambers et al. 2014) based
#' on soil moisture and soil temperature regimes
#'
#' @param Tregime A named numeric vector. The soil temperature regime
#'   \var{\dQuote{STR}} element of the return object of
#'   \code{\link{calc_SMTRs}}.
#' @param Sregime A named numeric vector. The soil moisture regime
#'   \var{\dQuote{SMR}} element of the return object of
#'   \code{\link{calc_SMTRs}}.
#' @param MAP_mm A numeric value. The mean annual precipitation in millimeters.
#'
#' @return A named numeric vector of \code{0s} and \code{1s} indicating
#'   the matching resilience and resistance class. Note: All elements may be
#'   0 if the input soil moisture/temperature regimes are not covered by
#'   Chambers et al. 2014.
#'
#' @references Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant,
#'   C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and
#'   A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce
#'   Impacts of Invasive Annual Grasses and Altered Fire Regimes on the
#'   Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale
#'   Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture,
#'   Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
#'
#' @examples
#' # Calculate soil moisture and soil temperature regimes
#' sw_in <- rSOILWAT2::sw_exampleData
#' sw_out <- sw_exec(inputData = sw_in)
#' SMTR <- calc_SMTRs(sim_in = sw_in, sim_out = sw_out)
#'
#' # Determine average across years and set aggregation agreement level
#' Tregime <- colMeans(SMTR[["STR"]]) >= 0.9
#' Sregime <- colMeans(SMTR[["SMR"]]) >= 0.9
#'
#' # Calculate resilience and resistance categories
#' clim <- calc_SiteClimate(weatherList = get_WeatherHistory(sw_in))
#' calc_RRs_Chambers2014(Tregime, Sregime, MAP_mm = 10 * clim[["MAP_cm"]])
#'
#' @export
calc_RRs_Chambers2014 <- function(Tregime, Sregime, MAP_mm) {

  # Result containers
  cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
  resilience <- resistance <- rep(0, times = length(cats))
  names(resilience) <- names(resistance) <- cats

  if (any(!is.na(Tregime)) && any(!is.na(Sregime))) {
    Table1 <- Chambers2014_Table1()
    Type <- as.logical(Tregime[Table1[, "STR"]]) &
      as.logical(Sregime[Table1[, "SMR"]])
    Characteristics <- MAP_mm >= Table1[, "MAP_low"] &
      MAP_mm <=  Table1[, "MAP_high"]

    # Resilience and Resistance
    is_notRR <- which(!is.na(Type) & !Type & Characteristics)
    if (any(is_notRR)) {
      resilience[Table1[is_notRR, "Resilience"]] <- 0
      resistance[Table1[is_notRR, "Resistance"]] <- 0
    }

    is_RR <- which(!is.na(Type) & Type & Characteristics)
    if (any(is_RR)) {
      resilience[Table1[is_RR, "Resilience"]] <- 1
      resistance[Table1[is_RR, "Resistance"]] <- 1
    }

  } else {
    resilience[] <- resistance[] <- NA
  }

  c(resilience = resilience, resistance = resistance)
}


#' Table 1 in Maestas et al. 2016
#'
#' @section Details: We need to make the following additional assumptions:
#'   \itemize{
#'     \item "Dry-Xeric" == "Xeric bordering on Aridic"
#'     \item "Weak-Aridic" == "Aridic bordering on Xeric"
#'   }
#'
#' @references Maestas, J.D., Campbell, S.B., Chambers, J.C., Pellant, M. &
#'   Miller, R.F. (2016). Tapping Soil Survey Information for Rapid Assessment
#'   of Sagebrush Ecosystem Resilience and Resistance. Rangelands, 38, 120-128.
Maestas2016_Table1 <- function() {
  matrix(
    c("Cryic", "Typic-Xeric", "High",
      "Cryic", "Dry-Xeric", "High",
      "Frigid", "Typic-Xeric", "High",
      "Cryic", "Weak-Aridic", "High",

      "Cryic", "Typic-Aridic", "Moderate",
      "Frigid", "Dry-Xeric", "Moderate",
      "Frigid", "Typic-Aridic", "Moderate",
      "Frigid", "Weak-Aridic", "Moderate",
      "Mesic", "Typic-Xeric", "Moderate",

      "Mesic", "Dry-Xeric", "Low",
      "Mesic", "Weak-Aridic", "Low",
      "Mesic", "Typic-Aridic", "Low"),

    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("STR", "SMR", "RR")))
}

#' Determine resilience & resistance classes (sensu Maestas et al. 2016) based
#' on soil moisture and soil temperature regimes
#'
#' @param Tregime A named numeric vector. The soil temperature regime
#'   \var{\dQuote{STR}} element of the return object of
#'   \code{\link{calc_SMTRs}}.
#' @param Sregime A named numeric vector. The soil moisture regime
#'   \var{\dQuote{SMR}} element of the return object of
#'   \code{\link{calc_SMTRs}}.
#'
#' @return A named numeric vector of \code{0s} and \code{1s} indicating
#'   the matching resilience and resistance class. Note: All elements may be
#'   0 if the input soil moisture/temperature regimes are not covered by
#'   Maestas et al. 2016.
#'
#' @references Maestas, J.D., Campbell, S.B., Chambers, J.C., Pellant, M. &
#'   Miller, R.F. (2016). Tapping Soil Survey Information for Rapid Assessment
#'   of Sagebrush Ecosystem Resilience and Resistance. Rangelands, 38, 120-128.
#'
#' @examples
#' # Calculate soil moisture and soil temperature regimes
#' sw_in <- rSOILWAT2::sw_exampleData
#' sw_out <- sw_exec(inputData = sw_in)
#' SMTR <- calc_SMTRs(sim_in = sw_in, sim_out = sw_out)
#'
#' # Determine average across years and set aggregation agreement level
#' Tregime <- colMeans(SMTR[["STR"]]) >= 0.9
#' Sregime <- colMeans(SMTR[["SMR"]]) >= 0.9
#'
#' # Calculate resilience and resistance categories
#' calc_RRs_Maestas2016(Tregime, Sregime)
#'
#' @export
calc_RRs_Maestas2016 <- function(Tregime, Sregime) {
  RR <- c(Low = NA, Moderate = NA, High = NA)

  if (any(!is.na(Tregime)) && any(!is.na(Sregime))) {
    Table1 <- Maestas2016_Table1()

    temp <- as.logical(Tregime[Table1[, "STR"]]) &
      as.logical(Sregime[Table1[, "SMR"]])

    is_notRR <- !is.na(temp) & !temp
    if (any(is_notRR)) {
      RR[Table1[is_notRR, "RR"]] <- 0
    }

    is_RR <- !is.na(temp) & temp
    if (any(is_RR)) {
      RR[Table1[is_RR, "RR"]] <- 1
    }
  }

  RR
}

#------ End of SMTR functions
########################
