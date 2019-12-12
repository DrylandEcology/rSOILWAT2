
#' Calculate the composition (land cover) representing a
#' potential natural vegetation based on climate relationships
#'
#' The function returns relative abundance/land cover values that completely
#' cover the surface (i.e., they sum to 1) of a site specified by climate and/or
#' fixed input values.
#'
#' @section Details: Some of the land cover/vegetation types, i.e., trees,
#'   annual grasses, and bare-ground are not estimated from climate
#'   relationships; they are either set to 0, or alternatively fixed at the
#'   value of the input argument(s).
#'
#' @section Details: The remaining vegetation types, i.e., shrubs, C3 grasses,
#'   C4 grasses, forbs, and succulents, are estimated from climate relationships
#'   using equations developed by Paruelo & Lauenroth 1996, or alternatively
#'   fixed at the value of the input argument(s). If values for
#'   \code{dailyC4vars} are provided, then equations developed by Teeri & Stowe
#'   1976 are used to limit the occurrence of C4 grasses.
#'
#' @section Details: The relative abundance values of the the vegetation types
#'   that can be estimated and are not fixed by inputs, are estimated in two
#'   steps: (i) as if they cover the entire surface; (ii) scaled to the
#'   proportion of the surface that is not fixed by inputs.
#'
#' @section Notes: The equations developed Paruelo & Lauenroth 1996 are based
#'  on sites with \var{MAT} from 2 C to 21.2 C and \var{MAP} from 117 to
#'  1011 mm. If \code{warn_extrapolation} is set to \code{TRUE}, then
#'  inputs are checked against supported ranges, i.e., if \var{MAT} is below
#'  1 C, then it is reset to 1 C with a warning. If other inputs exceed their
#'  ranges, then a warning is issued and the code proceeds.
#'
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter.
#' @param MAT_C A numeric value. Mean annual temperature in degree Celsius.
#' @param mean_monthly_ppt_mm A numeric vector of length 12. Mean monthly
#'   precipitation in millimeter.
#' @param mean_monthly_Temp_C A numeric vector of length 12. Mean monthly
#'   temperature in degree Celsius.
#' @param dailyC4vars A named list of length 3 or \code{NULL}.
#'   If not \code{NULL}, then a correction for C4-grasses based on
#'   Teeri & Stowe 1976 is applied based on the variables: \describe{
#'   \item{\code{Month7th_NSadj_MinTemp_C}}{Mean minimum temperature of July on
#'     the northern hemisphere and January on the southern hemisphere}
#'   \item{\code{DegreeDaysAbove65F_NSadj_DaysC}}{Degree days above 65 F = 18.33
#'     C in units of days x degree Celsius}
#'   \item{\code{LengthFreezeFreeGrowingPeriod_NSadj_Days}}{Mean annual number
#'     of days of the longest continuous period where minimum daily temperature
#'     remain above freezing} }
#' @param isNorth A logical value. \code{TRUE} for locations on northern
#'   hemisphere.
#' @param shrub_limit A numeric value. Default value is 0.2 based on page 1213
#'   of Paruelo & Lauenroth 1996.
#' @param fix_succulents A logical value. If \code{TRUE}, then value for the
#'   succulent component is fixed at \code{Succulents_Fraction} instead of
#'   calculated from climatic relationships.
#' @param Succulents_Fraction A numeric value between 0 and 1. \code{NA} is
#'   treated as if \code{fix_succulents} is \code{FALSE}.
#' @param fix_annuals A logical value. If \code{TRUE}, then value for the annual
#'   component is fixed at \code{Annuals_Fraction}.
#' @param Annuals_Fraction A numeric value. Default value is 0. A value between
#'   0 and 1.
#' @param fix_C4grasses A logical value. If \code{TRUE}, then value for the
#'   C4-grass component is fixed at \code{C4_Fraction} instead of calculated
#'   from climatic relationships.
#' @param C4_Fraction A numeric value between 0 and 1. \code{NA} is treated as
#'   if \code{fix_C4grasses} is \code{FALSE}.
#' @param fix_C3grasses A logical value. If \code{TRUE}, then value for the
#'   C3-grass component is fixed at \code{C3_Fraction} instead of calculated
#'   from climatic relationships.
#' @param C3_Fraction A numeric value between 0 and 1. \code{NA} is treated as
#'   if \code{fix_C3grasses} is \code{FALSE}.
#' @param fix_shrubs A logical value. If \code{TRUE}, then value for the shrub
#'   component is fixed at \code{Shrubs_Fraction} instead of calculated from
#'   climatic relationships.
#' @param Shrubs_Fraction A numeric value between 0 and 1. \code{NA} is treated
#'   as if \code{fix_shrubs} is \code{FALSE}.
#' @param fix_forbs A logical value. If \code{TRUE}, then value for the forb
#'   component is fixed at \code{Forbs_Fraction}.
#' @param Forbs_Fraction A numeric value. Default value is 0. A value between 0
#'   and 1.
#' @param fix_trees A logical value. If \code{TRUE}, then value for the tree
#'   component is fixed at \code{Forbs_Fraction}.
#' @param Trees_Fraction A numeric value. Default value is 0. A value between 0
#'   and 1.
#' @param fix_BareGround A logical value. If \code{TRUE}, then value for the
#'   bare ground component is fixed at \code{BareGround_Fraction}.
#' @param BareGround_Fraction A numeric value. Default value is 0. A value
#'   between 0 and 1.
#' @param fill_empty_with_BareGround A logical value. If \code{TRUE}, then
#'   incomplete land cover is considered (additional) bare-ground. If
#'   \code{FALSE}, then some hacks are used to "fill in" incomplete land cover
#'   with grasses and/or shrubs, and additionally,
#'   if \code{fix_BareGround} is \code{FALSE} with bare-ground.
#' @param warn_extrapolation A logical value. If \code{TRUE}, then
#'   warnings are issued if climate inputs \code{MAP_mm} and/or \code{MAT_C}
#'   fall outside the range of supported values. See notes.
#'
#' @return A list with three named numeric vectors. \describe{
#'   \item{Rel_Abundance_L0}{A numeric vector of length 8 with
#'     relative abundance/cover [0-1] values of land cover types that sum to 1.
#'     The names of the 8 types are: \var{Succulents}, \var{Forbs},
#'     \var{Grasses_C3}, \var{Grasses_C4}, \var{Grasses_Annuals},
#'     \var{Shrubs}, \var{Trees}, \var{BareGround}.
#'   }
#'   \item{Rel_Abundance_L1}{A numeric vector of length 5 with
#'     relative abundance/cover [0-1] values of \pkg{rSOILWAT2} land cover
#'     types that sum to 1.
#'     The names of the 5 types are: \var{SW_TREES}, \var{SW_SHRUBS},
#'     \var{SW_FORBS}, \var{SW_GRASS}, and \var{SW_BAREGROUND}.
#'   }
#'   \item{Grasses}{A numeric vector of length 3 with
#'     relative abundance/cover [0-1] values of the grass types that sum to 1.
#'     The names of the 3 sub-types are: \var{Grasses_C3}, \var{Grasses_C4},
#'     and \var{Grasses_Annuals}.
#'   }
#' }
#'
#' @references Paruelo J.M., Lauenroth W.K. (1996) Relative abundance of plant
#'   functional types in grasslands and shrublands of North America. Ecological
#'   Applications, 6, 1212-1224.
#' @references Teeri J.A., Stowe L.G. (1976) Climatic patterns and the
#'   distribution of C4 grasses in North America. Oecologia, 23, 1-12.
#'
#' @examples
#' ## Load weather dataset from rSOILWAT2
#' data("weatherData", package = "rSOILWAT2")
#' clim1 <- calc_SiteClimate(weatherList = weatherData)
#' clim2 <- calc_SiteClimate(weatherList = weatherData, do_C4vars = TRUE)
#'
#' ## All estimable vegetation types are estimated:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim1[["MAP_cm"]], MAT_C = clim1[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]])
#'
#' ## Climate is outside supported range with MAT < 0 C:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim1[["MAP_cm"]],
#'   MAT_C = clim1[["MAT_C"]] - clim1[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]] - clim1[["MAT_C"]])
#'
#' ## Some land cover types are fixed and others are estimated, and
#' ## the C4-grass adjustment is used:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim2[["MAP_cm"]], MAT_C = clim2[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim2[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim2[["meanMonthlyTempC"]],
#'   dailyC4vars = clim2[["dailyC4vars"]],
#'   fix_shrubs = TRUE, Shrubs_Fraction = 0.5,
#'   fix_BareGround = TRUE, BareGround_Fraction = 0.25)
#'
#' @export
estimate_PotNatVeg_composition <- function(MAP_mm, MAT_C,
  mean_monthly_ppt_mm, mean_monthly_Temp_C, dailyC4vars = NULL,
  isNorth = TRUE, shrub_limit = 0.2,
  fix_succulents = FALSE, Succulents_Fraction = NA,
  fix_annuals = TRUE, Annuals_Fraction = 0,
  fix_C4grasses = FALSE, C4_Fraction = NA,
  fix_C3grasses = FALSE, C3_Fraction = NA,
  fix_shrubs = FALSE, Shrubs_Fraction = NA,
  fix_forbs = FALSE, Forbs_Fraction = NA,
  fix_trees = TRUE, Trees_Fraction = 0,
  fix_BareGround = TRUE, BareGround_Fraction = 0,
  fill_empty_with_BareGround = TRUE,
  warn_extrapolation = TRUE) {

  veg_types <- c("Succulents", "Forbs",
    "Grasses_C3", "Grasses_C4", "Grasses_Annuals",
    "Shrubs", "Trees",
    "BareGround")
  Nveg <- length(veg_types)

  isuc <- 1 # succulents
  ifor <- 2 # forbs
  igc3 <- 3 # grasses-C3
  igc4 <- 4 # grasses-C4
  igan <- 5 # grasses-annuals
  ishr <- 6 # shrubs
  itre <- 7 # trees
  ibar <- 8 # bare-ground

  veg_cover <- rep(0, Nveg)

  # groups without climate-equations, i.e., always set to a specific value
  iset <- c(igan, itre, ibar)

  # groups with climate-equations to estimate relative abundance
  iestim <- c(igc4, igc3, ishr, ifor, isuc)
  igrasses <- c(igc3, igc4, igan)


  #--- Get the user specified fractions: input cover fraction values:
  input_cover <- rep(NA, Nveg)

  # Groups that are either fixed or 0, i.e., cannot be NA = not estimated
  input_cover[igan] <- if (fix_annuals) finite01(Annuals_Fraction) else 0
  input_cover[itre] <- if (fix_trees) finite01(Trees_Fraction) else 0
  input_cover[ibar] <- if (fix_BareGround) finite01(BareGround_Fraction) else 0

  # Groups that are either fixed or estimated based on climate-relationships
  input_cover[igc4] <- if (fix_C4grasses) C4_Fraction else NA
  input_cover[igc3] <- if (fix_C3grasses) C3_Fraction else NA
  input_cover[ishr] <- if (fix_shrubs) Shrubs_Fraction else NA
  input_cover[ifor] <- if (fix_forbs) Forbs_Fraction else NA
  input_cover[isuc] <- if (fix_succulents) Succulents_Fraction else NA

  # treat negative input values as if NA
  input_cover <- cut0Inf(input_cover, val = NA)

  #--- Decide if all fractions are sufficiently defined or if they need to be
  # estimated based on climate reltionships
  input_sum <- sum(input_cover, na.rm = TRUE)
  ifixed <- unique(c(iset, which(!is.na(input_cover))))

  ids_to_estim <- is.na(input_cover)
  n_to_estim <- sum(ids_to_estim)

  if (input_sum > 1) {
    stop("'estimate_PotNatVeg_composition': ",
        "User defined relative abundance values sum to more than ",
        "1 = full land cover.")
  }

  #--- Incomplete surface cover
  veg_cover <- input_cover

  if (n_to_estim <= 1) {
    #--- Less than one component to estimate: no need for equations

    if (n_to_estim == 0) {
      #--- All fixed, nothing to estimate
      if (fill_empty_with_BareGround) {
        veg_cover[ibar] <- 1 - sum(veg_cover[-ibar], na.rm = TRUE)

      } else {
        stop("'estimate_PotNatVeg_composition': ",
          "User defined relative abundance values are all fixed, ",
          "but their sum is smaller than 1 = full land cover.")
      }

    } else if (n_to_estim == 1) {
      #--- One value to estimate: difference from rest
      veg_cover[ids_to_estim] <- 1 - input_sum
    }

  } else {
    #---Potential natural vegetation
    # i.e., (input_sum < 1 && sum(is.na(input_cover)) > 1) is TRUE;
    # thus, estimate relative abundance fractions based on climate relationships

    if (MAP_mm <= 1) {
      # No precipitation ==> no vegetation, only bare-ground
      # TODO: what about fog?
      veg_cover[] <- 0
      veg_cover[ibar] <- 1

    } else {

      estim_cover <- rep(NA, Nveg)

      # Estimate climate variables
      if (isNorth) {
        Months_WinterTF <- c(12, 1:2)
        Months_SummerTF <- c(6:8)
      } else {
        Months_WinterTF <- c(6:8)
        Months_SummerTF <- c(12, 1:2)
      }

      # Fraction of precipitation falling during summer/winter months
      ppt.SummerToMAP <- sum(mean_monthly_ppt_mm[Months_SummerTF]) / MAP_mm
      ppt.WinterToMAP <- sum(mean_monthly_ppt_mm[Months_WinterTF]) / MAP_mm

      # Temperature in July minus temperature in January
      therm_amp <- mean_monthly_Temp_C[Months_SummerTF[2]] -
        mean_monthly_Temp_C[Months_WinterTF[2]]

      if (warn_extrapolation) {
        # Adjust climate variables to limits underlying the data used to develop
        # equations Paruelo & Lauenroth (1996): "The selected sites cover a
        #   range of MAT from 2 C to 21.2 C and a range of precipitation (MAP)
        #   from 117 to 1011 mm"

        # MAT limits:
        if (MAT_C < 1) {
          # Note: MAT = 1 C as limit instead of 2 C based on empirical testing;
          # also because log(x) is undefined for x < 0 and results in negative
          # values for x < 1. Hence the threshold of 1.
          warning("Equations used outside supported range (2 - 21.2 C): ",
           "MAT = ", round(MAT_C, 2), " C reset to 1 C.")
          MAT_C <- 1
        }

        if (MAT_C > 21.2) {
          warning("Equations used outside supported range (2 - 21.2 C): ",
            "MAT = ", round(MAT_C, 2), " C.")
        }

        if (MAP_mm < 117 || MAP_mm > 1011) {
          warning("Equations used outside supported range (117-1011 mm): ",
            "MAP = ", round(MAP_mm), " mm.")
        }
      }


      # 1. step: estimate relative abundance based on
      # Paruelo & Lauenroth (1996): shrub climate-relationship:
      if (MAP_mm < 1) {
        estim_cover[ishr] <- 0
      } else {
        # if not enough winter precipitation for a given MAP, then equation
        # results in negative values which we set to 0
        estim_cover[ishr] <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) +
          1.5451 * ppt.WinterToMAP, val = 0)
      }

      # Paruelo & Lauenroth (1996): C4-grass climate-relationship:
      if (MAT_C <= 0) {
        estim_cover[igc4] <- 0
      } else {
        # if either MAT < 0 or not enough summer precipitation or
        # too cold for a given MAP, then equation results in negative values
        # which we set to 0
        estim_cover[igc4] <- cut0Inf(-0.9837 + 0.000594 * MAP_mm +
          1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C), val = 0)

        # 2. step: Teeri JA, Stowe LG (1976)
        # This equations give percent species/vegetation -> use to limit
        # Paruelo's C4 equation, i.e., where no C4 species => C4 abundance == 0
        if (is.list(dailyC4vars)) {
          if (dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0) {
            grass_c4_species <- 0
          } else {
            x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9 / 5 + 32
            x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9 / 5
            x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
            grass_c4_species <- as.numeric(
              (1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
          }

          if (grass_c4_species <= rSW2_glovars[["tol"]]) {
            estim_cover[igc4] <- 0
          }
        }
      }

      # Paruelo & Lauenroth (1996): C3-grass climate-relationship:
      if (ppt.WinterToMAP <= 0) {
        c3_in_grassland <- c3_in_shrubland <- NA
      } else {
        # if not enough winter precipitation or too warm for a
        # given MAP, then equation results in negative values which we set to 0
        c3_in_grassland <- cut0Inf(1.1905 - 0.02909 * MAT_C +
          0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1, val = 0)
        c3_in_shrubland <- cut0Inf(1.1905 - 0.02909 * MAT_C +
          0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2, val = 0)
      }

      temp <- estim_cover[ishr] >= shrub_limit && !is.na(estim_cover[ishr])
      estim_cover[igc3] <- ifelse(temp, c3_in_shrubland, c3_in_grassland)

      # Paruelo & Lauenroth (1996): forb climate-relationship:
      if (MAP_mm < 1 || MAT_C <= 0) {
        estim_cover[ifor] <- NA
      } else {
        estim_cover[ifor] <- cut0Inf(-0.2035 + 0.07975 * log(MAP_mm) -
          0.0623 * log(MAT_C), val = 0)
      }

      # Paruelo & Lauenroth (1996): succulent climate-relationship:
      if (therm_amp <= 0 || ppt.WinterToMAP <= 0) {
        estim_cover[isuc] <- NA
      } else {
        estim_cover[isuc] <- cut0Inf(-1 +
            1.20246 * therm_amp ^ -0.0689 * ppt.WinterToMAP ^ -0.0322, val = 0)
      }

      # 3. step:
      ngood <- sum(!is.na(estim_cover[iestim]))

      # Any remaining NAs are set to 0
      estim_cover[iestim] <- replace_NAs_with_val(estim_cover[iestim],
        val_replace = 0)

      if (!fill_empty_with_BareGround && ngood <= 1) {
        #--- Hack if some of the equations produced NAs:
        # [these rules are made up arbitrarily by drs, Nov 2012]:
        # If no or only one successful equation, then add
        #   100% C3 if MAT < 10 C,
        #   100% shrubs if MAP < 600 mm, and
        #   100% C4 if MAT >= 10C & MAP >= 600 mm
        if (MAP_mm < 600) {
          estim_cover[ishr] <- 1 + estim_cover[ishr]
        }

        if (MAT_C < 10) {
          estim_cover[igc3] <- 1 + estim_cover[igc3]
        }

        if (MAT_C >= 10  & MAP_mm >= 600) {
          estim_cover[igc4] <- 1 + estim_cover[igc4]
        }
      }


      # 4. step: put all together:
      # i) groups with set values (iset) and groups with estimable but
      #    fixed values (iestim & !is.na)
      veg_cover[ifixed] <- input_cover[ifixed]

      # ii) groups with values to estimate (iestim & is.na):
      veg_cover[ids_to_estim] <- estim_cover[ids_to_estim]

      estim_cover_sum <- sum(estim_cover[ids_to_estim])

      if (estim_cover_sum > 0) {
        # Scale fractions to 0-1 with a sum equal to 1 that includes
        # veg_cover[ifixed], but but doesn't scale those that are fixed
        veg_cover[ids_to_estim] <- veg_cover[ids_to_estim] *
          (1 - sum(veg_cover[ifixed])) / estim_cover_sum

      } else {
        # cover to estimate is 0 and fixed_cover_sum < 1
        if (fill_empty_with_BareGround && !fix_BareGround) {
          # ==> fill land cover up with bare-ground
          veg_cover[ibar] <- 1 - sum(veg_cover[-ibar])

        } else {
          stop("'estimate_PotNatVeg_composition': ",
            "The estimated vegetation cover values are 0, ",
            "the user fixed relative abundance values sum to less than 1, ",
            "and bare-ground is fixed. ",
            "Thus, the function cannot compute complete land cover composition."
          )
        }
      }
    }
  }

  names(veg_cover) <- veg_types

  # Scale relative grass components to one (or set to 0)
  c3c4ann <- veg_cover[igrasses]
  grass_fraction <- sum(c3c4ann)

  if (grass_fraction > 0) {
    c3c4ann <- c3c4ann / grass_fraction
  }

  # Return values
  temp <- unname(veg_cover)

  list(
    # Full resolution: suitable for STEPWAT2
    Rel_Abundance_L0 = veg_cover,

    # SOILWAT2 land cover types:
    Rel_Abundance_L1 = c(
      SW_TREES = temp[itre],
      SW_SHRUB = temp[ishr],
      SW_FORBS = temp[ifor] + temp[isuc],
      SW_GRASS = grass_fraction,
      SW_BAREGROUND = temp[ibar]),

    # Relative contributions of sub-types to the grass type
    Grasses = c3c4ann
  )
}


get_season <- function(mo_season_TF, N_season = NULL) {
  # Calculate first month of season == last start in (circular) year
  starts <- calc_starts(mo_season_TF)

  if (is.null(N_season)) {
    N_season <- sum(mo_season_TF)
  }

  tmp <- starts[length(starts)] + seq_len(N_season) - 2
  tmp %% 12 + 1
}


#' Predict seasonal phenology
#'
#' Fits a loess-curve to a seasonal subset of monthly values and uses the fit
#' to make a prediction for a different subset of months.
#'
#' @param x A two-dimensional object. Rows correspond to month of the year,
#'   i.e., there must be 12 rows, and columns correspond to variables that are
#'   adjusted.
#' @param ref_season A numeric vector. The season as months (integers in 1-12)
#'   for which the values of \code{x} serve as reference (used for fitting).
#' @param target_season A numeric vector. The season as months (1-12)
#'   for which the prediction is made.
#'
#' @return A two-dimensional object with rows that correspond to
#'   \code{target_season} and columns that correspond to the adjusted variable
#'   values from \code{x}.
#'
#' @section Details: You may want to eliminate negative values that may arise
#'   from the fitting/prediction process.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' biomass_reference <- swProd_MonProd_grass(sw_in)[, 2:3]
#' biomass_adj <- biomass_reference
#' biomass_adj[] <- NA
#'
#' ## Adjust phenology from a reference Nov-Feb to a target Jul-Oct
#' ## non-growing season
#' ref_season <- c(11:12, 1:2) # November-February
#' target_season <- 7:10 # July-October
#' biomass_adj[target_season, ] <- predict_season(biomass_reference,
#'   ref_season, target_season)
#'
#' ## Adjust phenology from a reference Mar-Oct to a target Nov-Jun
#' ## growing season
#' ref_season <- 3:10 # March-October
#' target_season <- c(11:12, 1:6) # November-June
#' biomass_adj[target_season, ] <- predict_season(biomass_reference,
#'   ref_season, target_season)
#'
#' ## Plot reference and adjusted monthly values
#' \dontrun{
#' par_prev <- par(mfrow = c(2, 1))
#'
#' plot(1:12, biomass_reference[, 1], type = "l", col = "red",
#'   ylim = c(0, 250), xlab = "Month")
#' lines(1:12, biomass_adj[, 1])
#'
#' plot(1:12, biomass_reference[, 2], type = "l", col = "red",
#'   ylim = c(0, 1), xlab = "Month")
#' lines(1:12, biomass_adj[, 2])
#'
#' par(par_prev)
#' }
#'
#' @export
predict_season <- function(x, ref_season, target_season) {

  # `x` must be a two-dimensional object with 12 rows (corresponding to months)
  if (!inherits(x, c("matrix", "data.frame"))) {
    x <- as.matrix(x)
  }

  stopifnot(nrow(x) == 12L)

  # calculate padded reference season: add one month on either side so that
  # `stats::loess` does not introduce weird step predictions at season start/end
  temp <- c(ref_season[1] - 1, ref_season, ref_season[length(ref_season)] + 1)
  ref_padded <- (temp - 1) %% 12 + 1
  ref_seq <- 0:(length(ref_padded) - 1)

  # calculate sequence of target season
  target_seq <- seq(from = 1, to = length(ref_season),
    length = length(target_season))

  # length(std.season.seq) >= 3 because of padding and
  # test that season duration > 0
  lcoef <- calc.loess_coeff(N = length(ref_seq), span = 0.4)

  op <- options(c("warn", "error"))
  on.exit(options(op))
  # turn off warnings because stats::loess throws many warnings:
  # 'pseudoinverse used', see calc.loess_coeff(), etc.
  options(warn = -1, error = traceback)

  # Fit loess to reference season and predict to target season
  apply(x, 2, function(x) {
    lf <- stats::loess(
      x[ref_padded] ~ ref_seq,
      span = lcoef$span,
      degree = lcoef$degree)

    predict(lf, newdata = data.frame(ref_seq = target_seq))
  })
}


#' Adjust biomass phenology by pattern of growing/non-growing seasons to
#' site-specific climatic conditions
#'
#' @param x A two-dimensional object or a list of such objects. Rows of each
#'   such object correspond to month of the year, i.e., there must be 12 rows,
#'   and columns correspond to variables that are adjusted. These values
#'   reflect the phenological pattern described by
#'   \code{reference_growing_season}.
#' @param reference_growing_season A numeric vector with values
#'   between 1 and 12. Number of months that describe the potential
#'   active (growing) season of the input data \code{x}.
#'   Default values \code{3:10} describe a March-October growing season.
#' @param mean_monthly_temp_C A numeric vector of length 12. Mean monthly
#'   temperatures in Celsius of a target site for which \code{x} is
#'   adjusted.
#' @param growing_limit_C A numeric value. \code{mean_monthly_temp_C} equal or
#'   above this limit are considered suitable for growth (growing season).
#'   Default value is 4 C.
#' @param isNorth A logical value. \code{TRUE} for locations on the northern
#'   hemisphere, \code{FALSE} for locations on the southern hemisphere.
#'
#' @return An object as \code{x}, i.e., a two-dimensional object or a list of
#'   such objects with values adjusted to represent the phenology of a target
#'   described by \code{mean_monthly_temp_C}.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' biomass_reference <- swProd_MonProd_grass(sw_in)[, 2:3]
#'
#' ## Adjust phenology from the reference Mar-Oct to a target Nov-Jun
#' ## growing season
#' biomass_adj <- adjBiom_by_temp(
#'   x = biomass_reference,
#'   mean_monthly_temp_C = c(rep(10, 6), rep(0, 4), rep(10, 2)))
#'
#' ## Plot reference and adjusted monthly values
#' \dontrun{
#' par_prev <- par(mfrow = c(2, 1))
#'
#' plot(1:12, biomass_reference[, 1], type = "l", col = "red",
#'   ylim = c(0, 250), xlab = "Month")
#' lines(1:12, biomass_adj[, 1])
#'
#' plot(1:12, biomass_reference[, 2], type = "l", col = "red",
#'   ylim = c(0, 1), xlab = "Month")
#' lines(1:12, biomass_adj[, 2])
#'
#' par(par_prev)
#' }
#'
#' @export
adjBiom_by_temp <- function(x, mean_monthly_temp_C, growing_limit_C = 4,
  reference_growing_season = 3:10, isNorth = TRUE) {

  .Deprecated(new = "adj_phenology_by_temp")

  # Convert `x` to a list
  return_list <- inherits(x, "list")
  if (!return_list) {
    x <- list(x)
  }

  # Convert each element of x into a two-dimensional object with 12 rows
  for (k in seq_along(x)) {
    if (!inherits(x[[k]], c("matrix", "data.frame"))) {
      x[[k]] <- as.matrix(x[[k]])
    }
  }

  # Make sure that inputs represent twelve months
  stopifnot(
    sapply(x, nrow) == 12L,
    length(mean_monthly_temp_C) == 12L,
    all(reference_growing_season >= 1L),
    all(reference_growing_season <= 12L)
  )

  # Determine seasons: non-growing season, growing season
  mo_seasons_TF <- matrix(NA, nrow = 12, ncol = 2,
    dimnames = list(NULL, c("nongrowing", "growing")))
  mo_seasons_TF[, 1] <- as.vector(mean_monthly_temp_C < growing_limit_C)
  mo_seasons_TF[, 2] <- !mo_seasons_TF[, 1]
  n_seasons <- apply(mo_seasons_TF, 2, sum)


  # Describe seasonal conditions for which the input values are valid:
  ref_seasons <- list(
    nongrowing = {
      tmp <- rep(FALSE, 12)
      tmp[rSW2_glovars[["st_mo"]][-reference_growing_season]] <- TRUE
      get_season(
        mo_season_TF = tmp,
        N_season = sum(tmp)
      )},
    growing = {
      tmp <- rep(FALSE, 12)
      tmp[reference_growing_season] <- TRUE
      get_season(
        mo_season_TF = tmp,
        N_season = sum(tmp)
      )}
  )

  # Standard growing season is for northern hemisphere:
  # target needs to be adjusted for southern Hemisphere
  if (!isNorth) {
    mo_seasons_TF <- rbind(mo_seasons_TF[7:12, ], mo_seasons_TF[1:6, ])
  }

  # save reference: reference values required for "overlap" between seasons
  x_ref <- x

  # Adjust for timing and duration of non-growing/growing seasons
  for (iseason in 1:2) {
    if (n_seasons[iseason] > 0) {
      if (n_seasons[iseason] < 12) {
        site_season <- which(mo_seasons_TF[, iseason])

        site_season_months <- get_season(
          mo_season_TF = mo_seasons_TF[, iseason],
          N_season = n_seasons[iseason]
        )

        for (k in seq_along(x)) {
          x[[k]][site_season_months, ] <- cut0Inf(
            predict_season(x_ref[[k]], ref_seasons[[iseason]], site_season),
            val = 0)
        }

      } else {
        # if season lasts 12 months
        for (k in seq_along(x)) {
          # if non-growing/growing season: mean/max of the season's months
          fun <- switch(iseason, "mean", "max")
          temp <- apply(x_ref[[k]][ref_seasons[[iseason]], , drop = FALSE],
            MARGIN = 2, FUN = fun)
          x[[k]][] <- matrix(temp, nrow = 12, ncol = ncol(x[[k]]), byrow = TRUE)
        }
      }
    }
  }

  # Adjustements were done as if on northern hemisphere
  if (!isNorth) {
    for (k in seq_along(x)) {
      x[[k]] <- rbind(x[[k]][7:12, ], x[[k]][1:6, ])
    }
  }

  if (return_list) x else x[[1L]]
}



#' Find location of minimum and maximum
get_season_description_v2 <- function(x) {
  c(
    min = {
      tmp <- which(abs(x - min(x)) < rSW2_glovars[["tol"]])
      as.integer(quantile(tmp, probs = 0.5, type = 3, names = FALSE))
    },
    peak = {
      tmp <- which(abs(x - max(x)) < rSW2_glovars[["tol"]])
      as.integer(quantile(tmp, probs = 0.5, type = 3, names = FALSE))
    }
  )
}


#' Adjust a phenological pattern to a different temperature regime
#'
#' Extract the characteristics of seasonal (mean monthly) values of biomass,
#' activity, etc. (i.e., phenology) that were observed for a specific
#' (reference) climate (at a specific site), and project those characteristics
#' to the mean monthly temperature values of a different site and/or different
#' climate conditions.
#'
#' The algorithm attempts to adjust for the following patterns:
#' \itemize{
#'   \item Shifts in timing of highest/lowest seasonal biomass/activity
#'   \item Effects of residual temperature differences not accounted
#'         for by shifts in timing while maintaining a unimodal phenological
#'         pattern
#'   \item Maintain inactive period if present
#'   \item Maintain seasonality expressed as correlation between phenology
#'         and mean monthly temperature values
#' }
#'
#' @param x A numeric vector of length 12. The values
#'   reflect the phenological pattern that occur on average under
#'   reference mean monthly temperature values \code{ref_temp}.
#' @param ref_temp A numeric vector of length 12. Reference mean monthly
#'   temperature values in degree Celsius under which \code{x} was
#'   determined / is valid.
#' @param target_temp A numeric vector of length 12. Mean monthly
#'   temperature values in degree Celsius of a target site / condition
#'   for which \code{x} is to be adjusted.
#'
#' @return An updated copy of \code{x} where the values have been adjusted
#'   to represent the phenology of a target climate described by
#'   \code{target_temp}.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' phen_reference <- as.data.frame(
#'   swProd_MonProd_grass(sw_in)[, c("Biomass", "Live_pct")]
#' )
#'
#' clim <- calc_SiteClimate(weatherList = rSOILWAT2::weatherData)
#' ref_temp <- clim[["meanMonthlyTempC"]]
#' temp_randomwarmer3C <- ref_temp + stats::rnorm(12, 3, 1)
#'
#' ## Adjust phenology from the reference Mar-Oct to a target Nov-Jun
#' ## growing season
#' phen_adj <- sapply(phen_reference, function(x) adj_phenology_by_temp(
#'   x = x,
#'   ref_temp = ref_temp,
#'   target_temp = temp_randomwarmer3C
#' ))
#'
#' ## Plot reference and adjusted monthly values
#' \dontrun{
#' par_prev <- par(mfrow = c(2, 1))
#'
#' plot(1:12, phen_reference[, 1], type = "l", col = "red",
#'   ylim = c(0, 250), xlab = "Month")
#' lines(1:12, phen_adj[, 1])
#' legend("bottom", ncol = 2,
#'   legend = c("reference", "adjusted"),
#'   lwd = 2, lty = 1, col = c("red", "black")
#' )
#'
#' plot(1:12, phen_reference[, 2], type = "l", col = "red",
#'   ylim = c(0, 1), xlab = "Month")
#' lines(1:12, phen_adj[, 2])
#'
#' par(par_prev)
#' }
#'
#' @export
adj_phenology_by_temp <- function(x, ref_temp, target_temp) {

  stopifnot(
    length(x) == 12,
    length(ref_temp) == 12,
    length(target_temp) == 12
  )

  mon <- rSW2_glovars[["st_mo"]]

  #------ Step 1) Estimate month of peak and of minimum, and
  # calculate differences in timing
  ref_desc <- get_season_description_v2(ref_temp)
  target_desc <- get_season_description_v2(target_temp)

  name_periods <- names(ref_desc)
  n_periods <- length(ref_desc)

  desc_diffs <- as.integer(circ_minus(target_desc, ref_desc, int = 12))
  names(desc_diffs) <- names(ref_desc)


  #------ Step 2) Translate sequence of months from the reference to the target
  # based on season descriptions of two points: minimum and peak

  #--- Adjust lengths of sub-periods: (i) peak to min, (ii) min to peak
  seq_periods <- name_periods[1 + (seq_along(name_periods) - 2) %% n_periods]

  target_mon <- NULL
  id_peak <- NA

  for (p in seq_periods) {
    i <- which(p == name_periods)
    iprev <- 1 + (i - 2) %% n_periods

    # Identify months of sub-period `p` in reference
    ids <- circ_seq(
      from = ref_desc[iprev],
      to = ref_desc[p],
      int = 12
    )

    n_ids <- length(ids)

    # Target sub-period duration
    n_ids2 <- n_ids + desc_diffs[p] - desc_diffs[iprev]

    # Resample sub-period to adjusted duration in target
    ids2 <- if (n_ids2 == 0L) {
      NULL
    } else if (n_ids2 == 1L) {
      mean(ids)
    } else {
      circ_seq(
        from = ids[1],
        to = ids[n_ids],
        int = 12,
        length.out = n_ids2
      )[-1]
    }

    # Add to new month sequence
    target_mon <- c(target_mon, ids2)

    if (p == "peak") {
      id_peak <- length(target_mon)
    }
  }


  #--- Fix shift in seasonality by difference in peak month
  shift <- target_desc["peak"] - id_peak

  if (shift != 0L) {
    ids <- 1 + (mon - shift - 1) %% 12L
    target_mon <- target_mon[ids]
  }

  stopifnot(length(target_mon) == 12L)


  #------ Step 3) estimate periodic spline and apply to adjusted target months
  res <- predict(
    splines::periodicSpline(x ~ mon, period = 12),
    target_mon)[["y"]]


  #------- Step 4) Scale up/down to correct for residual temperature effects
  res2 <- res

  ttmp <- predict(
    splines::periodicSpline(ref_temp ~ mon, period = 12),
    target_mon)[["y"]]

  # Assume: temperature span above 0 C corresponds to activity span above mean
  temp_span <- max(0, mean(sort(ref_temp, decreasing = TRUE)[1:3]) - 0)
  x_span <- max(0, mean(sort(x, decreasing = TRUE)[1:3]) - mean(x))

  if (temp_span > 0 && x_span > 0) {
    tmp <- (target_temp - ttmp) / temp_span * x_span
    tmp_res <- res
    tmp2 <- tmp

    if (diff(range(tmp)) / diff(range(res)) > 0.1) {
      # apply minimal up/down shift
      dmin <- min(tmp)
      tmp_res <- res + dmin
      tmp2 <- tmp - dmin

      # smooth the rest with a (concave) quadratic function
      # to avoid "false secondary peaks" in the result
      # (phenological activity is mostly unimodal)
      is_to_smooth <- get_season(mo_season_TF = tmp2 > rSW2_glovars[["tol"]])

      m <- stats::lm(tmp2[is_to_smooth] ~
          stats::poly(seq_along(is_to_smooth), 2))

      if (coef(m)[3] < 0) {
        # only use if concave
        tmp2[is_to_smooth] <- fitted(m)
      }
    }

    res2 <- pmax(min(x), tmp_res + tmp2)
  }

  # If climate has any months with sub-zero temps, then
  # don't apply additive correction to low activity months
  if (any(target_temp <= 0)) {
    low_value <- 0.1 * diff(range(x))
    res2 <- ifelse(res <= low_value, res, res2)
  }


  #----- Step 5) Inactive phase: allow activity to go/stay at 0
  res3 <- res2
  is_x_zero <- x < rSW2_glovars[["tol"]]
  is_res_zero <- res2 < rSW2_glovars[["tol"]]

  if (any(is_x_zero) && sum(is_res_zero) / sum(is_x_zero) <= 0.5) {
    # Do we have a flat lower level or a minimum valley?
    ids_res_low <- get_season(abs(res2 - min(res2)) < rSW2_glovars[["tol"]])
    low_duration <- length(ids_res_low)

    if (low_duration > 0) {
      # Zap flat lower level to zero
      if (low_duration > 1) {
        # Ease transition down to zero if longer than 5 month
        # by not zapping first/last month
        if (low_duration > 5) {
          ids_res_low <- ids_res_low[-c(1, length(ids_res_low))]
        }

      } else {
        # Zap minimum and contiguous surroundings to zero
        low_surroundings <- min(res2) + min(
          min(x[x > 0]) / diff(range(x)) * diff(range(res2)),
          quantile(x, probs = mean(is_x_zero))
        )

        tmp <- low_surroundings > res2

        # periodic running median with window = 3 (see ?runmed)
        ids_res_low <- c(
          median(tmp[c(12, 1:2)]),
          apply(stats::embed(tmp, 3), 1, median),
          median(tmp[c(11:12, 1)])
        )
      }

      # Zap low values to zero
      res3[ids_res_low] <- 0
    }
  }

  #------ Step 6) Maintain seasonality
  res5 <- res3
  rho_ref <- suppressWarnings(cor(x, ref_temp))
  rho_delta <- cor(res5, target_temp) - rho_ref

  if (is.finite(rho_delta) && abs(rho_delta) > 0.05) {

    # Define bounds within which optimization is attempted
    #  - Stay within band of (1 - fudge[1, 1], 1 + fudge[2, 1]) of annual range
    #  - Stay within (1 - fudge[1, 2], 1 + fudge[2, 2]) of monthly values
    #  - If overall temperature increases, then make positive band larger
    #  - If overall temperature decreases, then make negative band larger
    #  - Keep zeros at zero
    tdelta <- mean(target_temp) - mean(ref_temp)
    ff <- if (tdelta > 0.5) {
      c(0.1, 1)
    } else if (tdelta < -0.5) {
      c(1, 0.1)
    } else {
      c(0.5, 0.5)
    }
    fband <- c(0.25, 0.75)
    fudge <- rbind(ff[1] * fband, ff[2] * fband)
    is_res_zero <- res5 < rSW2_glovars[["tol"]]

    lower <- pmax(0,
      (1 - fudge[1, 1]) * min(res5),
      (1 - fudge[1, 2]) * res5
    )

    upper <- pmax(0,
      lower + 0.01,
      pmin(
        (1 + fudge[2, 1]) * max(res5),
        (1 + fudge[2, 2]) * res5
      )
    )

    # Ideally: keep zeros at zero, but cannot optimize 0-0 because of
    # "non-finite finite-difference value" error
    lower <- ifelse(is_res_zero, 0, lower)
    upper <- ifelse(is_res_zero, 0.01, upper)

    # Optimize target values so that they maintain seasonality
    # as good as possible
    tmp <- try(stats::optim(
      par = res5,
      fn = function(res, cor_ref = rho_ref) {
        abs(cor_ref - cor(res, target_temp))
      },
      method = "L-BFGS-B",
      lower = lower,
      upper = upper
    )$par,
      silent = TRUE
    )

    res5 <- if (inherits(tmp, "try-error")) res5 else tmp

    # Zap back to zero if needed
    res5 <- ifelse(is_res_zero, 0, res5)
  }


  # Make sure that we don't get negative biomass/activity values
  ifelse(res5 < 0, 0, res5)
}


#' Biomass equations
#'
#' @param MAP_mm A numeric vector. Mean annual precipitation in
#'   millimeters (mm).
#' @references Milchunas & Lauenroth 1993 (Fig. 2):
#'   Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
#' @name biomass
NULL

#' Estimate shrub biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2

#' Estimate grass biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5

adjBiom_by_ppt <- function(biom_shrubs, biom_C3, biom_C4, biom_annuals,
  biom_maxs, map_mm_shrubs, map_mm_std_shrubs,
  map_mm_grasses, map_mm_std_grasses,
  vegcomp_std_shrubs, vegcomp_std_grass) {

  # Intercepts to match outcomes of M & L 1993 equations under 'default'
  # MAP with our previous default inputs for shrubs and sgs-grasslands
  # Whereas these intercepts were introduced artificially, they could also be
  # interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found
  # "Accumulation in the standing dead was 63% of inputs, in the litter 8%,
  # and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977)
  # Dynamics of dry matter production in a mixed-grass prairie in western
  # North Dakota. Oecologia, 27, 339-351.
  Shrub_ANPPintercept <- (vegcomp_std_shrubs[1] * biom_maxs["Sh.Amount.Live"] +
    vegcomp_std_shrubs[2] * biom_maxs["C3.Amount.Live"] +
    vegcomp_std_shrubs[3] * biom_maxs["C4.Amount.Live"]) -
    Shrub_ANPP(map_mm_std_shrubs)
  Grasses_ANPPintercept <- (vegcomp_std_grass[1] * biom_maxs["Sh.Amount.Live"] +
    vegcomp_std_grass[2] * biom_maxs["C3.Amount.Live"] +
    vegcomp_std_grass[3] * biom_maxs["C4.Amount.Live"]) -
    Grass_ANPP(map_mm_std_grasses)

  # Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
  minimum.totalBiomass <- 0 # This is a SOILWAT2 parameter
  Shrub_BiomassScaler <- max(minimum.totalBiomass,
    Shrub_ANPP(map_mm_shrubs) + Shrub_ANPPintercept)
  Grass_BiomassScaler <- max(minimum.totalBiomass,
    Grass_ANPP(map_mm_grasses) + Grasses_ANPPintercept)

  # Scale live biomass amount by productivity; assumption:
  # ANPP = peak standing live biomass
  biom_shrubs$Sh.Amount.Live <- biom_shrubs$Sh.Amount.Live * Shrub_BiomassScaler
  biom_C3$C3.Amount.Live <- biom_C3$C3.Amount.Live * Grass_BiomassScaler
  biom_C4$C4.Amount.Live <- biom_C4$C4.Amount.Live * Grass_BiomassScaler
  biom_annuals$Annual.Amount.Live <-
    biom_annuals$Annual.Amount.Live * Grass_BiomassScaler

  #Scale litter amount by productivity and adjust for ratio of litter/live
  biom_shrubs$Sh.Litter <- biom_shrubs$Sh.Litter * Shrub_BiomassScaler *
    biom_maxs["Sh.Litter"] / biom_maxs["Sh.Amount.Live"]
  biom_C3$C3.Litter <- biom_C3$C3.Litter * Grass_BiomassScaler *
    biom_maxs["C3.Litter"] / biom_maxs["C3.Amount.Live"]
  biom_C4$C4.Litter <- biom_C4$C4.Litter * Grass_BiomassScaler *
    biom_maxs["C4.Litter"] / biom_maxs["C4.Amount.Live"]
  biom_annuals$Annual.Litter <- biom_annuals$Annual.Litter *
    Grass_BiomassScaler *
    biom_maxs["Annual.Litter"] / biom_maxs["Annual.Amount.Live"]

  #Guarantee that live fraction = ]0, 1]
  biom_shrubs$Sh.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_shrubs$Sh.Perc.Live))
  biom_C3$C3.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_C3$C3.Perc.Live))
  biom_C4$C4.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_C4$C4.Perc.Live))
  biom_annuals$Annual.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_annuals$Annual.Perc.Live))

  #Calculate total biomass based on scaled live biomass amount
  biom_shrubs$Sh.Biomass <- biom_shrubs$Sh.Amount.Live /
    biom_shrubs$Sh.Perc.Live
  biom_C3$C3.Biomass <- biom_C3$C3.Amount.Live / biom_C3$C3.Perc.Live
  biom_C4$C4.Biomass <- biom_C4$C4.Amount.Live / biom_C4$C4.Perc.Live
  biom_annuals$Annual.Biomass <- biom_annuals$Annual.Amount.Live /
    biom_annuals$Annual.Perc.Live

  list(biom_shrubs = biom_shrubs,
       biom_C3 = biom_C3,
       biom_C4 = biom_C4,
       biom_annuals = biom_annuals)
}


#' Adjust mean monthly biomass values of grass and shrub functional groups by
#' climate relationships
#'
#' @param tr_VegBiom A data.frame with 12 rows (one for each month) and columns
#'   \code{X.Biomass}, \code{X.Amount.Live}, \code{X.Perc.Live}, and
#'   \code{X.Litter} where \code{X} are for the functional groups shrubs,
#'   \code{X = Sh}; C3-grasses, \code{X = C3}; C4-grasses, \code{X = C4}; and
#'   annuals, \code{X = Annual} containing default input values.
#' @param do_adjBiom_by_temp A logical value. If \code{TRUE} then monthly
#'   phenology is adjusted by temperature.
#' @param do_adjBiom_by_ppt A logical value. If \code{TRUE} then monthly biomass
#'   is adjusted by precipitation.
#' @param fgrass_c3c4ann A numeric vector of length 3. Relative contribution
#'   [0-1] of the C3-grasses, C4-grasses, and annuals functional groups. The sum
#'   of \code{fgrass_c3c4ann} is 1.
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter of the
#'   location.
#' @inheritParams adjBiom_by_temp
#'
#' @section Default inputs: \itemize{
#'   \item Shrubs are based on location \var{\sQuote{IM_USC00107648_Reynolds}}
#'     which resulted in a vegetation composition of 70 \% shrubs and 30 \%
#'     C3-grasses. Default monthly biomass values were estimated for
#'     MAP = 450 mm yr-1.
#'   \item Grasses are based on location \var{\sQuote{GP_SGSLTER}}
#'     (shortgrass steppe) which resulted in 12 \% shrubs, 22 \% C3-grasses,
#'     and 66 \% C4-grasses. Default biomass values were estimated for
#'     MAP = 340 mm yr-1. }
#'
#' @return A list with two elements \code{grass}, \code{shrub}. Each element is
#'   a matrix with 12 rows (one for each month) and columns \code{Biomass},
#'   \code{Amount.Live}, \code{Perc.Live}, and \code{Litter}.
#'
#' @references Bradford, J.B., Schlaepfer, D.R., Lauenroth, W.K. & Burke, I.C.
#'   (2014). Shifts in plant functional types have time-dependent and regionally
#'   variable impacts on dryland ecosystem water balance. J Ecol, 102,
#'   1408-1418.
#'
#' @export
estimate_PotNatVeg_biomass <- function(tr_VegBiom,
  do_adjBiom_by_temp = FALSE, do_adjBiom_by_ppt = FALSE,
  fgrass_c3c4ann = c(1, 0, 0), growing_limit_C = 4, isNorth = TRUE,
  MAP_mm = 450,
  mean_monthly_temp_C = c(rep(growing_limit_C - 1, 2),
    rep(growing_limit_C + 1, 8),
    rep(growing_limit_C - 1, 2))) {

  # Default shrub biomass input is at MAP = 450 mm/yr, and default grass
  # biomass input is at MAP = 340 mm/yr

  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) # shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) # shrubs, C3, and C4

  #Calculate 'live biomass amount'
  tr_VegBiom$Sh.Amount.Live <- tr_VegBiom$Sh.Biomass * tr_VegBiom$Sh.Perc.Live
  tr_VegBiom$C3.Amount.Live <- tr_VegBiom$C3.Biomass * tr_VegBiom$C3.Perc.Live
  tr_VegBiom$C4.Amount.Live <- tr_VegBiom$C4.Biomass * tr_VegBiom$C4.Perc.Live
  tr_VegBiom$Annual.Amount.Live <- tr_VegBiom$Annual.Biomass *
    tr_VegBiom$Annual.Perc.Live

  # Scale monthly values of litter and live biomass amount by column-max;
  # total biomass will be back calculated
  itemp <- grepl("Litter", names(tr_VegBiom)) |
    grepl("Amount.Live", names(tr_VegBiom))
  colmax <- apply(tr_VegBiom[, itemp], MARGIN = 2, FUN = max)
  tr_VegBiom[, itemp] <- sweep(tr_VegBiom[, itemp], MARGIN = 2,
    STATS = colmax, FUN = "/")

  # Pull vegetation types
  x <- list()
  x[["biom_shrubs"]] <- tr_VegBiom[, grepl("Sh", names(tr_VegBiom))]
  x[["biom_C3"]] <- tr_VegBiom[, grepl("C3", names(tr_VegBiom))]
  x[["biom_C4"]] <- tr_VegBiom[, grepl("C4", names(tr_VegBiom))]
  x[["biom_annuals"]] <- tr_VegBiom[, grepl("Annual", names(tr_VegBiom))]

  # adjust phenology for mean monthly temperatures
  if (do_adjBiom_by_temp) {
    x <- adjBiom_by_temp(
      x = x,
      mean_monthly_temp_C = mean_monthly_temp_C,
      growing_limit_C = growing_limit_C,
      isNorth = isNorth
    )
  }

  # if (do_adjBiom_by_ppt) then adjust biomass amounts by productivity
  # relationship with MAP
  x <- adjBiom_by_ppt(
    biom_shrubs = x[["biom_shrubs"]],
    biom_C3 = x[["biom_C3"]],
    biom_C4 = x[["biom_C4"]],
    biom_annuals = x[["biom_annuals"]],
    biom_maxs = colmax,
    map_mm_shrubs = if (do_adjBiom_by_ppt) MAP_mm else StandardShrub_MAP_mm,
    map_mm_std_shrubs = StandardShrub_MAP_mm,
    map_mm_grasses = if (do_adjBiom_by_ppt) MAP_mm else StandardGrasses_MAP_mm,
    map_mm_std_grasses = StandardGrasses_MAP_mm,
    vegcomp_std_shrubs = StandardShrub_VegComposition,
    vegcomp_std_grass = StandardGrasses_VegComposition
  )

  biom_grasses <-
    x[["biom_C3"]] * fgrass_c3c4ann[1] +
    x[["biom_C4"]] * fgrass_c3c4ann[2] +
    x[["biom_annuals"]] * fgrass_c3c4ann[3]

  cn <- dimnames(biom_grasses)[[2]]
  cn <- sapply(strsplit(cn, split = ".", fixed = TRUE),
    function(x) paste0(x[-1], collapse = "."))
  dimnames(biom_grasses)[[2]] <- cn

  biom_shrubs <- x[["biom_shrubs"]]
  dimnames(biom_shrubs)[[2]] <- cn

  list(grass = as.matrix(biom_grasses),
       shrub = as.matrix(biom_shrubs))
}

#' Lookup transpiration coefficients
#'
#' Lookup transpiration coefficients for grasses, shrubs, and trees per soil
#' layer or per soil depth increment of 1 cm per distribution type for
#' each simulation run and copy values to \var{\sQuote{datafile.soils}}
#'
#' \itemize{
#'    \item first row of datafile is label for per soil layer
#'      \var{\dQuote{Layer}} or per soil depth increment of 1 cm
#'      \var{\dQuote{DepthCM}}
#'    \item second row of datafile is source of data
#'    \item the other rows contain the data for each distribution type = columns
#' }
#' @section Note: cannot write data from \var{\sQuote{sw_input_soils}} to
#'   \var{\sQuote{datafile.soils}}
#' @export
TranspCoeffByVegType <- function(tr_input_code, tr_input_coeff,
  soillayer_no, trco_type, layers_depth,
  adjustType = c("positive", "inverse", "allToLast")) {

  #extract data from table by category
  trco.code <- as.character(tr_input_code[,
    which(colnames(tr_input_code) == trco_type)])
  trco <- rep(0, times = soillayer_no)
  trco.raw <- na.omit(tr_input_coeff[,
    which(colnames(tr_input_coeff) == trco_type)])

  if (trco.code == "DepthCM") {
    temp <- sum(trco.raw, na.rm = TRUE)
    trco_sum <- if (temp == 0 || is.na(temp)) 1L else temp
    lup <- 1
    for (l in 1:soillayer_no) {
      llow <- as.numeric(layers_depth[l])
      if (is.na(llow) | lup > length(trco.raw)) {
        l <- l - 1
        break
      }
      trco[l] <- sum(trco.raw[lup:llow], na.rm = TRUE) / trco_sum
      lup <- llow + 1
    }
    usel <- l

  } else if (trco.code == "Layer") {
    usel <- if (length(trco.raw) < soillayer_no) {
        length(trco.raw)
      } else soillayer_no

    ltemp <- seq_len(usel)
    stemp <- sum(trco.raw[ltemp], na.rm = TRUE)
    temp <- if (stemp == 0 && is.na(stemp)) 1 else stemp
    trco[ltemp] <- trco.raw[ltemp] / temp
  }

  if (identical(adjustType, "positive")) {
    # equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
    trco <- trco / sum(trco)

  } else if (identical(adjustType, "inverse")) {
    irows <- 1:max(which(trco > 0))
    # equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
    trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)

  } else if (identical(adjustType, "allToLast")) {
    irow <- max(which(trco > 0))
    if (irow > 1) {
      # adding all the missing roots because soil is too shallow to the
      # deepest available layer
      trco[irow] <- 1 - sum(trco[1:(irow - 1)])
    } else {
      trco[1] <- 1
    }
  }

  trco
}

#' Replace selected biomass values of a
#' \link[rSOILWAT2:swProd-class]{rSOILWAT2::swProd} object
#'
#' @param fg A character string. One of the functional groups represented by
#'  \pkg{rSOILWAT2}
#' @param use A logical vector.
#'
#' @export
update_biomass <- function(fg = c("Grass", "Shrub", "Tree", "Forb"), use,
  prod_input, prod_default) {

  fg <- match.arg(fg)

  comps <- c("_Litter", "_Biomass", "_FractionLive", "_LAIconv")
  veg_ids <- lapply(comps, function(x)
    grep(paste0(fg, x), names(use)))
  veg_incl <- lapply(veg_ids, function(x) use[x])

  temp <- swProd_MonProd_veg(prod_default, fg)
  if (any(unlist(veg_incl))) {
    for (k in seq_along(comps)) if (any(veg_incl[[k]]))
      temp[veg_incl[[k]], k] <-
        as.numeric(prod_input[, veg_ids[[k]][veg_incl[[k]]]])
  }

  temp
}
