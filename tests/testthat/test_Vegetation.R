context("Vegetation functions")

# Inputs
data("weatherData", package = "rSOILWAT2")
clim <- calc_SiteClimate(weatherList = weatherData,
  year.start = 1949, year.end = 2010,
  do_C4vars = FALSE, simTime2 = NULL)

# Tests
test_that("Vegetation: estimate land cover composition", {

  # Each of the returned vectors sums to 1 and each element is finite and >= 0
  expect_pnv <- function(pnv) {
    for (k in seq_along(pnv)) {
      expect_equal(sum(pnv[[k]]), 1)
      expect_true(all(is.finite(pnv[[k]])))
      expect_true(all(pnv[[k]] >= 0))
    }
  }

  # Indices copied from function `estimate_PotNatVeg_composition`
  isuc <- 1 # succulents
  ifor <- 2 # forbs
  igc3 <- 3 # grasses-C3
  igc4 <- 4 # grasses-C4
  igan <- 5 # grasses-annuals
  ishr <- 6 # shrubs
  itre <- 7 # trees
  ibar <- 8 # bare-ground
  iset <- c(igan, itre, ibar)
  iestim <- c(igc4, igc3, ishr, ifor, isuc)
  igrasses <- c(igc3, igc4, igan)

  #--- All estimable vegetation types are estimated:
  pnv0_expected <- list(
    Rel_Abundance_L0 =
      c(
        Succulents = 0,
        Forbs = 0.130129368278067,
        Grasses_C3 = 0.315462487535754,
        Grasses_C4 = 0,
        Grasses_Annuals = 0,
        Shrubs = 0.554408144186179,
        Trees = 0,
        BareGround = 0
      ),
    Rel_Abundance_L1 = c(
      SW_TREES = 0,
      SW_SHRUB = 0.554408144186179,
      SW_FORBS = 0.130129368278067,
      SW_GRASS = 0.315462487535754,
      SW_BAREGROUND = 0
    ),
    Grasses = c(
      Grasses_C3 = 1,
      Grasses_C4 = 0,
      Grasses_Annuals = 0
    )
  )

  expect_silent(
    pnv <- estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]], MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]])
    )

  expect_pnv(pnv)
  expect_equal(pnv, pnv0_expected)

  # The set land cover types are 0
  for (k in iset) {
    expect_equal(sum(pnv[["Rel_Abundance_L0"]][[k]]), 0)
  }


  #--- Some land cover types are fixed and others are estimated:
  Shrubs_Fraction <- 0.5
  BareGround_Fraction <- 0.25

  expect_silent(
    pnv <- estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]], MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      fix_shrubs = TRUE, Shrubs_Fraction = Shrubs_Fraction,
      fix_BareGround = TRUE, BareGround_Fraction = BareGround_Fraction)
  )

  expect_pnv(pnv)

  # The fixed types retained their input values
  expect_equal(sum(pnv[["Rel_Abundance_L0"]][[ishr]]), Shrubs_Fraction)
  expect_equal(sum(pnv[["Rel_Abundance_L0"]][[ibar]]), BareGround_Fraction)


  #--- The function `estimate_PotNatVeg_composition` can fail under a few
  # situations:
  # (i) fixed sum is more than 1
  expect_error(
    estimate_PotNatVeg_composition(
      MAP_mm = 0, MAT_C = 0,
      mean_monthly_ppt_mm = rep(0, 12),
      mean_monthly_Temp_C = rep(0, 12),
      fix_succulents = TRUE, Succulents_Fraction = 10)
  )

  # (ii) all fixed but sum is less than 1 and !fill_empty_with_BareGround
  expect_error(
    estimate_PotNatVeg_composition(
      MAP_mm = 0, MAT_C = 0,
      mean_monthly_ppt_mm = rep(0, 12),
      mean_monthly_Temp_C = rep(0, 12),
      fix_succulents = TRUE, Succulents_Fraction = 0,
      fix_annuals = TRUE, Annuals_Fraction = 0,
      fix_C4grasses = TRUE, C4_Fraction = 0,
      fix_C3grasses = TRUE, C3_Fraction = 0,
      fix_shrubs = TRUE, Shrubs_Fraction = 0,
      fix_forbs = TRUE, Forbs_Fraction = 0,
      fix_trees = TRUE, Trees_Fraction = 0,
      fix_BareGround = TRUE, BareGround_Fraction = 0,
      fill_empty_with_BareGround = FALSE)
  )

  # (iii) cover to estimate is 0, fixed types are less than 1, and bare-ground
  # is fixed
  expect_error(
    estimate_PotNatVeg_composition(
      MAP_mm = 900, MAT_C = -10,
      mean_monthly_ppt_mm = c(0, 0, rep(100, 9), 0),
      mean_monthly_Temp_C = rep(-10, 12),
      fill_empty_with_BareGround = FALSE)
  )

  # The last errors are avoided if `fill_empty_with_BareGround = TRUE`
  # and bare-ground is not fixed; we get 100% bare-ground cover
  expect_silent(
    pnv <- estimate_PotNatVeg_composition(
      MAP_mm = 0, MAT_C = 0,
      mean_monthly_ppt_mm = rep(0, 12),
      mean_monthly_Temp_C = rep(0, 12),
      fix_succulents = TRUE, Succulents_Fraction = 0,
      fix_annuals = TRUE, Annuals_Fraction = 0,
      fix_C4grasses = TRUE, C4_Fraction = 0,
      fix_C3grasses = TRUE, C3_Fraction = 0,
      fix_shrubs = TRUE, Shrubs_Fraction = 0,
      fix_forbs = TRUE, Forbs_Fraction = 0,
      fix_trees = TRUE, Trees_Fraction = 0,
      fix_BareGround = FALSE, fill_empty_with_BareGround = TRUE)
  )

  expect_pnv(pnv[1:2])
  expect_equivalent(pnv[["Rel_Abundance_L0"]][ibar], 1)

  expect_silent(
    pnv <- estimate_PotNatVeg_composition(
      MAP_mm = 900, MAT_C = -10,
      mean_monthly_ppt_mm = c(0, 0, rep(100, 9), 0),
      mean_monthly_Temp_C = rep(-10, 12),
      fix_BareGround = FALSE, fill_empty_with_BareGround = TRUE)
  )

  expect_pnv(pnv[1:2])
  expect_equivalent(pnv[["Rel_Abundance_L0"]][ibar], 1)
})
