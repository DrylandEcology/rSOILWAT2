context("Pedotransfer functions: SWP <-> VWC")

# How the functions are applied in rSFSW2
# section: aggregation
#   - dailyRechargeExtremes
#     - case 1: SWPtoVWC(-0.033, texture$sand.top, texture$clay.top)
#   - dailySuitablePeriodsAvailableWater, dailySWPdrynessIntensity,
#     monthlySWAbulk
#     - case 1: SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top)
#   - mean doy: SWAbulk
#     - case 2: SWPtoVWC(index.SWPcrit, sand, clay)
#   - mean doy: SWPmatric
#     - case 3: VWCtoSWP(res.dailyMean[ir], textureDAgg$sand[al],
#               textureDAgg$clay[al]) # ir is vector; al is value
# section: functions
#   - get_SWPmatric_aggL
#     - case 3: VWCtoSWP(vwcmatric$top, texture$sand.top, texture$clay.top)
#     - case 3: VWCtoSWP(vwcmatric$aggMean.top, texture$sand.top,
#                 texture$clay.top)
#     - case 6: VWCtoSWP(vwcmatric$val[, -index.header], sand, clay)


# Inputs
# Table 2 from Cosby, B.J., Hornberger, G.M., Clapp, R.B. & Ginn, T.R. (1984).
# A statistical exploration of the relationships of soil moisture
# characteristics to the physical properties of soils. Water Resour Res, 20,
# 682-690.
texture <- data.frame(
  sand = c(0.92, 0.82, 0.58, 0.43, 0.17, 0.58, 0.32, 0.10, 0.52, 0.06, 0.22),
  clay = c(0.03, 0.06, 0.10, 0.18, 0.13, 0.27, 0.34, 0.34, 0.42, 0.47, 0.58)
)
row.names(texture) <- c("Sand", "Loamy sand", "Sandy loam", "Loam",
  "Silty loam", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay",
  "Silty clay", "Clay")

# Field capacity and agricultural permanent wilting point
swp_fix <- c(fc = -0.0333, pwp = -1.5) # MPa
vwc_fix <- data.frame(
  fc = c(0.103519295200457, 0.138084712513314, 0.210684319180335,
    0.276327910591054, 0.344767253784927, 0.259008902122202, 0.331526118930414,
    0.391036796958834, 0.292943352979446, 0.4058577839142, 0.368820489547312),
  pwp = c(0.0325953572147933, 0.05064269086372, 0.0903291990594713,
    0.143273427070284, 0.163171562436244, 0.152236773973314, 0.210032386550814,
    0.248623511289573, 0.196521033130402, 0.282030801991246, 0.269525768616734)
)
row.names(vwc_fix) <- row.names(texture)

ftemp <- file.path("..", "test_data", "swp_values.rds")
if (FALSE) {
  swp_vals <- unlist(lapply(row.names(texture), function(itext)
    VWCtoSWP(vwc_fix, texture[itext, "sand"], texture[itext, "clay"])))
  dim(swp_vals) <- c(nrow(vwc_fix), ncol(vwc_fix), nrow(texture))
  dimnames(swp_vals) <- list(row.names(texture), names(swp_fix),
    row.names(texture))
  saveRDS(swp_vals, file = ftemp)

} else {
  swp_vals <- readRDS(ftemp)
}

#--- Tests
test_that("To SWP", {
  # 1. VWC in fraction [single value] + sand and clay in fraction [single vals]
  #    --> SWP in MPa [single value]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(swp_fix[ifix],
      VWCtoSWP(vwc_fix[itext, ifix], texture[itext, "sand"],
        texture[itext, "clay"]))

  # 2. VWC in fraction [single value] + sand and clay in fraction
  #    [vectors of length d]
  #    --> SWP in MPa [vector of length d]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(swp_vals[itext, ifix, ],
      VWCtoSWP(vwc_fix[itext, ifix], texture[, "sand"], texture[, "clay"]))

  # 3. VWC in fraction [vector of length l] + sand and clay in fraction
  #    [single values]
  #    --> SWP in MPa [vector of length l]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(swp_vals[, ifix, itext],
      VWCtoSWP(vwc_fix[, ifix], texture[itext, "sand"], texture[itext, "clay"]))

  # 4. VWC in fraction [vector of length l] + sand and clay in fraction
  #    [vectors of length d]
  #    --> SWP in MPa [matrix with nrow = l and ncol = d, VWC vector repeated
  #        for each column]: probably not used
  for (ifix in names(swp_fix))
    expect_equivalent(swp_vals[, ifix, ],
      VWCtoSWP(vwc_fix[, ifix], texture[, "sand"], texture[, "clay"]))

  # 5. VWC in fraction [matrix with nrow = l and ncol = d] + sand and clay in
  #    fraction [single values]
  #    --> SWP in MPa [matrix with nrow = l and ncol = d]
  for (itext in row.names(texture))
    expect_equivalent(swp_vals[, , itext],
      VWCtoSWP(vwc_fix, texture[itext, "sand"], texture[itext, "clay"]))

  # 6. VWC in fraction [matrix with nrow = l and ncol = d] + sand and clay in
  #    fraction [vectors of length d]
  #    --> SWP in MPa [matrix with nrow = l and ncol = d, sand/clay vector
  #        repeated for each row]
  for (ifix in names(swp_fix)) {
    xin <- matrix(vwc_fix[, ifix], nrow = nrow(vwc_fix), ncol = nrow(texture),
      byrow = TRUE)
    xout <- matrix(swp_fix[ifix], nrow = nrow(vwc_fix), ncol = nrow(texture))
    expect_equivalent(xout,
      VWCtoSWP(xin, texture[, "sand"], texture[, "clay"]))
  }
})


test_that("To VWC", {
  # 1. SWP in MPa [single value] + sand and clay in fraction [single values]
  #    --> VWC in fraction [single value]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(vwc_fix[itext, ifix],
      SWPtoVWC(swp_fix[ifix], texture[itext, "sand"], texture[itext, "clay"]))

  # 2. SWP in MPa [single value] + sand and clay in fraction
  #    [vectors of length d]
  #    --> VWC in fraction [vector of length d]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(vwc_fix[, ifix],
      SWPtoVWC(swp_fix[ifix], texture[, "sand"], texture[, "clay"]))

  # 3. SWP in MPa [vector of length l] + sand and clay in fraction
  #    [single values]
  #    --> VWC in fraction [vector of length l]
  for (ifix in names(swp_fix)) for (itext in row.names(texture))
    expect_equivalent(
      SWPtoVWC(rep(swp_fix[ifix], nrow(texture)), texture[itext, "sand"],
        texture[itext, "clay"]),
      rep(vwc_fix[itext, ifix], nrow(texture)))

  # 4. SWP in MPa [vector of length l] + sand and clay in fraction
  #    [vectors of length d]
  #    --> VWC in fraction [matrix with nrow = l and ncol = d, SWP vector
  #        repeated for each column]: probably not used

  # 5. SWP in MPa [matrix with nrow = l and ncol = d] + sand and clay in
  #    fraction [single values]
  #    --> VWC in fraction [matrix with nrow = l and ncol = d]

  # 6. SWP in MPa [matrix with nrow = l and ncol = d] + sand and clay in
  #    fraction [vectors of length d]
  #    --> VWC in fraction [matrix with nrow = l and ncol = d, sand/clay vector
  #        repeated for each row]

})
