#--- Plot and compare Shape of Soil Water Retention Curves ------

# This script builds theoretical theta-phi (and phi-theta) relationships (SWRCs)
#   * for each implemented pair of SWRC/PTF
#   * for a variety of soil textures
#
# and produced plots:
# (i) to compare curves among soil textures within a SWRC
# (ii) to compare curves among SWRCs within each soil texture


# Required packages not part of `rSOILWAT2`
stopifnot(requireNamespace("ggplot2"))



#--- List of (available) SWRC-PTF combinations ------
list_swrcs_ptfs <- unname(as.list(as.data.frame(t(
  rSOILWAT2::list_matched_swrcs_ptfs()
))))

tmp <- check_ptf_availability(sapply(list_swrcs_ptfs, `[`, j = 2))
list_swrcs_ptfs <- list_swrcs_ptfs[tmp]

if (!all(tmp)) {
  message(
    "Unavailable PTFs are skipped: ",
    toString(shQuote(names(tmp)[!tmp]))
  )
}



#--- Inputs ------
thetas <- seq(0.00, 0.55, by = 0.001)
phis <- unique(sort(c(
  seq(-1000, -4, by = 1),
  seq(-4, -1, by = 0.1),
  seq(-1, -0.01, by = 0.01),
  -0.033,
  seq(-0.01, 0, by = 0.001),
  seq(-0.001, 0, by = 0.0001)
)))

soiltxtcls <- data.frame(
  sand_frac = c(
    0.92, 0.82, 0.58, 0.43, 0.17, 0.58, 0.32, 0.1, 0.52, 0.06, 0.22
  ),
  clay_frac = c(
    0.03, 0.06, 0.1, 0.18, 0.13, 0.27, 0.34, 0.34, 0.42, 0.47, 0.58
  ),
  bd = c(
    1.614, 1.482, 1.520, 1.246, 1.464, 1.700, 1.143, 1.384, 1.26, 1.437, 1.277
  ),
  fcoarse = 0
)
rownames(soiltxtcls) <- gsub(
  " ",
  ".",
  c(
    "Sand", "Loamy sand", "Sandy loam", "Loam", "Silty loam", "Sandy clay loam",
    "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"
  )
)


tag_soils <- paste0("soil__", rownames(soiltxtcls))


#--- Estimate SWRCp ------
swrcps <- lapply(
  list_swrcs_ptfs,
  function(sp){
    rSOILWAT2::ptf_estimate(
      sand = soiltxtcls[, "sand_frac"],
      clay = soiltxtcls[, "clay_frac"],
      bdensity = soiltxtcls[, "bd"],
      fcoarse = soiltxtcls[, "fcoarse"],
      swrc_name = sp[1],
      ptf_name = sp[2]
    )
  }
)


#--- Calculate phi ------
phi_sim <- mapply(
  function(sp, ps) {
    rSOILWAT2::swrc_vwc_to_swp(
      thetas,
      swrc = list(name = sp[1], swrcp = ps)
    )
  },
  list_swrcs_ptfs,
  swrcps,
  SIMPLIFY = FALSE
)



#--- * Prepare calculated phi ------
tmp <- mapply(
  function(sp, theta, phi) {
    data.frame(
      SWRC = sp[1],
      PTF = sp[2],
      `SWRC-PTF` = paste(sp, collapse = "-"),
      theta = theta,
      {
        colnames(phi) <- tag_soils
        phi
      }
    )
  },
  list_swrcs_ptfs,
  lapply(seq_along(list_swrcs_ptfs), function(k) thetas),
  phi_sim,
  SIMPLIFY = FALSE
)

x_phi <- reshape(
  do.call(rbind, tmp),
  direction = "long",
  varying = tag_soils,
  sep = "__"
)

# beautify
x_phi[, "time"] <- gsub(".", " ", x_phi[, "time"], fixed = TRUE)
colnames(x_phi)[colnames(x_phi) == "time"] <- "soil texture"
colnames(x_phi)[colnames(x_phi) == "soil"] <- "phi"
colnames(x_phi)[colnames(x_phi) == "SWRC.PTF"] <- "SWRC-PTF"




#--- * Figure of phi: compare curves among soil textures within a SWRC ------
fname1 <- file.path("Fig_SOILWAT2_SWRCs-PTFs_Curves-phi_SoilTextures.pdf")

if (!file.exists(fname1)) {
  tmp <- ggplot2::ggplot(x_phi) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = theta,
        y = -phi,
        color = `soil texture`,
        linetype = `soil texture`
      )
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.033, 1.5, 30),
      color = "gray",
      linetype = "dotted"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(`SWRC-PTF`)) +
    ggplot2::scale_y_log10(limits = c(1e-4, 1e3)) +
    ggplot2::xlab(Volumetric~Water~Content~~(cm^3/cm^3)) +
    ggplot2::ylab(Matric~~Potential~~(-MPa)) +
    egg::theme_article()


  #--- Write to file
  npanels <- apply(
    unique(ggplot2::ggplot_build(tmp)$layout$layout[, c("ROW", "COL")]),
    2,
    max
  )

  pdf(
    file = fname1,
    height = 4 * npanels[1],
    width = 5 * (npanels[2] + 0.5)
  )
  plot(tmp)
  dev.off()
}



#--- * Figure of phi: compare curves among SWRCs within each soil texture ------
fname2 <- file.path(".", "Fig_SOILWAT2_SWRCs-PTFs_Curves-phi_SWRCs.pdf")

if (!file.exists(fname2)) {
  tmp <- ggplot2::ggplot(x_phi) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = theta,
        y = -phi,
        color = `SWRC-PTF`,
        linetype = `SWRC-PTF`
      )
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.033, 1.5, 30),
      color = "gray",
      linetype = "dotted"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(`soil texture`)) +
    ggplot2::scale_y_log10(limits = c(1e-4, 1e3)) +
    ggplot2::xlab(Volumetric~Water~Content~~(cm^3/cm^3)) +
    ggplot2::ylab(Matric~~Potential~~(-MPa)) +
    egg::theme_article()


  #--- Write to file
  npanels <- apply(
    unique(ggplot2::ggplot_build(tmp)$layout$layout[, c("ROW", "COL")]),
    2,
    max
  )

  pdf(
    file = fname2,
    height = 3 * npanels[1],
    width = 4 * (npanels[2] + 0.5)
  )
  plot(tmp)
  dev.off()
}




#--- Calculate theta ------
theta_sim <- mapply(
  function(sp, ps) {
    rSOILWAT2::swrc_swp_to_vwc(
      phis,
      swrc = list(name = sp[1], swrcp = ps)
    )
  },
  list_swrcs_ptfs,
  swrcps,
  SIMPLIFY = FALSE
)



#--- * Prepare calculated theta ------
tmp <- mapply(
  function(sp, theta, phi) {
    data.frame(
      SWRC = sp[1],
      PTF = sp[2],
      `SWRC-PTF` = paste(sp, collapse = "-"),
      phi = phi,
      {
        colnames(theta) <- tag_soils
        theta
      }
    )
  },
  list_swrcs_ptfs,
  theta_sim,
  lapply(seq_along(list_swrcs_ptfs), function(k) phis),
  SIMPLIFY = FALSE
)

x_theta <- reshape(
  do.call(rbind, tmp),
  direction = "long",
  varying = tag_soils,
  sep = "__"
)

# beautify
x_theta[, "time"] <- gsub(".", " ", x_theta[, "time"], fixed = TRUE)
colnames(x_theta)[colnames(x_theta) == "time"] <- "soil texture"
colnames(x_theta)[colnames(x_theta) == "soil"] <- "theta"
colnames(x_theta)[colnames(x_theta) == "SWRC.PTF"] <- "SWRC-PTF"




#--- * Figure of theta: compare curves among soil textures within a SWRC ------
fname1 <- file.path("Fig_SOILWAT2_SWRCs-PTFs_Curves-theta_SoilTextures.pdf")

if (!file.exists(fname1)) {
  tmp <- ggplot2::ggplot(x_theta) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = -phi,
        y = theta,
        color = `soil texture`,
        linetype = `soil texture`
      )
    ) +
    ggplot2::geom_vline(
      xintercept = c(0.033, 1.5, 30),
      color = "gray",
      linetype = "dotted"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(`SWRC-PTF`)) +
    ggplot2::scale_x_log10(limits = c(1e-4, 1e3)) +
    ggplot2::xlab(Matric~~Potential~~(-MPa)) +
    ggplot2::ylab(Volumetric~Water~Content~~(cm^3/cm^3)) +
    egg::theme_article()


  #--- Write to file
  npanels <- apply(
    unique(ggplot2::ggplot_build(tmp)$layout$layout[, c("ROW", "COL")]),
    2,
    max
  )

  pdf(
    file = fname1,
    height = 4 * npanels[1],
    width = 5 * (npanels[2] + 0.5)
  )
  plot(tmp)
  dev.off()
}



#--- * Figure of theta: compare curves among SWRCs within each soil texture ------
fname2 <- file.path(".", "Fig_SOILWAT2_SWRCs-PTFs_Curves-theta_SWRCs.pdf")

if (!file.exists(fname2)) {
  tmp <- ggplot2::ggplot(x_theta) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = -phi,
        y = theta,
        color = `SWRC-PTF`,
        linetype = `SWRC-PTF`
      )
    ) +
    ggplot2::geom_vline(
      xintercept = c(0.033, 1.5, 30),
      color = "gray",
      linetype = "dotted"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(`soil texture`)) +
    ggplot2::scale_x_log10(limits = c(1e-4, 1e3)) +
    ggplot2::xlab(Matric~~Potential~~(-MPa)) +
    ggplot2::ylab(Volumetric~Water~Content~~(cm^3/cm^3)) +
    egg::theme_article()


  #--- Write to file
  npanels <- apply(
    unique(ggplot2::ggplot_build(tmp)$layout$layout[, c("ROW", "COL")]),
    2,
    max
  )

  pdf(
    file = fname2,
    height = 3 * npanels[1],
    width = 4 * (npanels[2] + 0.5)
  )
  plot(tmp)
  dev.off()
}
