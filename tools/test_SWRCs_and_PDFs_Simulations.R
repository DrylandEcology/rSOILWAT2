#--- Compare SOILWAT2 simulations under varying SWRCs and PTFs ------

# This script runs SOILWAT2 simulations (using the package default dataset)
#   * for each implemented pair of SWRC/PTF
#   * for default and 50% reduced precipitation events
#
# and produced time-series plots by soil layer and by year of
# volumetric water content (VWC), soil water potential (SWP), and transpiration


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


#--- Settings ------
list_plot_vars <- list(
  list(slot = "SWPMATRIC", var = "SWP [MPa]", trans = function(x) -0.1 * x),
  list(slot = "VWCBULK", var = "VWC [cm / cm]", trans = function(x) x),
  list(slot = "TRANSP", var = "Transpiration [mm]", trans = function(x) 10 * x)
)

nsoils_used <- Inf
nyears_used <- 10
fadj_ppts <- c(0.5, 1)

soiltxtcls <- data.frame(
  sand_frac = c(
    0.92, 0.82, 0.58, 0.43, 0.17, 0.58, 0.32, 0.1, 0.52, 0.06, 0.22
  ),
  clay_frac = c(
    0.03, 0.06, 0.1, 0.18, 0.13, 0.27, 0.34, 0.34, 0.42, 0.47, 0.58
  ),
  `bulkDensity_g/cm^3` = c(
    1.614, 1.482, 1.520, 1.246, 1.464, 1.700, 1.143, 1.384, 1.26, 1.437, 1.277
  ),
  gravel_content = 0,
  check.names = FALSE
)
rownames(soiltxtcls) <- gsub(
  " ",
  "-",
  c(
    "Sand", "Loamy sand", "Sandy loam", "Loam", "Silty loam", "Sandy clay loam",
    "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"
  )
)


#--- Loop over precipitation adjustments ------
for (k0a in seq_along(fadj_ppts)) {

  #--- Loop over soil texture classes ------
  for (k0b in seq_len(nrow(soiltxtcls))) {

    #--- Simulate ------
    soils <- rSOILWAT2::swSoils_Layers(rSOILWAT2::sw_exampleData)
    nsoils <- nrow(soils)
    nsoils_used2 <- min(nsoils_used, nsoils)
    for (k0bi in seq_len(ncol(soiltxtcls))) {
      soils[, colnames(soiltxtcls)[k0bi]] <- soiltxtcls[k0b, k0bi]
    }

    year_start <- rSOILWAT2::swYears_StartYear(rSOILWAT2::sw_exampleData)

    swout <- lapply(
      list_swrcs_ptfs,
      function(sp) {
        sw_in <- rSOILWAT2::sw_exampleData
        rSOILWAT2::swSoils_Layers(sw_in) <- soils
        rSOILWAT2::swWeather_MonScalingParams(sw_in)[, "PPT"] <- fadj_ppts[k0a]
        rSOILWAT2::swSite_SWRCflags(sw_in) <- sp
        rSOILWAT2::swSoils_omSWRCp(sw_input) <-
          rSOILWAT2::sw2_list_omSWRCp[[sp[[1L]]]]
        rSOILWAT2::sw_exec(inputData = sw_in)
      }
    )


    #--- Create figures ------
    for (k1 in seq_along(list_plot_vars)) {

      fname_fig <- file.path(
        ".",
        paste0(
          "Fig_SOILWAT2_SWRCs-PTFs_Simulation-Run_",
          "_Soil-", rownames(soiltxtcls)[k0b],
          "_PPT", round(100 * fadj_ppts[k0a]), "pct",
          "__response-", list_plot_vars[[k1]][["slot"]],
          ".pdf"
        )
      )

      if (file.exists(fname_fig)) next

      #--- * Prepare data ------
      tmp_swout <- mapply(
        function(x, sp) {
          tmp <- slot(slot(x, list_plot_vars[[k1]][["slot"]]), "Day")
          res <- data.frame(
            SWRC = sp[1],
            PTF = sp[2],
            tmp[, c("Year", "Day")],
            date = as.Date(
              paste(tmp[, "Year"], tmp[, "Day"], sep = "-"),
              format = "%Y-%j"
            ),
            list_plot_vars[[k1]][["trans"]](tmp[, 2 + seq_len(nsoils)])
          )
          colnames(res) <- gsub("transp_total_", "", colnames(res))
          res
        },
        swout,
        list_swrcs_ptfs,
        SIMPLIFY = FALSE
      )

      var_swout <- reshape(
        do.call(rbind, tmp_swout),
        direction = "long",
        varying = grep("Lyr_", colnames(tmp_swout[[1]]), value = TRUE),
        sep = "_"
      )

      # rename "Lyr" to variable name
      colnames(var_swout)[grep("Lyr", colnames(var_swout))] <-
        list_plot_vars[[k1]][["var"]]

      # create combined SWRC-PTF name (for coloration)
      var_swout[, "SWRC-PTF"] <- paste(
        var_swout[, "SWRC"],
        var_swout[, "PTF"],
        sep = "-"
      )

      # add soil layer depths
      var_swout[, "Layer"] <- factor(
        var_swout[, "time"],
        levels = seq_len(nsoils),
        labels = paste0(
          c(0, soils[-nsoils, "depth_cm"]), "-",
          soils[, "depth_cm"],
          " cm"
        )
      )

      #--- Subset data to requested years and soil layers
      ids <-
        var_swout$Year %in% (year_start + seq_len(nyears_used) - 1) &
        var_swout$time %in% seq_len(nsoils_used2)
      var_swout_used <- var_swout[ids, , drop = FALSE]


      #--- * Create plot ------
      tmp <- ggplot2::ggplot(var_swout_used) +
        ggplot2::geom_line(
          ggplot2::aes(
            x = Day,
            y = .data[[list_plot_vars[[k1]][["var"]]]],
            color = `SWRC-PTF`,
            linetype = `SWRC-PTF`
          )
        ) +
        ggplot2::facet_wrap(
          ggplot2::vars(Layer, Year),
          nrow = nsoils_used2,
          #scales = "free_y",
          labeller = ggplot2::label_wrap_gen(multi_line = FALSE)
        ) +
        egg::theme_article()


      #--- Write to file
      pdf(
        file = fname_fig,
        height = 2.5 * nsoils_used2,
        width = 3 * (nyears_used + 1)
      )
      plot(tmp)
      dev.off()
    }
  }
}
