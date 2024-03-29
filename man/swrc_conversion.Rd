% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_Pedotransfer_Functions.R
\name{swrc_conversion}
\alias{swrc_conversion}
\alias{swrc_swp_to_vwc}
\alias{swrc_vwc_to_swp}
\title{Conversion between bulk soil water content and soil water potential}
\usage{
swrc_conversion(
  direction = c("swp_to_vwc", "vwc_to_swp"),
  x,
  fcoarse,
  layer_width,
  swrc,
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
)

swrc_swp_to_vwc(
  swp_MPa,
  fcoarse,
  layer_width,
  swrc = list(swrc_name = NULL, ptf_name = NULL, swrcp = NULL),
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
)

swrc_vwc_to_swp(
  vwcBulk,
  fcoarse,
  layer_width,
  swrc = list(swrc_name = NULL, ptf_name = NULL, swrcp = NULL),
  sand = NULL,
  clay = NULL,
  bdensity = NULL,
  outer_if_equalsize = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{direction}{A character string. Indicates the direction of
soil water conversion.}

\item{x}{A numeric value, vector, or matrix.
The soil water values to be converted,
either soil water potential (units \verb{[MPa]}) of the soil matric component or
bulk volumetric water content (units \verb{[cm/cm]}).}

\item{fcoarse}{A numeric value or vector.
Coarse fragments, e.g., gravel, (> 2 mm; units of \verb{[m3/m3]})
relative to the whole soil of each soil layer.
\code{fcoarse} is required, for instance, to translate between
values relative to the matric soil component (< 2 mm fraction) and
relative to the whole soil (matric soil plus coarse fragments).}

\item{layer_width}{A numeric value or vector.
Depth interval, width, of each soil layer (units of \code{cm}).
\code{layer_width} is required to translate between
soil water content of a soil layer and volumetric water content.}

\item{swrc}{A named list.
Contains all necessary elements of a \code{SWRC},
i.e., \code{name} (short for \code{swrc_name}) and \code{swrcp},
or all necessary elements to estimate parameters of a \code{SWRC} given
soil parameters, i.e., \code{swrc_name} and \code{ptf_name}.}

\item{sand}{A numeric value or vector.
Sand content of the matric soil component
(< 2 mm fraction; units of \verb{[g/g]}) of each soil layer.}

\item{clay}{A numeric value or vector.
Clay content of the matric soil component
(< 2 mm fraction; units of \verb{[g/g]}) of each soil layer.}

\item{bdensity}{A numeric value or vector.
Density of the whole soil
(matric soil plus coarse fragments; units \verb{[g/cm3]}).}

\item{outer_if_equalsize}{A logical value.
Relevant only if \code{x} of length \code{l} and soils of length \code{d} are equal.
If \code{TRUE}, then the returned object has a size of \verb{l x d} = \verb{l x l}
where the \code{d} sets of soil values are repeated for each value of \code{x}.
If \code{FALSE} (default), then the returned object has a size of \code{l} = \code{d}
where the the \code{SWRC} conversion is applied to the
first element of \code{x} and soils, the second elements, and so on.}

\item{verbose}{A logical value. If \code{TRUE}, then display
\code{SOILWAT2} internal warnings and other messages.}

\item{swp_MPa}{A numeric object. The soil water potential values
(units \verb{[MPa]}) of the soil matric component to be converted to
bulk volumetric water content
(i.e., relative to the whole soil; units \verb{[cm/cm]}).}

\item{vwcBulk}{A numeric object. The volumetric water content values
(relative to the whole soil; units \verb{[cm/cm]})
to be converted to soil water potential (units \verb{[MPa]})
of the soil matric component.}
}
\value{
The dimensions of the output are a function of \code{x} and the
number of soil values (e.g., rows or length of \code{swrc[["swrcp"]]}).
The returned object has:
\itemize{
\item length \code{l} if both \code{x} and soils are of length \code{l}.
\item length \code{l} if \code{x} has length \code{l} and there is one soil.
\item length \code{d} if \code{x} is one value and soils are of length \code{d}.
\item size \verb{l x d} if \code{x} has length \code{l} and soils are of length \code{d}
(if \code{l} and \code{d} are not equal or \code{outer_if_equalsize} is \code{TRUE};
cf. the first case);
the \code{d} sets of soil values are repeated for each value of \code{x}.
\item size \verb{l x d} if \code{x} has size \verb{l x d} and there is one soil.
the soil is repeated for each value of \code{x}.
\item size \verb{l x d} if \code{x} has size \verb{l x d} and soils are of length \code{d}
the \code{d} sets of soil values are repeated for each row of \code{x}.
}
}
\description{
Conversion between bulk soil water content and soil water potential
}
\section{Functions}{
\itemize{
\item \code{swrc_swp_to_vwc()}: Convenience wrapper
to convert from \code{SWP} to bulk \code{VWC} with selected \code{SWRC}

\item \code{swrc_vwc_to_swp()}: Convenience wrapper
to convert from bulk \code{VWC} to matric \code{SWP} with selected \code{SWRC}

}}
\section{Details}{

\code{\link[=swrc_names]{swrc_names()}} lists implemented \code{SWRCs};
\code{\link[=ptf_names]{ptf_names()}} lists implemented \code{PTFs}; and
\code{\link[=check_ptf_availability]{check_ptf_availability()}} checks availability of \code{PTFs}.


For backward compatibility, \code{fcoarse} and \code{layer_width} may be missing.
If they are missing, then the soils are assumed to contain
\verb{0\%} coarse fragments and be represented by \verb{1 cm} wide soil layers.


Arguments \code{sand}, \code{clay}, and \code{bdensity} are only required
if \code{SWRC} parameter values need to be estimated on the fly,
i.e., if \code{swrc} does not contain the element \code{swrcp}
(with suitable \code{SWRC} parameter values).
This is handled by \code{\link[=ptf_estimate]{ptf_estimate()}} and additionally requires
the element \code{ptf_name} for argument \code{swrc}.


If \code{swrc} contains element \code{swrcp} with one set of \code{SWRC} parameters,
i.e., one row, then the parameter set is repeated for each value of \code{x}.


If \code{vwc} inputs represent the matric component
(instead of expected bulk values), then set \code{fcoarse} to 0.
This works, however, only if \code{swrcp} are provided or \code{fcoarse} is not
utilized by the requested \code{PTF}.
}

\examples{
fsand <- c(0.5, 0.3)
fclay <- c(0.2, 0.1)
fcrs1 <- c(0, 0)
fcrs2 <- c(0.4, 0.1)

swrc1 <- list(
  name = "Campbell1974",
  swrcp = ptf_estimate(
    sand = fsand,
    clay = fclay,
    fcoarse = fcrs1,
    swrc_name = "Campbell1974",
    ptf_name = "Cosby1984"
  )
)
swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, swrc = swrc1)
swrc_swp_to_vwc(c(-1.5, NA), fcoarse = fcrs1, swrc = swrc1)
swrc_swp_to_vwc(-1.5, fcoarse = fcrs1, sand = fsand, clay = fclay)
swrc_vwc_to_swp(c(0.10, 0.15, 0.20), fcoarse = fcrs1, swrc = swrc1)
swrc_vwc_to_swp(c(0.10, NA, 0.20), fcoarse = fcrs1, swrc = swrc1)

swrc2 <- list(
  name = "Campbell1974",
  swrcp = ptf_estimate(
    sand = fsand,
    clay = fclay,
    fcoarse = fcrs2,
    swrc_name = "Campbell1974",
    ptf_name = "Cosby1984"
  )
)
swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, swrc = swrc2)
(1 - fcrs2) * swrc_swp_to_vwc(-1.5, swrc = swrc2)
swrc_swp_to_vwc(-1.5, fcoarse = fcrs2, sand = fsand, clay = fclay)
swrc_vwc_to_swp(c(0.10, 0.15, 0.20), fcoarse = fcrs2, swrc = swrc2)


# Available water holding capacity "AWC"
soils <- swSoils_Layers(rSOILWAT2::sw_exampleData)
p <- ptf_estimate(
  sand = soils[, "sand_frac"],
  clay = soils[, "clay_frac"],
  fcoarse = soils[, "gravel_content"]
)
tmp <- swrc_swp_to_vwc(
  c(-1.5, -0.033),
  fcoarse = soils[, "gravel_content"],
  swrc = list(name = "Campbell1974", swrcp = p)
)
awc <- diff(c(0, soils[, "depth_cm"])) * as.vector(diff(tmp))


# Shape of SWRCs
theta <- seq(0.05, 0.55, by = 0.001)
soils <- data.frame(
  sand_frac = c(sand = 0.92, silty_loam = 0.17, silty_clay = 0.06),
  clay_frac = c(0.03, 0.13, 0.58),
  bd = c(1.614, 1.464, 1.437)
)
phi <- list(
  Campbell1974 = swrc_vwc_to_swp(
    theta,
    sand = soils[, "sand_frac"],
    clay = soils[, "clay_frac"],
    swrc = list(swrc_name = "Campbell1974", ptf_name = "Cosby1984")
  )
)

if (check_ptf_availability("Rosetta3")) {
  phi[["vanGenuchten1980"]] <- swrc_vwc_to_swp(
    theta,
    sand = soils[, "sand_frac"],
    clay = soils[, "clay_frac"],
    bdensity = soils[, "bd"],
    swrc = list(swrc_name = "vanGenuchten1980", ptf_name = "Rosetta3")
  )
}

# Use PTF "neuroFX2021" to estimate parameters of SWRC `FXW`
\dontrun{
# Set neuroFX2021 file path, see details in `ptf_neuroFX2021_for_FXW()`
options(RSW2_FILENEUROFX2021 = "path/to/sscbd.RData")
}

if (check_ptf_availability("neuroFX2021")) {
  phi[["FXW"]] <- swrc_vwc_to_swp(
    theta,
    sand = soils[, "sand_frac"],
    clay = soils[, "clay_frac"],
    bdensity = soils[, "bd"],
    swrc = list(swrc_name = "FXW", ptf_name = "neuroFX2021")
  )
}

if (interactive() && requireNamespace("graphics")) {
  par_prev <- graphics::par(mfcol = c(length(phi), 1))

  for (k in seq_along(phi)) {
    graphics::matplot(
      theta, -phi[[k]],
      type = "l",
      log = "y",
      xlim = c(0, max(theta)),
      xlab = "theta [m/m]",
      ylim = c(1e-4, 1e6),
      ylab = "-phi [MPa]",
      main = paste0("Soil Water Retention Curve (", names(phi)[k], ")")
    )
    graphics::abline(h = -c(-1.5, -0.033), col = "gray", lty = 3)
    graphics::legend("topright", rownames(soils), col = 1:3, lty = 1:3)
  }

  graphics::par(par_prev)
}


}
\references{
Cosby, B. J., G. M. Hornberger, R. B. Clapp, & T. R. Ginn. 1984.
A statistical exploration of the relationships of soil moisture
characteristics to the physical properties of soils.
Water Resources Research, 20:682-690, \doi{10.1029/WR020i006p00682}
}
\seealso{
\code{\link[=ptf_estimate]{ptf_estimate()}},
\code{\link[=check_swrcp]{check_swrcp()}},
\code{\link[=check_ptf_availability]{check_ptf_availability()}}
}
