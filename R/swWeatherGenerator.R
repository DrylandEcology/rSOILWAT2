###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


#' Estimate coefficients for use by \var{SOILWAT2} weather generator
#'
#' Estimates coefficients for the two site-specific files
#' \var{mkv_covar.in} and \var{mkv_prob.in} required by the first-order
#' Markov weather generator in \var{SOILWAT2} \var{> v4.2.5}.
#'
#' @section Notes: This code is a complete overhaul compared to the version
#'   from \var{rSFSTEP2} on \code{2019-Feb-10}
#'   \url{https://github.com/DrylandEcology/rSFSTEP2/commit/cd9e161971136e1e56d427a4f76062bbb0f3d03a}
#'
#' @param weatherData A list of elements of class
#'   \code{\linkS4class{swWeatherData}} or a \code{data.frame} as returned by
#'   \code{\link{dbW_weatherData_to_dataframe}}.
#' @param WET_limit_cm A numeric value. A day with more precipitation than
#'   this value is considered \var{wet} instead of \var{dry}. Default is 0.
#'   This values should be equal to the corresponding value used in
#'   \var{SOILWAT2}'s function \code{SW_MKV_today}.
#' @param na.rm A logical value. If \code{TRUE}, then missing weather values
#'   in the input \code{weatherData} are excluded; if \code{FALSE}, then
#'   missing values are propagated.
#' @inheritParams set_missing_weather
#' @param imputation_type A character string; currently, one of three options
#'   used as arguments to call \code{\link{impute_df}}:
#'   \describe{
#'     \item{\var{"none"}}{no imputation is carried out; note: any \code{NA}s
#'       will likely cause a \var{SOILWAT2} simulation to fail;}
#'     \item{\var{"meanX"}}{missing values will be replaced by the average
#'       of \var{X} non-missing values before and \var{X} non-missing values
#'       after; \var{X} must be a positive integer; note: this may fail if
#'       there are less than \var{2 * X} non-missing values;}
#'     \item{\var{"locf"}}{missing values will be replaced by the
#'       \var{"last-observation-carried-forward"} imputation method.}
#'  }
#'
#' @return A list with two named elements:
#'   \describe{
#'     \item{\code{mkv_doy}}{A data.frame with 366 rows (day of year) and
#'        5 columns: \describe{
#'          \item{DOY}{Day of year.}
#'          \item{p_W_W}{Probability that doy is wet if the previous day
#'            (doy - 1) was wet.}
#'          \item{p_W_D}{Probability that doy is wet if the previous day
#'            (doy - 1) was dry.}
#'          \item{PPT_avg}{Average amount of precipitation (centimeters)
#'            on doy if it is wet.}
#'          \item{PPT_sd}{Standard deviation of amount of precipitation
#'            (centimeters) on doy if it is wet.}
#'        }}
#'     \item{\code{mkv_woy}}{A data.frame with 53 rows (\var{SOILWAT2}
#'       weeks of year, i.e., counted as consecutive \var{heptads} of days) and
#'       11 columns: \describe{
#'         \item{WEEK}{Week of year.}
#'         \item{wTmax_C}{Average daily maximum temperature (C) for week.}
#'         \item{wTmin_C}{Average daily minimum temperature (C) for week.}
#'         \item{var_MAX}{Variance of daily maximum temperature for week.}
#'         \item{cov_MAXMIN}{Covariance of daily maximum and minimum
#'           temperatures for week.}
#'         \item{cov_MINMAX}{Identical to \code{cov_MAXMIN}.}
#'         \item{var_MIN}{Variance of daily minimum temperature for week.}
#'         \item{CF_Tmax_wet}{Difference between average daily maximum
#'           temperature (C) for wet days of week and \code{wTmax_C}.}
#'         \item{CF_Tmax_dry}{Difference between average daily maximum
#'           temperature (C) for dry days of week and \code{wTmax_C}.}
#'         \item{CF_Tmin_wet}{Same as \code{CF_Tmax_wet} but for daily
#'           minimum temperature.}
#'         \item{CF_Tmin_dry}{Same as \code{CF_Tmax_dry} but for daily
#'           minimum temperature.}
#'       }}
#'   }
#'
#' @seealso \code{\link{print_mkv_files}} to print values to \var{SOILWAT2}
#'   compatible files. \code{\link{swMarkov_Prob}} and
#'   \code{\link{swMarkov_Conv}} to extract/replace values in a \pkg{rSOILWAT2}
#'   input object of class \code{\linkS4class{swInputData}}.
#'
#' @examples
#' res1 <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' wdata <- data.frame(dbW_weatherData_to_dataframe(weatherData, valNA = NULL))
#' res2 <- dbW_estimate_WGen_coefs(wdata)
#'
#' sw_in <- rSOILWAT2::sw_exampleData
#' swMarkov_Prob(sw_in) <- res2[["mkv_doy"]]
#' swMarkov_Conv(sw_in) <- res2[["mkv_woy"]]
#'
#' @export
dbW_estimate_WGen_coefs <- function(weatherData, WET_limit_cm = 0,
  na.rm = FALSE, valNA = NULL, imputation_type = c("none", "mean5", "locf")) {

  imputation_type <- match.arg(imputation_type)

  if (grepl("mean", imputation_type)) {
    temp <- regexec("[[:digit:]]+", imputation_type)[[1]]
    imputation_span <- if (temp > 0) {
        as.integer(regmatches(imputation_type, temp))
      } else {
        5L
      }

    imputation_type <- "mean"
  }

  # daily weather data
  if (inherits(weatherData, "list") &&
      all(sapply(weatherData, inherits, what = "swWeatherData"))) {
    wdata <- data.frame(dbW_weatherData_to_dataframe(weatherData,
      valNA = valNA))
  } else {
    wdata <- data.frame(set_missing_weather(weatherData, valNA = valNA))
  }

  n_days <- nrow(wdata)


  #-----------------------------------------------------------------------------
  #------ calculate mkv_prob.in
  #--- calculate WET days
  wdata[["WET"]] <- wdata[["PPT_cm"]] > WET_limit_cm
  wdata[["WET_yesterday"]] <- c(wdata[["WET"]][n_days], wdata[["WET"]][-n_days])


  #--- calculate WET days that follow a WET day (WW) and
  #              WET days that follow a DRY day (WD)
  wdata[["WW"]] <- wdata[["WET"]] & wdata[["WET_yesterday"]]
  wdata[["WD"]] <- wdata[["WET"]] & !wdata[["WET_yesterday"]]


  #--- output container: dataframe for storing mkv_prob.in data
  doys <- 366  # see SOILWAT2 constant `MAX_DAYS`
  outs <- c("DOY", "p_W_W", "p_W_D", "PPT_avg", "PPT_sd")
  mkv_prob <- data.frame(matrix(NA, nrow = doys, ncol = length(outs),
    dimnames = list(NULL, outs)))
  mkv_prob[, "DOY"] <- seq_len(doys)

  #--- mean/sd of precipitation across years for doy i if it is a wet day
  temp <- by(wdata[, c("WET", "PPT_cm")], INDICES = wdata[, "DOY"],
    function(x) {
      ppt <- x[x[, "WET"], "PPT_cm"]
      if (length(ppt) > 0) {
        c(PPT_avg = mean(ppt, na.rm = na.rm), PPT_sd = sd(ppt, na.rm = na.rm))
      } else {
        # there are no wet days for this DOY; thus PPT = 0
        c(PPT_avg = 0, PPT_sd = 0)
      }
    })
  mkv_prob[, c("PPT_avg", "PPT_sd")] <- do.call(rbind, temp)


  #--- wetprob = p(wet|wet) = "p_W_W" #nolint
  #    = probability that it precipitates today if it was wet
  #      (precipitated) yesterday
  #    dryprob = p(wet|dry) = "p_W_D" #nolint
  #    = probability that it precipitates today if it was dry
  #      (did not precipitate) yesterday
  temp <- by(wdata[, c("WET", "WET_yesterday", "WW", "WD")],
    INDICES = wdata[, "DOY"], function(x) {
      # p(wet): probability that today is wet
      p_W <- mean(x[, "WET"], na.rm = na.rm)
      # number of DOY = i that follow a wet day
      n_Wy <- sum(x[, "WET_yesterday"], na.rm = na.rm)
      # number of DOY = i that follow a dry day
      n_Dy <- sum(!x[, "WET_yesterday"], na.rm = na.rm)

      c(
        # `wetprob` calculated as the number of years with doy being wet
        # given previous day is wet divided by the number of years with
        # the previous day being wet
        p_W_W = if (n_Wy > 0) sum(x[, "WW"], na.rm = na.rm) / n_Wy else p_W,
         # `dryprob` calculated as the number of years with doy being wet
        # given previous day is dry divided by the number of years with
        # the previous day being dry
        p_W_D = if (n_Dy > 0) sum(x[, "WD"], na.rm = na.rm) / n_Dy else p_W)
    })
  mkv_prob[, c("p_W_W", "p_W_D")] <- do.call(rbind, temp)

  #--- Make sure probability values are well formed: finite & 0 <= p <= 1
  ids_bad0 <- !is.finite(mkv_prob[, "p_W_W"]) | mkv_prob[, "p_W_W"] < 0
  if (any(ids_bad0, na.rm = na.rm)) {
    mkv_prob[ids_bad0, "p_W_W"] <- 0
  }

  ids_bad1 <- mkv_prob[, "p_W_W"] > 1
  if (any(ids_bad1, na.rm = na.rm)) {
    mkv_prob[ids_bad1, "p_W_W"] <- 1
  }


  #--- Check that no missing coefficients
  if (anyNA(mkv_prob)) {
    ids_baddoy <- mkv_prob[apply(mkv_prob, 1, anyNA), "DOY"]

    msg <- paste0("values for n = ", length(ids_baddoy), " DOYs: ",
      paste(ids_baddoy, collapse = ", "))

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg)
    } else {
      message("Impute missing `mkv_prob` ", msg)
      mkv_prob <- impute_df(mkv_prob, imputation_type = imputation_type,
        span = imputation_span)
    }
  }



  #-----------------------------------------------------------------------------
  #------ mkv_covar.in

  #--- week as interpreted by SOILWAT2 function `Doy2Week`
  wdata[["WEEK"]] <- 1 + floor((wdata[["DOY"]] - 1) / 7)

  #--- output container: dataframe for storing mkv_cov.in data
  weeks <- 53 # see SOILWAT2 constant `MAX_WEEKS`
  outs <- c("WEEK", "wTmax_C", "wTmin_C",
    "var_MAX", "cov_MAXMIN", "cov_MINMAX", "var_MIN",
    "CF_Tmax_wet", "CF_Tmax_dry", "CF_Tmin_wet", "CF_Tmin_dry")
  mkv_cov <- data.frame(matrix(NA, nrow = weeks, ncol = length(outs),
    dimnames = list(NULL, outs)))


  #--- Aggregate for each week
  mkv_cov[, "WEEK"] <- seq_len(weeks)

  # Average weekly temperature values
  mkv_cov[, "wTmax_C"] <- tapply(wdata[["Tmax_C"]], wdata[["WEEK"]], mean,
    na.rm = na.rm)
  mkv_cov[, "wTmin_C"] <- tapply(wdata[["Tmin_C"]], wdata[["WEEK"]], mean,
    na.rm = na.rm)

  # Variance-covariance values among maximum and minimum temperature
  temp <- by(wdata[, c("Tmax_C", "Tmin_C")], wdata[["WEEK"]], cov,
    use = "na.or.complete")
  temp <- sapply(temp, function(x) c(x[1, 1], x[1, 2], x[2, 1], x[2, 2]))
  mkv_cov[, "var_MAX"] <- temp[1, ]
  mkv_cov[, "cov_MAXMIN"] <- temp[2, ]
  mkv_cov[, "cov_MINMAX"] <- temp[3, ]
  mkv_cov[, "var_MIN"] <- temp[4, ]


  #--- Temperature correction factors (delta values)
  # Used to correct random temperature values based on average conditions
  # if that target day is wet or dry (e.g., overcast weather tends to
  # increase minimum daily temperature and decrease maximum daily tempature)
  temp <- by(wdata[, c("WET", "Tmax_C", "Tmin_C")], INDICES = wdata[, "WEEK"],
    function(x) {
      iswet <- x[, "WET"]
      isanywet <- any(iswet, na.rm = na.rm)
      isdry <- !iswet
      isanydry <- any(isdry, na.rm = na.rm)

      c(Tmax_mean_wet = if (isanywet) {
            mean(x[iswet, "Tmax_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmax_C"], na.rm = na.rm)
          },
        Tmax_mean_dry = if (isanydry) {
            mean(x[isdry, "Tmax_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmax_C"], na.rm = na.rm)
          },
        Tmin_mean_wet = if (isanywet) {
            mean(x[iswet, "Tmin_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmin_C"], na.rm = na.rm)
          },
        Tmin_mean_dry = if (isanydry) {
            mean(x[isdry, "Tmin_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmin_C"], na.rm = na.rm)
          }
      )
    }
  )
  temp <- do.call(rbind, temp)

  mkv_cov[, "CF_Tmax_wet"] <- temp[, "Tmax_mean_wet"] - mkv_cov[, "wTmax_C"]
  mkv_cov[, "CF_Tmax_dry"] <- temp[, "Tmax_mean_dry"] - mkv_cov[, "wTmax_C"]
  mkv_cov[, "CF_Tmin_wet"] <- temp[, "Tmin_mean_wet"] - mkv_cov[, "wTmin_C"]
  mkv_cov[, "CF_Tmin_dry"] <- temp[, "Tmin_mean_dry"] - mkv_cov[, "wTmin_C"]


  #--- Check that no missing coefficients
  if (anyNA(mkv_cov)) {
    ids_badweek <- mkv_cov[apply(mkv_cov, 1, anyNA), "WEEK"]

    msg <- paste0("values for n = ", length(ids_badweek), " weeks: ",
      paste(ids_badweek, collapse = ", "))

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg)
    } else {
      message("Impute missing `mkv_cov` ", msg)
      mkv_cov <- impute_df(mkv_cov, imputation_type = imputation_type,
        span = imputation_span)
    }
  }


  list(mkv_woy = mkv_cov, mkv_doy = mkv_prob)
}


#' Imputes missing values in a data.frame
#'
#' @param x A \code{\link{data.frame}} with numerical columns.
#' @param imputation_type A character string; currently, one of two values:
#'   \describe{
#'     \item{\var{"none"}}{no imputation is carried out;}
#'     \item{\var{"mean"}}{missing values will be replaced by the average
#'       of \code{span} non-missing values before and \code{span} non-missing
#'       values after; note: this may fail if there are less than
#'       \code{2 * span} non-missing values;}
#'     \item{\var{"locf"}}{missing values will be replaced by the
#'       \var{"last-observation-carried-forward"} imputation method.}
#'  }
#' @param span An integer value. The number of non-missing values considered
#'   if \code{imputation_type = "mean"}.
#'
#' @return An updated version of \code{x}.
#'
#' @export
impute_df <- function(x, imputation_type = c("none", "mean", "locf"),
  span = 5L) {

  imputation_type <- match.arg(imputation_type)
  span <- round(span)

  if (imputation_type == "none") {
    return(x)
  }

  #--- imputations
  icols_withNAs <- which(apply(x, 2, anyNA))

  for (k1 in icols_withNAs) {
    irows_withNA <- which(is.na(x[, k1]))

    for (k2 in irows_withNA) {
      if (imputation_type == "mean" && span > 0) {
        #--- imputation by mean of neighbor values
        spank <- span

        # locate a sufficient number of non-missing neighbors
        repeat {
          temp <- 1 + (seq(k2 - spank, k2 + spank) - 1) %% 366
          ids_source <- temp[!(temp %in% irows_withNA)]

          if (length(ids_source) >= 2 * span || spank >= 366) {
            break
          } else {
            spank <- spank + 1
          }
        }

        # impute mean of neighbors
        x[k2, k1] <- mean(x[ids_source, k1])


      } else if (imputation_type == "locf") {
        #--- imputation by last-observation carried forward
        dlast <- 1

        # locate last non-missing value
        repeat {
          temp <- 1 + (k2 - dlast - 1) %% 366
          ids_source <- temp[!(temp %in% irows_withNA)]

          if (length(ids_source) == 1 || dlast >= 366) {
            break
          } else {
            dlast <- dlast + 1
          }
        }

        # impute locf
        x[k2, k1] <- x[ids_source, k1]
      }
    }
  }

  x
}


#' Print Markov weather generator files as required by \var{SOILWAT2}
#'
#' @param mkv_doy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#' @param mkv_woy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#'
#' @seealso \code{\link{dbW_estimate_WGen_coefs}} to
#'   calculate the necessary values based on daily weather data.
#'
#' @return This function is called for its side effect, i.e., writing values
#'   to files \var{\dQuote{mkv_prob.in}} and \var{\dQuote{mkv_covar.in}}.
#'
#' @examples
#' res <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' print_mkv_files(mkv_doy = res[["mkv_doy"]], mkv_woy = res[["mkv_woy"]],
#'   path = normalizePath("."))
#'
#' @export
print_mkv_files <- function(mkv_doy, mkv_woy, path, digits = 5) {
  colnames(mkv_doy)[1] <- paste0("#", colnames(mkv_doy)[1])
  write.table(format(mkv_doy, digits = digits),
    file = file.path(path, "mkv_prob.in"),
    sep = "\t", row.names = FALSE, quote = FALSE)

  colnames(mkv_woy)[1] <- paste0("#", colnames(mkv_woy)[1])
  write.table(format(mkv_woy, digits = digits),
    file = file.path(path, "mkv_covar.in"),
    sep = "\t", row.names = FALSE, quote = FALSE)

  invisible(TRUE)
}
