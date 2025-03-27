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


#' List daily weather variables incorporated in the weather generator
#' @export
weatherGenerator_dataColumns <- function() {
  c("Tmax_C", "Tmin_C", "PPT_cm")
}


#' Estimate coefficients of the `SOILWAT2` weather generator
#'
#' Estimated coefficients correspond to what is required by the two files
#' `"mkv_covar.in"` and `"mkv_prob.in"` for the first-order
#' Markov weather generator in `SOILWAT2` `> v4.2.5`.
#'
#' @section Notes: This code is a complete overhaul compared to the version
# nolint start: line_length_linter.
#' from [`rSFSTEP2` on `2019-Feb-10`](https://github.com/DrylandEcology/rSFSTEP2/commit/cd9e161971136e1e56d427a4f76062bbb0f3d03a).
# nolint end: line_length_linter.
#'
#' @section Notes: This function will produce `NA`s in the output if there
#' are insufficient weather observation in the input data `weatherData`
#' for a specific day or week of the year. Such `NA`s will cause a
#' `SOILWAT2` run to fail (potentially non-graciously and
#' with non-obvious error messages). To avoid that, this function offers
#' imputation approaches in order to fill in those failed coefficient
#' estimates; see [rSW2utils::impute_df()], but please note that
#' any such imputation likely introduces biases in the generated weather.
#'
#' @section Details:
#' Most users will likely want to set `propagate_NAs` to `FALSE`.
#' Note: `propagate_NAs` corresponds to `!na.rm`
#' from previous versions of this function with a different default value.
#' Consider an example: a the 30-year long input `weatherData` is
#' complete except for missing values on Jan 1, 2018.
#'   * If `propagate_NAs` is set to `TRUE`, then the
#'     coefficients for day 1 and week 1 of year will be `NA` --
#'     despite all the available data. In this case, the missing coefficients
#'     for day 1 and week 1 of year will be imputed.
#'   * If `propagate_NAs` is set to `FALSE`, then the coefficients
#'     for day 1 and week 1 of year will be calculated based on the non-missing
#'     values for that day respectively that week of year. No imputation occurs.
#'
#'
#' @param weatherData A list of elements of class [`swWeatherData`]
#' or a data frame as returned by [dbW_weatherData_to_dataframe()].
#' @param WET_limit_cm A numeric value. A day with more precipitation than
#' this value is considered `wet` instead of `dry`. Default is 0.
#' This values should be equal to the corresponding value used in
#' `SOILWAT2`'s function `SW_MKV_today()`.
#' @param propagate_NAs A logical value. If `TRUE`, then missing weather
#' values in the input `weatherData` are excluded; if `FALSE`, then
#' missing values are propagated to the estimation. See Details.
#' @param imputation_type A text string. Passed to [rSW2utils::impute_df()]
#' for imputation of missing values in estimated weather generator coefficients.
#' @param imputation_span An integer value. Passed to [rSW2utils::impute_df()]
#' for imputation of missing values in estimated weather generator coefficients.
#' @inheritParams set_missing_weather
#'
#' @return A list with two named elements:
#'   * `"mkv_doy"`: A data frame with 366 rows (day of year) and 5 columns:
#'      * `"DOY"`: Rows represent day of year.
#'      * `"p_W_W"`: Probability that `DOY` is wet if the previous day
#'        `(doy - 1)` was wet.
#'      * `"p_W_D"`: Probability that `DOY` is wet if the previous day
#'         `(doy - 1)` was dry.
#'      * `"PPT_avg"`: Average amount of precipitation `[cm]` on `DOY` if wet.
#'      * `"PPT_sd"`: Standard deviation of amount of precipitation `[cm]`
#'        on `DOY` if wet.
#'
#'   * `"mkv_woy"`: A data frame with 53 rows `SOILWAT2` weeks of year
#'     (i.e., consecutive `heptads` of days) and 11 columns
#'       * `"WEEK"`: Rows represent week of year.
#'       * `"wTmax_C"`: Average daily maximum temperature `[C]` for week.
#'       * `"wTmin_C"`: Average daily minimum temperature `[C]` for week.
#'       * `"var_MAX"`: Variance of daily maximum temperature for week.
#'       * `"cov_MAXMIN"`: Covariance of daily maximum and minimum
#'           temperatures for week.
#'       * `"cov_MINMAX"`: Identical to `"cov_MAXMIN"`.
#'       * `"var_MIN"`: Variance of daily minimum temperature for week.
#'       * `"CF_Tmax_wet"`: Difference between average daily maximum
#'           temperature `[C]` for wet days of week and `"wTmax_C"`.
#'       * `"CF_Tmax_dry"`: Difference between average daily maximum
#'           temperature `[C]` for dry days of week and `"wTmax_C"`.
#'       * `"CF_Tmin_wet"`: Same as `"CF_Tmax_wet"` but for daily
#'           minimum temperature.
#'       * `"CF_Tmin_dry"`: Same as `"CF_Tmax_dry"` but for daily
#'           minimum temperature.
#'
#'
#' @seealso [print_mkv_files()] to print values to `SOILWAT2`
#' compatible files. [swMarkov_Prob()] and
#' [swMarkov_Conv()] to extract/replace values in a `rSOILWAT2`
#' input object of class [`swInputData`].
#'
#' @examples
#' res1 <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
#' res2 <- dbW_estimate_WGen_coefs(wdata)
#'
#' sw_in <- rSOILWAT2::sw_exampleData
#' swMarkov_Prob(sw_in) <- res2[["mkv_doy"]]
#' swMarkov_Conv(sw_in) <- res2[["mkv_woy"]]
#'
#' @md
#' @export
dbW_estimate_WGen_coefs <- function(
  weatherData,
  WET_limit_cm = 0,
  propagate_NAs = FALSE,
  valNA = NULL,
  imputation_type = c("none", "mean", "locf"),
  imputation_span = 5L
) {

  # daily weather data
  if (
    inherits(weatherData, "list") &&
    all(sapply(weatherData, inherits, what = "swWeatherData"))
  ) {
    wdata <- data.frame(
      dbW_weatherData_to_dataframe(weatherData, valNA = valNA)
    )
  } else {
    wdata <- data.frame(
      set_missing_weather(weatherData, valNA = valNA)
    )
  }

  n_days <- nrow(wdata)

  imputation_type <- match.arg(imputation_type)

  na.rm <- !propagate_NAs

  #------ calculate mkv_prob.in
  icol_day <- grep(
    "DOY|Day",
    colnames(wdata),
    ignore.case = TRUE,
    value = TRUE
  )

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
  mkv_prob <- data.frame(
    matrix(nrow = doys, ncol = length(outs), dimnames = list(NULL, outs))
  )
  mkv_prob[, "DOY"] <- seq_len(doys)

  #--- mean/sd of precipitation across years for doy i if it is a wet day
  temp <- by(
    wdata[, c("WET", "PPT_cm")],
    INDICES = wdata[, icol_day],
    function(x) {
      # if `na.rm` is TRUE, then remove NAs in `WET`; if only NAs -> PPT_avg = 0
      # if `na.rm` is FALSE, then any NA propagates to PPT_avg = NA
      iswet <- if (na.rm) which(x[, "WET"]) else x[, "WET"]
      ppt <- x[iswet, "PPT_cm"]

      if (length(ppt) > 0) {
        c(
          PPT_avg = mean(ppt, na.rm = na.rm),
          PPT_sd = sd(ppt, na.rm = na.rm)
        )
      } else {
        # there are no wet days for this DOY; thus PPT = 0
        c(
          PPT_avg = 0,
          PPT_sd = 0
        )
      }
    }
  )
  mkv_prob[, c("PPT_avg", "PPT_sd")] <- do.call(rbind, temp)


  #--- wetprob = p(wet|wet) = "p_W_W" #nolint
  #    = probability that it precipitates today if it was wet
  #      (precipitated) yesterday
  #    dryprob = p(wet|dry) = "p_W_D" #nolint
  #    = probability that it precipitates today if it was dry
  #      (did not precipitate) yesterday
  temp <- by(
    wdata[, c("WET", "WET_yesterday", "WW", "WD")],
    INDICES = wdata[, icol_day],
    function(x) {
      # p(wet): probability that today is wet
      p_W <- mean(x[, "WET"], na.rm = na.rm)
      # number of DOY = i that follow a wet day
      n_Wy <- sum(x[, "WET_yesterday"], na.rm = na.rm)
      # number of DOY = i that follow a dry day
      n_Dy <- sum(!x[, "WET_yesterday"], na.rm = na.rm)

      c(
        p_W_W = if (isTRUE(n_Wy > 0)) {
          # `p(wet|wet)` estimated as the number of years with doy being wet
          # given previous day is wet divided by the number of years with
          # the previous day being wet
          sum(x[, "WW"], na.rm = na.rm) / n_Wy
        } else {
          # `p(wet|wet)` approximated with frequency that today is wet for
          # data where yesterday is never wet (avoid division by zero);
          # this value is likely near 0 because p(wet yesterday) = 0
          # and p(wet today) ~ p(wet yesterday)
          p_W
        },

        p_W_D = if (isTRUE(n_Dy > 0)) {
          # `p(wet|dry)` estimated as the number of years with doy being wet
          # given previous day is dry divided by the number of years with
          # the previous day being dry
          sum(x[, "WD"], na.rm = na.rm) / n_Dy
        } else {
          # `p(wet|dry)` approximated with frequency that today is wet for
          # data where yesterday is never dry (avoid division by zero);
          # this value is likely near 1 because p(wet yesterday) = 1
          # and p(wet today) ~ p(wet yesterday)
          p_W
        }
      )
    }
  )

  mkv_prob[, c("p_W_W", "p_W_D")] <- do.call(rbind, temp)

  #--- Make sure probability values are well formed: 0 <= p <= 1
  ids_bad0 <- which(mkv_prob[, "p_W_W"] < 0)
  mkv_prob[ids_bad0, "p_W_W"] <- 0

  ids_bad1 <- which(mkv_prob[, "p_W_W"] > 1)
  mkv_prob[ids_bad1, "p_W_W"] <- 1


  #--- Check that no missing coefficients
  if (anyNA(mkv_prob)) {
    ids_baddoy <- mkv_prob[apply(mkv_prob, 1, anyNA), "DOY"]

    msg <- paste0(
      "values for n = ", length(ids_baddoy),
      " DOYs: ", toString(ids_baddoy)
    )

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg, call. = FALSE)
    } else {
      message("Impute missing `mkv_prob` ", msg, call. = FALSE)
      mkv_prob <- rSW2utils::impute_df(
        mkv_prob,
        imputation_type = imputation_type,
        imputation_span = imputation_span,
        cyclic = TRUE
      )
    }
  }



  #------ mkv_covar.in

  #--- week as interpreted by SOILWAT2 function `Doy2Week`
  wdata[["WEEK"]] <- 1 + floor((wdata[[icol_day]] - 1) / 7)

  #--- output container: dataframe for storing mkv_cov.in data
  weeks <- 53 # see SOILWAT2 constant `MAX_WEEKS`
  outs <- c(
    "WEEK", "wTmax_C", "wTmin_C",
    "var_MAX", "cov_MAXMIN", "cov_MINMAX", "var_MIN",
    "CF_Tmax_wet", "CF_Tmax_dry", "CF_Tmin_wet", "CF_Tmin_dry"
  )
  mkv_cov <- data.frame(
    matrix(nrow = weeks, ncol = length(outs), dimnames = list(NULL, outs))
  )


  #--- Aggregate for each week
  mkv_cov[, "WEEK"] <- seq_len(weeks)

  # Average weekly temperature values
  mkv_cov[, "wTmax_C"] <- tapply(
    wdata[["Tmax_C"]],
    wdata[["WEEK"]],
    mean,
    na.rm = na.rm
  )

  mkv_cov[, "wTmin_C"] <- tapply(
    wdata[["Tmin_C"]],
    wdata[["WEEK"]],
    mean,
    na.rm = na.rm
  )

  # Variance-covariance values among maximum and minimum temperature
  temp <- by(
    wdata[, c("Tmax_C", "Tmin_C")],
    wdata[["WEEK"]],
    cov,
    use = if (na.rm) "na.or.complete" else "everything"
  )
  temp <- sapply(temp, function(x) c(x[1, 1], x[1, 2], x[2, 1], x[2, 2]))

  mkv_cov[, "var_MAX"] <- temp[1, ]
  mkv_cov[, "cov_MAXMIN"] <- temp[2, ]
  mkv_cov[, "cov_MINMAX"] <- temp[3, ]
  mkv_cov[, "var_MIN"] <- temp[4, ]


  #--- Temperature correction factors (delta values)
  # Used to correct random temperature values based on average conditions
  # if that target day is wet or dry (e.g., overcast weather tends to
  # increase minimum daily temperature and decrease maximum daily tempature)
  temp <- by(
    wdata[, c("WET", "Tmax_C", "Tmin_C")],
    INDICES = wdata[, "WEEK"],
    function(x) {
      # if `na.rm` is TRUE, then consider `WET` = NA as FALSE
      # if `na.rm` is FALSE, then propagate NAs in `WET` -> neither wet nor dry
      iswet <- if (na.rm) {
        which_wet <- which(x[, "WET"]) # numeric vector
        out <- rep(FALSE, length(x[, "WET"]))
        # only days where 'WET' is TRUE are considered wet
        out[which_wet] <- TRUE
        out # logical vector same length as x[, "WET"]
      } else {
        x[, "WET"] # logical vector
      }

      isanywet <- isTRUE(any(iswet, na.rm = na.rm))

      # previously isdry became all FALSE if na.rm = TRUE (because then iswet
      # was numeric  vector with all positive digits)
      isdry <- !iswet
      isanydry <- isTRUE(any(isdry, na.rm = na.rm))

      # if no wet/dry days in week of year, then use overall mean instead
      # of conditional mean (i.e., given wet/dry)
      c(
        Tmax_mean_wet = if (isanywet) {
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

    msg <- paste0(
      "values for n = ", length(ids_badweek),
      " weeks: ", toString(ids_badweek)
    )

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg, call. = FALSE)
    } else {
      message("Impute missing `mkv_cov` ", msg, call. = FALSE)
      mkv_cov <- rSW2utils::impute_df(
        mkv_cov,
        imputation_type = imputation_type,
        imputation_span = imputation_span,
        cyclic = TRUE
      )
    }
  }


  list(mkv_woy = mkv_cov, mkv_doy = mkv_prob)
}


#' Print Markov weather generator files as required by \var{SOILWAT2}
#'
#' @param mkv_doy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#' @param mkv_woy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#' @param path A character string. The path on disk to the location
#'   where output files should be created.
#' @param digits An integer value. The number of digits with which to write
#'   the values to disk.
#'
#' @seealso \code{\link{dbW_estimate_WGen_coefs}} to
#'   calculate the necessary values based on daily weather data.
#'
#' @return This function is called for its side effect, i.e., writing values
#'   to files \var{\dQuote{mkv_prob.in}} and \var{\dQuote{mkv_covar.in}}.
#'
#' @examples
#' path <- tempdir()
#'
#' res <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' print_mkv_files(
#'   mkv_doy = res[["mkv_doy"]],
#'   mkv_woy = res[["mkv_woy"]],
#'   path = path)
#'
#' ## Cleanup
#' unlink(list.files(path), force = TRUE)
#'
#' @export
print_mkv_files <- function(mkv_doy, mkv_woy, path, digits = 5) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  colnames(mkv_doy)[1] <- paste0("#", colnames(mkv_doy)[1])
  utils::write.table(
    format(mkv_doy, digits = digits),
    file = file.path(path, "mkv_prob.in"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  colnames(mkv_woy)[1] <- paste0("#", colnames(mkv_woy)[1])
  utils::write.table(
    format(mkv_woy, digits = digits),
    file = file.path(path, "mkv_covar.in"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  invisible(TRUE)
}



# Check that \code{weather} meets expectations
check_weather <- function(weather, required_variables) {
  stopifnot(
    length(dim(weather)) == 2,
    sapply(
      required_variables,
      function(p) length(grep(p, x = colnames(weather))) == 1
    )
  )
}

# Aggregate daily weather for each time step
prepare_weather <- function(
  data_daily,
  time_steps = c("Year", "Month", "Week", "Day"),
  na.rm = FALSE
) {

  weather_list <- list()
  id_daily <- "Day" == time_steps

  for (it in time_steps[!id_daily]) {
    weather_list[[it]] <- dbW_dataframe_aggregate(data_daily, it, na.rm = na.rm)
  }

  weather_list[["Day"]] <- data_daily
  weather_list
}

# Prepare weather data object for \code{\link{compare_dailyweather}}
prepare_weather_for_comparison <- function(
  weather,
  time_steps = c("Year", "Month", "Week", "Day"),
  na.rm = FALSE
) {
  req_vars <- c("Year", "Tmax_C", "Tmin_C", "PPT_cm")

  if (length(weather) == length(time_steps) &&
      inherits(weather, "list")) {
    # if a list, then four suitable elements one for each time step
    stopifnot(time_steps %in% names(weather))
    for (it in time_steps) {
      required_variables <- c(req_vars, switch(it,
        Year = NULL, Month = "Month", Week = "Week", Day = "DOY|Day"))
      check_weather(weather[[it]], required_variables)
      if (it == "Day") {
        colnames(weather[[it]])[2] <- "Day"
      }
    }

    res <- weather

  } else if (inherits(weather, c("matrix", "data.frame"))) {
    # if a two-dim object, then it has to be daily data and we need to
    # aggregate for each time step
    check_weather(weather, c("DOY|Day", req_vars))
    colnames(weather)[2] <- "Day"
    res <- prepare_weather(weather, na.rm = na.rm)

  } else {
    stop("Structure of `weather` not suitable", call. = FALSE)
  }

  res
}

#' Compare two weather datasets: produces comparison plots for means, quantiles,
#' and Markov weather generator input parameters for all time steps
#'
#' @section Notes: The number of days represented by \code{ref_weather} and by
#'   \code{weather} does not need to be the same.
#' @section Notes: See also the Weather generator integration tests (in file
#'   \var{"tests/testthat/test_WeatherGenerator_functionality.R}).
#'
#' @param ref_weather A \code{list} of or a two-dimensional numerical object,
#'   e.g., \code{data.frame} or \code{matrix}. If a \code{list} then four
#'   suitable elements one for each \code{time_step}. Otherwise, daily weather
#'   data with columns \var{Year}, \var{DOY}, \var{Tmax_C}, \var{Tmin_C},
#'   and \var{PPT_cm}; for instance, the result of
#'   function \code{\link{dbW_weatherData_to_dataframe}}. This represents the
#'   reference weather against which \code{weather} is compared.
#' @param weather A \code{list} of elements such as described for
#'   \code{ref_weather} or an object such as \code{ref_weather}. This represents
#'   the weather data (potentially from many sites and/or runs) that are
#'   compared against \code{ref_weather}.
#' @param N An integer number representing the number of runs or sites contained
#'   in \code{weather}.
#' @inheritParams dbW_estimate_WGen_coefs
#' @param path A character string. The directory path in which output figures
#'   will be saved.
#' @param tag A character string to uniquely identify a set of output figures.
#'
#' @return This function is called for its side effects of producing figures
#'   that are saved as \var{png} files on the disk.
#'
#' @examples
#' path <- tempdir()
#'
#' ## Example with default rSOILWAT2 weather data
#' w0 <- dbW_weatherData_to_dataframe(rSOILWAT2::weatherData)
#' w1 <- w0
#' w1[, "Tmax_C"] <- w1[, "Tmax_C"] + 2
#' w1[, "Tmin_C"] <- w1[, "Tmin_C"] - 2
#'
#' compare_weather(
#'   ref_weather = w0,
#'   weather = w1,
#'   N = 1,
#'   path = path,
#'   tag = "Example1-Silly"
#' )
#'
#' ## Example with STEPWAT2 output data averaged across iterations (`-o` option)
#' fname_main <- "file_STEPWAT2_main.csv"
#' fname_dev <- "file_STEPWAT2_dev.csv"
#' if (all(file.exists(fname_main, fname_dev))) {
#'   cols_STEPWAT2 <- c(
#'     "Year", "Day", "PRECIP_ppt_Mean", "TEMP_max_C_Mean", "TEMP_min_C_Mean"
#'   )
#'   cols_rSOILWAT2 <- c("Year", "Day", "PPT_cm", "Tmax_C", "Tmin_C")
#'   w0 <- utils::read.csv(fname_main)[, cols_STEPWAT2]
#'   colnames(w0) <- cols_rSOILWAT2
#'   w1 <- utils::read.csv(fname_dev)[, cols_STEPWAT2]
#'   colnames(w1) <- cols_rSOILWAT2
#'
#'   # Note: Since values are averages across many iterations, most days
#'   # have average precipitation values > 0; thus, we need to adjust
#'   # `WET_limit_cm` accordingly (here, with a guess)
#'   compare_weather(
#'     ref_weather = w0,
#'     weather = w1,
#'     N = 1,
#'     WET_limit_cm = 0.1,
#'     path = path,
#'     tag = "Example2-STEPWAT2"
#'   )
#' }
#'
#' ## Cleanup
#' unlink(list.files(path), force = TRUE)
#'
#' @export
compare_weather <- function(
  ref_weather,
  weather,
  N,
  WET_limit_cm = 0,
  path = ".",
  tag = format(Sys.time(), "%Y%m%d-%H%M")
) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  time_steps <- c("Year", "Month", "Week", "Day")
  weather_vars <- c("Tmax_C", "Tmin_C", "PPT_cm")
  vtags <- c("tamax (C)", "tamin (C)", "pr (cm)")
  ftags <- c("Mean", "SD", "Cor(., pr)")

  #--- Prepare reference weather
  ref_df <- list(prepare_weather_for_comparison(ref_weather, na.rm = TRUE))

  #--- Prepare other weather
  if (N > 1 && length(weather) == N && inherits(weather, "list") &&
      !all(time_steps %in% names(weather))) {
    # many runs/sites: each element consists of an element as `ref_weather`
    comp_df <- lapply(weather, prepare_weather_for_comparison)

  } else if (N == 1) {
    # one run/site
    comp_df <- list(prepare_weather_for_comparison(weather, na.rm = TRUE))

  } else {
    stop("Structure of `weather` not suitable", call. = FALSE)
  }


  #------- OUTPUTS
  #--- * Boxplots of means, SD, and cor(., pr) ------
  calculate_MeansSDs <- function(
    data,
    vars = weather_vars,
    var_ppt = "PPT_cm",
    periods = time_steps
  ) {
    tmp <- lapply(
      vars,
      function(var) {
        vapply(
          periods,
          function(ts) {
            vapply(
              data,
              function(x) {
                tmp <- x[[ts]][, var]
                c(
                  mean(tmp, na.rm = TRUE),
                  stats::sd(tmp, na.rm = TRUE),
                  stats::cor(tmp, x[[ts]][, var_ppt])
                )
              },
              FUN.VALUE = rep(NA_real_, times = 3L)
            )
          },
          FUN.VALUE = array(NA_real_, dim = c(3L, times = length(data)))
        )
      }
    )

    array(
      unlist(tmp),
      dim = c(3L, length(data), length(time_steps), length(weather_vars)),
      dimnames = list(
        c("mean", "sd", "cor(., pr)"),
        names(data),
        time_steps,
        weather_vars
      )
    )
  }

  foo_bxp <- function(data, ref_data, ylab, legend = FALSE) {
    if (is.null(dim(data))) {
      data <- matrix(data, nrow = 1, dimnames = list(NULL, names(data)))
    }
    stopifnot(ncol(data) == length(ref_data))
    ylim <- range(data, ref_data, na.rm = TRUE)

    if (all(is.finite(ylim))) {
      graphics::boxplot(data, ylim = ylim, ylab = ylab)
      graphics::points(
        seq_along(ref_data),
        ref_data,
        col = "red",
        pch = 4,
        lwd = 2
      )

      if (legend) {
        graphics::legend(
          "topright",
          legend = c("Reference", "Weather"),
          col = c("red", "black"),
          pch = c(4, 16),
          pt.lwd = 2
        )
      }

    } else {
      graphics::plot.new()
    }
  }


  # Calculate means and sds
  ref_MeanSD <- calculate_MeansSDs(ref_df)
  comp_MeanSD <- calculate_MeansSDs(comp_df)

  # Make figure
  panels <- dim(ref_MeanSD)[c(1L, 4L)]
  grDevices::png(
    units = "in",
    res = 150,
    height = 3 * panels[[1L]],
    width = 4 * panels[[2L]],
    file = file.path(path, paste0(tag, "_CompareWeather_Boxplots.png"))
  )
  par_prev <- graphics::par(
    mfrow = panels,
    mar = c(2, 2.5, 0.5, 0.5),
    mgp = c(1, 0, 0),
    tcl = 0.3,
    cex = 1
  )

  for (kv in seq_along(vtags)) {
    for (kf in seq_along(ftags)) {
      tmp_ylab <- if (grepl(".", ftags[[kf]], fixed = TRUE)) {
        sub(".", vtags[[kv]], ftags[[kf]], fixed = TRUE)
      } else {
        paste(ftags[[kf]], vtags[[kv]])
      }

      foo_bxp(
        data = comp_MeanSD[kf, , , kv],
        ref_data = ref_MeanSD[kf, , , kv],
        ylab = tmp_ylab,
        legend = kv == 1L && kf == 1L
      )
    }
  }

  graphics::par(par_prev)
  grDevices::dev.off()


  #--- * Quantile-quantile scatterplots ------
  foo_qq <- function(data, ref_data, var, time, lab, legend = FALSE) {

    vlim <- range(
      sapply(
        c(ref_data, data),
        function(x) range(x[[time]][, var], na.rm = TRUE)
      )
    )

    if (all(is.finite(vlim))) {
      probs <- seq(0, 1, length.out = 1000)

      x <- stats::quantile(
        ref_data[[1L]][[time]][, var], probs = probs,
        na.rm = TRUE
      )
      graphics::plot(
        x,
        x,
        type = "n",
        xlim = vlim,
        ylim = vlim,
        asp = 1,
        xlab = paste0(time, "ly: reference ", lab),
        ylab = paste0(time, "ly: weather ", lab)
      )

      for (k in seq_along(data)) {
        qy <- stats::quantile(
          data[[k]][[time]][, var], probs = probs,
          na.rm = TRUE
        )
        graphics::points(x, qy, pch = 46)
      }

      graphics::abline(h = 0, lty = 2)
      graphics::abline(v = 0, lty = 2)
      graphics::segments(
        x0 = vlim[1],
        y0 = vlim[1],
        x1 = vlim[2],
        y1 = vlim[2],
        col = "red",
        lwd = 2
      )


      if (legend) {
        graphics::legend(
          "topleft",
          legend = c("Reference", "Weather"),
          col = c("red", "black"),
          pch = c(NA, 16),
          pt.lwd = 2,
          lty = c(1, NA),
          lwd = 2,
          merge = TRUE
        )
      }

    } else {
      graphics::plot.new()
    }
  }

  # Make figure
  panels <- c(length(time_steps), length(weather_vars))
  grDevices::png(
    units = "in",
    res = 150,
    height = 3 * panels[[1L]],
    width = 3 * panels[[2L]],
    file = file.path(path, paste0(tag, "_CompareWeather_QQplots.png"))
  )
  par_prev <- graphics::par(
    mfrow = panels,
    mar = c(2, 2.5, 0.5, 0.5),
    mgp = c(1, 0, 0),
    tcl = 0.3,
    cex = 1
  )

  for (kt in seq_along(time_steps)) {
    for (kv in seq_along(weather_vars)) {
      foo_qq(
        comp_df,
        ref_df,
        var = weather_vars[[kv]],
        time = time_steps[[kt]],
        lab = vtags[[kv]],
        legend = kt == 1L
      )
    }
  }

  graphics::par(par_prev)
  grDevices::dev.off()



  #--- * Climate time-series ------
  foo_mts <- function(data, ref_data, var, time, lab, legend = FALSE) {

    get_mts <- function(x, var, time) {
      if (identical(time, "Year")) {
        lapply(x, function(x) x[[time]][, var])
      } else {
        lapply(
          x,
          function(x) {
            tapply(
              x[[time]][, var],
              INDEX = x[[time]][, 2L],
              FUN = mean,
              na.rm = TRUE
            )
          }
        )
      }
    }

    xref <- get_mts(ref_data, var, time)[[1L]]
    xw <- get_mts(data, var, time)

    vlim <- range(
      vapply(
        c(xref, xw),
        range,
        na.rm = TRUE,
        FUN.VALUE = rep(NA_real_, times = 2L)
      )
    )

    is_comparable <- if (identical(time, "Year")) {
      all(
        vapply(
          data,
          function(x) {
            identical(
              ref_data[[1L]][["Year"]][, "Year"],
              x[["Year"]][, "Year"]
            )
          },
          FUN.VALUE = NA
        )
      )
    } else {
      all(lengths(xw) == length(xref))
    }

    if (all(is_comparable, is.finite(vlim))) {
      xt <- seq_along(xref)

      graphics::plot(
        xt,
        y = xref,
        type = "n",
        ylim = vlim,
        xlab = if (identical(time, "Year")) time else paste("Mean", time),
        ylab = lab
      )

      for (k in seq_along(xw)) {
        graphics::lines(xt, xw[[k]], col = "darkgray")
      }

      graphics::lines(xt, xref, col = "red")

      if (legend) {
        graphics::legend(
          "topleft",
          legend = c("Reference", "Weather"),
          col = c("red", "black"),
          pch = c(NA, 16),
          pt.lwd = 2,
          lty = c(1, NA),
          lwd = 2,
          merge = TRUE
        )
      }

    } else {
      graphics::plot.new()
    }
  }

  # Make figure
  panels <- c(length(time_steps), length(weather_vars))
  grDevices::png(
    units = "in",
    res = 150,
    height = 3 * panels[[1L]],
    width = 4 * panels[[2L]],
    file = file.path(path, paste0(tag, "_CompareWeather_MeanTimeSeries.png"))
  )
  par_prev <- graphics::par(
    mfrow = panels,
    mar = c(2, 2.5, 0.5, 0.5),
    mgp = c(1, 0, 0),
    tcl = 0.3,
    cex = 1
  )

  for (kt in seq_along(time_steps)) {
    for (kv in seq_along(weather_vars)) {
      foo_mts(
        comp_df,
        ref_df,
        var = weather_vars[[kv]],
        time = time_steps[[kt]],
        lab = vtags[[kv]],
        legend = kt == 1L
      )
    }
  }

  graphics::par(par_prev)
  grDevices::dev.off()


  #--- * Does output weather recreate weather generator inputs? ------
  ref_wgin <- dbW_estimate_WGen_coefs(
    ref_df[[1L]][["Day"]],
    WET_limit_cm = WET_limit_cm,
    imputation_type = "mean"
  )

  comp_wgin <- lapply(
    comp_df,
    function(x) {
      dbW_estimate_WGen_coefs(
        x[["Day"]],
        WET_limit_cm = WET_limit_cm,
        imputation_type = "mean"
      )
    }
  )


  foo_scatter_wgin <- function(data, ref_data, obj, fname) {
    vars <- colnames(ref_data[[obj]])[-1L]
    panels <- if (length(vars) == 4L) {
      c(2L, 2L)
    } else if (length(vars) == 10L) {
      c(4L, 3L)
    } else {
      rep(ceiling(sqrt(length(vars))), 2L)
    }

    grDevices::png(
      units = "in",
      res = 150,
      height = 3 * panels[[1L]],
      width = 3 * panels[[2L]],
      file = fname
    )

    par_prev <- graphics::par(
      mfrow = panels,
      mar = c(2, 2.5, 0.5, 0.5),
      mgp = c(1, 0, 0),
      tcl = 0.3,
      cex = 1
    )

    for (v in vars) {
      x <- ref_data[[obj]][, v]

      vlim_obs <- range(x, na.rm = TRUE)
      vlim <- range(
        sapply(data, function(x) range(x[[obj]][, v], na.rm = TRUE))
      )

      if (all(is.finite(vlim_obs)) && all(is.finite(vlim))) {
        graphics::plot(
          x,
          x,
          type = "n",
          xlim = vlim,
          ylim = vlim,
          asp = 1,
          xlab = paste0("Reference ", v),
          ylab = paste0("Weather ", v)
        )

        for (k in seq_along(data)) {
          isgood <- complete.cases(cbind(x, data[[k]][[obj]][, v]))
          graphics::lines(
            stats::lowess(x[isgood], data[[k]][[obj]][isgood, v]),
            col = "gray"
          )
        }

        graphics::abline(h = 0, lty = 2)
        graphics::abline(v = 0, lty = 2)
        graphics::segments(
          x0 = vlim_obs[1],
          y0 = vlim_obs[1],
          x1 = vlim_obs[2],
          y1 = vlim_obs[2],
          col = "red",
          lwd = 2
        )

        if (v == vars[1]) {
          graphics::legend(
            "topleft",
            legend = c("Reference", "Weather"),
            col = c("red", "black"),
            lwd = 2
          )
        }

      } else {
        graphics::plot.new()
      }
    }

    graphics::par(par_prev)
    grDevices::dev.off()
  }


  foo_scatter_wgin(
    data = comp_wgin,
    ref_data = ref_wgin,
    obj = "mkv_doy",
    fname = file.path(
      path,
      paste0(tag, "_CompareWeather_WGenInputs_DayOfYear.png")
    )
  )

  foo_scatter_wgin(
    data = comp_wgin,
    ref_data = ref_wgin,
    obj = "mkv_woy",
    fname = file.path(
      path,
      paste0(tag, "_CompareWeather_WGenInputs_WeekOfYear.png")
    )
  )

}


#' Generate daily weather data using SOILWAT2's weather generator
#'
#' This function is a convenience wrapper for [dbW_estimate_WGen_coefs()].
#'
#' @inheritParams dbW_estimate_WGen_coefs
#' @inheritParams sw_weather_data
#' @param years An integer vector. The calendar years for which to generate
#' daily weather. If `NULL`, then extracted from `weatherData`.
#' @param elevation A numeric value. Site elevation above sea level `[m]`.
#'   Used only if specific humidity is provided as input
#'   for calculating relative humidity.
#' @param wgen_coeffs A list with two named elements `"mkv_doy"` and
#' `"mkv_woy"`, i.e., the return value of [dbW_estimate_WGen_coefs()].
#' If `NULL`, then [dbW_estimate_WGen_coefs()] is called on `weatherData`.
#' @param seed An integer value or `NULL`. See [base::set.seed()].
#' @param return_weatherDF A logical value. See section "Value".
#'
#' @return An updated copy of `weatherData` where missing values are imputed
#' by the weather generator.
#' If `return_weatherDF` is `TRUE`, then the result is converted to a
#' data frame where columns represent weather variables.
#' If `return_weatherDF` is `FALSE`, then the result is
#' a list of elements of class [`swWeatherData`].
#'
#' @section Details:
#' The current implementation of the weather generator produces values
#' only for variables in [weatherGenerator_dataColumns()].
#' Values are generated for those days where at least one of the implemented
#' variables is missing; if any value is missing, then values for that day of
#' all implemented variables will be replaced by those produced
#' by the weather generator.
#'
#' @seealso [dbW_imputeWeather()]
#'
#' @examples
#' # Load data for 1949-2010
#' wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
#'
#' # Treat data for 2005-2010 as our 'dataset'
#' ids <- wdata[, "Year"] >= 2005
#' x <- wdata[ids, ]
#'
#' # Set June-August of 2008 as missing
#' ids <- x[, "Year"] == 2008 & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
#' x[ids, -(1:2)] <- NA
#'
#' ## Example 1: generate weather for any missing values in our 'dataset'
#' wout1 <- dbW_generateWeather(x, return_weatherDF = TRUE)
#'
#' ## Example 2: generate weather based on our 'dataset' but for
#' ## years 2005-2015 and use estimated weather generator coefficients from
#' ## a different dataset
#' wgen_coeffs <- dbW_estimate_WGen_coefs(
#'   wdata,
#'   imputation_type = "mean",
#'   imputation_span = 5
#' )
#'
#' # Set seed to make output reproducible
#' wout2 <- dbW_generateWeather(
#'   x,
#'   years = 2005:2015,
#'   wgen_coeffs = wgen_coeffs,
#'   seed = 123
#' )
#'
#' ## Example 3: generate weather based only on estimated weather generator
#' ## coefficients from a different dataset
#' x_empty <- weatherHistory()
#' wout3 <- dbW_generateWeather(
#'   x_empty,
#'   years = 2050:2055,
#'   wgen_coeffs = wgen_coeffs,
#'   seed = 123
#' )
#'
#' ## Compare input weather with generated weather
#' path <- tempdir()
#' compare_weather(
#'   ref_weather = x,
#'   weather = wout1,
#'   N = 1,
#'   path = path,
#'   tag = "Example1-WeatherGenerator"
#' )
#' unlink(list.files(path), force = TRUE)
#'
#' @md
#' @export
dbW_generateWeather <- function(
  weatherData,
  years = NULL,
  elevation = NA,
  wgen_coeffs = NULL,
  imputation_type = "mean",
  imputation_span = 5L,
  return_weatherDF = FALSE,
  digits = NA,
  seed = NULL
) {
  #--- Obtain missing/null arguments
  if (is.null(wgen_coeffs)) {
    wgen_coeffs <- dbW_estimate_WGen_coefs(
      weatherData,
      propagate_NAs = FALSE,
      imputation_type = imputation_type,
      imputation_span = imputation_span
    )
  }

  if (!dbW_check_weatherData(weatherData, check_all = FALSE)) {
    weatherData <- dbW_dataframe_to_weatherData(weatherData)
  }


  if (is.null(years)) {
    years <- get_years_from_weatherData(weatherData)
  }

  #--- Put rSOILWAT2 input object together to produce imputed daily weather
  sw_in <- rSOILWAT2::sw_exampleData

  # Set years
  swYears_EndYear(sw_in) <- max(years)
  swYears_StartYear(sw_in) <- min(years)

  # Set elevation
  swSite_IntrinsicSiteParams(sw_in)[3L] <- as.numeric(elevation)

  # Turn on weather generator (to fill in missing values)
  swWeather_UseMarkov(sw_in) <- TRUE
  swWeather_UseMarkovOnly(sw_in) <- FALSE

  # Set weather generator coefficients
  swMarkov_Prob(sw_in) <- wgen_coeffs[["mkv_doy"]]
  swMarkov_Conv(sw_in) <- wgen_coeffs[["mkv_woy"]]

  # Turn off monthly use flags
  sw_in@weather@use_cloudCoverMonthly <- FALSE
  sw_in@weather@use_humidityMonthly <- FALSE
  sw_in@weather@use_windSpeedMonthly <- FALSE

  # Specify available daily input variables
  # and prescribe Tmax, Tmin, PPT
  dif <- calc_dailyInputFlags(weatherData)
  dif[weatherGenerator_dataColumns()] <- TRUE
  sw_in@weather@dailyInputFlags <- dif


  #--- Process weather in SOILWAT2
  set.seed(seed)
  res <- .Call(C_rSW2_processAllWeather, weatherData, sw_in)

  if (isTRUE(as.logical(return_weatherDF[[1L]]))) {
    res <- dbW_weatherData_to_dataframe(res)
  }

  dbW_weatherData_round(res, digits = digits)
}





#' Impute missing weather values
#'
#' Impute missing weather values first by the weather generator
#' (for supported variables, see [weatherGenerator_dataColumns()]) and
#' second, if any missing values remain, using one of the imputation types
#' provided by [rSW2utils::impute_df()] for each variable separately.
#'
#' @inheritParams dbW_generateWeather
#' @param use_wg A logical value. Should the weather generator be used first?
#' @param method_after_wg A string. The imputation type passed
#' to [rSW2utils::impute_df()], if any missing values remain after the
#' weather generator.
#' @param nmax_run An integer value. Runs (sets of consecutive missing values)
#' that are equal or shorter to `nmax_run` are imputed;
#' longer runs remain unchanged. Passed to [rSW2utils::impute_df()].
#'
#' @return An updated copy of `weatherData` where missing values are imputed.
#' If `return_weatherDF` is `TRUE`, then a
#' data frame where columns represent weather variables is returned.
#' If `return_weatherDF` is `FALSE`, then the result is converted to a
#' a list of elements of class [`swWeatherData`].
#'
#' @section Details:
#' The weather generator (see [dbW_generateWeather()]) produces new values
#' for all implemented variables [weatherGenerator_dataColumns()] on days
#' where at least one of these variables is missing; this is to maintain
#' physical consistency among those variables.
#' This differs from the approach employed by `method_after_wg`
#' which imputes missing variables for each variable separately
#' (see [rSW2utils::impute_df()]).
#'
#' @seealso [rSW2utils::impute_df()], [dbW_generateWeather()]
#'
#' @examples
#' # Load example data
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#' dif <- c(rep(TRUE, 3L), rep(FALSE, 11L))
#' dif[13L] <- TRUE # ACTUAL_VP
#' dif[14L] <- TRUE # SHORT_WR, desc_rsds = 2
#' wdata <- getWeatherData_folders(
#'   LookupWeatherFolder = file.path(path_demo, "Input"),
#'   weatherDirName = "data_weather_daymet",
#'   filebasename = "weath",
#'   startYear = 1980,
#'   endYear = 1981,
#'   dailyInputFlags = dif,
#'   method = "C"
#' )
#' x0 <- x <- dbW_weatherData_to_dataframe(wdata)
#' dif0 <- calc_dailyInputFlags(x0)
#'
#' # Set June-August of 1980 as missing
#' ids_missing <- x[, "Year"] == 1980 & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
#' x[ids_missing, -(1:2)] <- NA
#'
#' x1 <- dbW_imputeWeather(x, return_weatherDF = TRUE)
#' x2 <- dbW_imputeWeather(x, method_after_wg = "none", return_weatherDF = TRUE)
#' x3 <- dbW_imputeWeather(
#'   x,
#'   use_wg = FALSE,
#'   method_after_wg = "locf",
#'   return_weatherDF = TRUE
#' )
#'
#' if (requireNamespace("graphics")) {
#'   ## Compare original vs imputed values for May-Sep of 1980
#'   ip <- x[, "Year"] == 1980 & x[, "DOY"] >= 123 & x[, "DOY"] <= 274
#'   doy <- x[ip, "DOY"]
#'
#'   vars <- names(dif0)[dif0]
#'   nr <- ceiling(sqrt(length(vars)))
#'
#'   par_prev <- graphics::par(mfrow = c(nr, ceiling(length(vars) / nr)))
#'
#'   for (k in seq_along(vars)) {
#'     graphics::plot(doy, x0[ip, vars[[k]]], ylab = vars[[k]], type = "l")
#'     graphics::lines(doy, x1[ip, vars[[k]]], col = "red", lty = 2L)
#'     graphics::lines(doy, x2[ip, vars[[k]]], col = "purple", lty = 3L)
#'     graphics::lines(doy, x3[ip, vars[[k]]], col = "darkgreen", lty = 3L)
#'     graphics::lines(doy, x0[ip, vars[[k]]], col = "gray")
#'   }
#'
#'   graphics::par(par_prev)
#' }
#'
#' @md
#' @export
dbW_imputeWeather <- function(
  weatherData,
  use_wg = TRUE,
  seed = NULL,
  method_after_wg = c("interp", "locf", "mean", "none", "fail"),
  nmax_run = Inf,
  elevation = NA,
  return_weatherDF = FALSE
) {

  method_after_wg <- match.arg(method_after_wg)

  if (method_after_wg == "interp" || isTRUE(is.finite(nmax_run))) {
    stopifnot(
      # `rSW2utils::impute_df()` v0.2.1 required for
      # type "interp" and argument "nmax_run"
      check_version(
        as.numeric_version(getNamespaceVersion("rSW2utils")),
        expected_version = "0.2.1",
        level = "patch"
      )
    )
  }

  if (dbW_check_weatherData(weatherData, check_all = FALSE)) {
    weatherData <- dbW_weatherData_to_dataframe(weatherData)
  }


  vars_wgen <- weatherGenerator_dataColumns()

  if (
    isTRUE(as.logical(use_wg)[[1L]]) &&
      any(is_missing_weather(weatherData[, vars_wgen, drop = FALSE]))
  ) {
    #--- Use weather generator for available variables
    tmp <- dbW_generateWeather(
      weatherData = weatherData,
      elevation = elevation,
      return_weatherDF = TRUE,
      seed = seed
    )

    weatherData[, vars_wgen] <- tmp[, vars_wgen]
  }

  #--- Imputation after first pass of weather generator
  vars_meteo <- intersect(weather_dataColumns(), colnames(weatherData))

  # see `calc_dailyInputFlags(weatherData, vars_meteo)`
  tmp_miss <- is_missing_weather(weatherData[, vars_meteo, drop = FALSE])
  tmp <- apply(!tmp_miss, MARGIN = 2L, FUN = any)
  vars_wv <- names(tmp)[tmp] # variables with values

  needs_im <- which(
    tmp_miss[, vars_wv, drop = FALSE],
    arr.ind = TRUE
  )

  if (NROW(needs_im) > 0) {
    if (method_after_wg == "fail") {
      stop(
        "Missing values in variables ",
        if (use_wg) "after weather generator ",
        "(method_after_wg = 'fail'): ",
        toString(vars_wv[unique(needs_im[, "col"])]),
        call. = FALSE
      )
    }

    weatherData[, vars_wv][needs_im] <- NA # set all types of missingness to NA

    tmp <- rSW2utils::impute_df(
      weatherData[, vars_wv, drop = FALSE],
      imputation_type = method_after_wg,
      nmax_run = nmax_run
    )

    weatherData[, vars_wv][needs_im] <- tmp[needs_im]
  }

  if (isTRUE(as.logical(return_weatherDF[[1L]]))) {
    weatherData
  } else {
    dbW_dataframe_to_weatherData(weatherData)
  }
}


#' Replace missing values with values from another weather data set
#'
#' @inheritParams dbW_generateWeather
#' @param subData A weather data object.
#' @param vars_substitute Names of variables for which missing values
#' in `weatherData` should be replaced by values from `subData`.
#' If `NULL`, then all weather variables (i.e., [weather_dataColumns()]).
#' @param by Names of variables used to match days (rows) between
#' `weatherData` and `subData`. If `NULL`, then all variables occurring both
#' in `weatherData` and `subData` that are not weather variables.
#' @param by_weatherData See `by`.
#' @param by_subData See `by`.
#' @param return_weatherDF A logical value. See section "Value".
#'
#' @return An updated copy of `weatherData` where missing values have been
#' replaced by corresponding values from `subData`.
#' If `return_weatherDF` is `TRUE`, then the result is converted to a
#' data frame where columns represent weather variables.
#' If `return_weatherDF` is `FALSE`, then the result is
#' a list of elements of class [`swWeatherData`].
#'
#' @seealso [dbW_generateWeather()], [dbW_imputeWeather()]
#'
#' @examples
#' # Load example data
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#' dif <- c(rep(TRUE, 3L), rep(FALSE, 11L))
#' dif[13L] <- TRUE # ACTUAL_VP
#' dif[14L] <- TRUE # SHORT_WR, desc_rsds = 2
#' wdata <- getWeatherData_folders(
#'   LookupWeatherFolder = file.path(path_demo, "Input"),
#'   weatherDirName = "data_weather_daymet",
#'   filebasename = "weath",
#'   startYear = 1980,
#'   endYear = 1981,
#'   dailyInputFlags = dif,
#'   method = "C"
#' )
#' x0 <- x <- dbW_weatherData_to_dataframe(wdata)
#' dif0 <- calc_dailyInputFlags(x0)
#'
#' # Set June-August of 1980 as missing
#' ids_1980 <- x[, "Year"] == 1980
#' ids_missing <- ids_1980 & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
#' x[ids_missing, -(1:2)] <- NA
#'
#' # Substitute missing values
#' all.equal(
#'   dbW_substituteWeather(x, x0[ids_1980, ], return_weatherDF = TRUE),
#'   x0
#' )
#'
#'
#' @md
#' @export
dbW_substituteWeather <- function(
  weatherData,
  subData,
  vars_substitute = NULL,
  by = NULL,
  by_weatherData = by,
  by_subData = by,
  return_weatherDF = FALSE
) {
  #--- Convert to data frames
  if (dbW_check_weatherData(weatherData, check_all = FALSE)) {
    weatherData <- dbW_weatherData_to_dataframe(weatherData)
  }

  if (dbW_check_weatherData(subData, check_all = FALSE)) {
    subData <- dbW_weatherData_to_dataframe(subData)
  }

  #--- Align days (rows) and variables (columns)
  vars_both <- intersect(colnames(weatherData), colnames(subData))

  if (is.null(vars_substitute)) {
    vars_req <- vars_both
  } else {
    vars_req <- intersect(vars_both, vars_substitute)
    if (length(vars_req) != length(vars_substitute)) {
      warning(
        "Not all requested variables present in both datasets.", call. = FALSE
      )
    }
  }

  vars_meteo <- intersect(vars_req, weather_dataColumns())

  if (is.null(by_weatherData)) {
    by_weatherData <- by_subData
  }

  if (is.null(by_subData)) {
    by_subData <- by_weatherData
  }

  if (is.null(by_weatherData) && is.null(by_subData)) {
    by_weatherData <- by_subData <- setdiff(vars_both, weather_dataColumns())
  }

  if (
    any(
      length(by_weatherData) == 0L,
      length(by_weatherData) != length(by_subData),
      !all(by_weatherData %in% colnames(weatherData)),
      !all(by_subData %in% colnames(subData))
    )
  ) {
    stop("Insufficient/bad information to match days.", call. = FALSE)
  }

  wdids <- do.call(
    paste,
    args = c(as.list(as.data.frame(weatherData)[by_weatherData]), sep = "-")
  )
  sdids <- do.call(
    paste,
    args = c(as.list(as.data.frame(subData)[by_subData]), sep = "-")
  )

  ids <- match(wdids, sdids, nomatch = 0L)
  idsnn <- ids > 0L

  if (!any(idsnn)) {
    warning("No matching days found.", call. = FALSE)
  }

  needsSub <- is_missing_weather(weatherData[idsnn, vars_meteo, drop = FALSE])

  weatherData[idsnn, vars_meteo][needsSub] <- subData[ids, vars_meteo][needsSub]


  #--- Return
  if (isTRUE(as.logical(return_weatherDF[[1L]]))) {
    weatherData
  } else {
    dbW_dataframe_to_weatherData(weatherData)
  }
}



#' Fix weather data
#'
#' Missing values are `"fixed"` with the following approach:
#'   1. `weatherData` is formatted for `rSOILWAT2`, i.e., converted to a
#'      Gregorian calendar and required but missing variables added.
#'   2. Short spells of missing values
#'      (consecutive days shorter than `nmax_interp`) are linearly interpolated
#'      from adjacent non-missing values
#'      (meta data tag `"interpolateLinear (<= X days)"`)
#'   3. Short spells of missing precipitation values are
#'      * Linearly interpolated if `precip_lt_nmax` is `NA`
#'      * Set to a fixed numeric value of `precip_lt_nmax`
#'        (meta data tag `"fixedValue"`)
#'      * Substituted with values from `subData` if `precip_lt_nmax` is `Inf`
#'        (see next point)
#'   4. Values from a second weather data object `subData` are used to replace
#'      (meta data tag `substituteData"`):
#'      * Missing precipitation values (if `precip_lt_nmax` is `Inf`)
#'      * Values before first day with any non-missing values
#'      * Variables absent in `weatherData` and present in `subData`
#'   5. Long-term daily means are used to replace any remaining missing values
#'      (meta data tag `"longTermDailyMean"`);
#'      for instance, this approach may be applied for
#'      * Values of variables that are present in `weatherData` and
#'        absent in `subData`, before first day with any non-missing values
#'      * Values after end of available values in both `weatherData` and
#'        `subData`
#'
#' @inheritParams dbW_substituteWeather
#' @inheritParams dbW_convert_to_GregorianYears
#' @param nmax_interp An integer value. Maximum spell length of missing values
#' for which linear interpolation is applied.
#' @param precip_lt_nmax A numeric value. Should short spells of
#' missing precipitation values be
#' linearly interpolated (if `NA`),
#' substituted with values from `subData` (if `Inf`), or
#' replaced by a fixed numeric value (default is 0)?
#'
#' @return A list with two named elements
#'   * `"weatherData"`: An updated copy of the input `weatherData`
#'     where missing values have been replaced.
#'     If `return_weatherDF` is `TRUE`, then the result is converted to a
#'     data frame where columns represent weather variables.
#'     If `return_weatherDF` is `FALSE`, then the result is
#'     a list of elements of class [`swWeatherData`].
#'   * `"meta"`: a data frame with the same dimensions as `"weatherData"`
#'     with tags indicating which approach was used to replaced missing values
#'     in corresponding cells of `weatherData` (see section `Details`)
#'
#' @seealso [dbW_imputeWeather()], [dbW_substituteWeather()],
#' [dbW_generateWeather()]
#'
#' @examples
#' x0 <- x <- dbW_weatherData_to_dataframe(rSOILWAT2::weatherData)
#'
#' tmp <- x[, "Year"] == 1981
#' ids_to_interp <- tmp & x[, "DOY"] >= 144 & x[, "DOY"] <= 145
#' x[ids_to_interp, -(1:2)] <- NA
#'
#' tmp <- x[, "Year"] == 1980
#' ids_to_sub <- tmp & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
#' x[ids_to_sub, -(1:2)] <- NA
#'
#' xf <- dbW_fixWeather(x, x0, return_weatherDF = TRUE)
#' all.equal(
#'   xf[["weatherData"]][!ids_to_interp, ],
#'   as.data.frame(x0)[!ids_to_interp, ]
#' )
#' table(xf[["meta"]])
#'
#' @md
#' @export
dbW_fixWeather <- function(
  weatherData,
  subData = NULL,
  new_startYear = NULL,
  new_endYear = NULL,
  nmax_interp = 7L,
  precip_lt_nmax = 0,
  return_weatherDF = FALSE
) {
  nmax_interp <- as.integer(nmax_interp)
  vars_time <- c("Year", "DOY")

  #--- Convert to data frames and add missing variables (if any)
  weatherData <- if (dbW_check_weatherData(weatherData, check_all = FALSE)) {
    dbW_weatherData_to_dataframe(weatherData)
  } else {
    upgrade_weatherDF(weatherData)
  }

  stopifnot(
    c(vars_time, weather_dataColumns()) %in% colnames(weatherData)
  )

  #--- Locate years
  if (is.null(new_startYear) || is.null(new_endYear)) {
    years <- get_years_from_weatherDF(
      weatherDF = weatherData,
      years = NULL
    )[["years"]]

    if (is.null(new_startYear)) new_startYear <- years[[1L]]
    if (is.null(new_endYear)) new_endYear <- years[[length(years)]]
  }


  #--- Add missing days to complete full requested calendar years
  weatherData1 <- dbW_convert_to_GregorianYears(
    weatherData = weatherData,
    new_startYear = new_startYear,
    new_endYear = new_endYear,
    type = "asis"
  )

  is_miss1 <- is_missing_weather(weatherData1[, weather_dataColumns()])
  meta <- array(dim = dim(is_miss1), dimnames = dimnames(is_miss1))


  #--- Determine first and last day with at least one observation
  tmp <- which(rowSums(!is_miss1) > 0L)
  ids <- tmp[c(1L, length(tmp))]

  ids_startend <- if (length(tmp) > 0L) {
    # before start
    (weatherData1[["Year"]] < weatherData1[ids[[1L]], "Year"]) |
      (weatherData1[["Year"]] == weatherData1[ids[[1L]], "Year"] &
          weatherData1[["DOY"]] < weatherData1[ids[[1L]], "DOY"]) |
      # after end
      (weatherData1[["Year"]] == weatherData1[ids[[2L]], "Year"] &
          weatherData1[["DOY"]] > weatherData1[ids[[2L]], "DOY"]) |
      (weatherData1[["Year"]] > weatherData1[ids[[2L]], "Year"])
  }


  #--- Interpolate short missing runs
  weatherData2 <- suppressWarnings(
    dbW_imputeWeather(
      weatherData1,
      use_wg = FALSE,
      method_after_wg = "interp",
      nmax_run = nmax_interp,
      return_weatherDF = TRUE
    )
  )

  # special treatment of precipitation
  #   (NA = interpolate; x = fixedValue; Inf = subData)
  is_pptFixedValue <- NULL

  if (!isTRUE(is.na(precip_lt_nmax[[1L]]))) {

    if (isTRUE(is.finite(precip_lt_nmax[[1L]]))) {
      # Find linear interpolated precip values and replace with fixed value
      ppt_miss2a <- is_missing_weather(weatherData2[, "PPT_cm"])
      is_pptFixedValue <- which(!ppt_miss2a[, 1L] & is_miss1[, "PPT_cm"])
      weatherData2[["PPT_cm"]][is_pptFixedValue] <- precip_lt_nmax[[1L]]

    } else {
      # Reset for replacement by subData in following step
      weatherData2[["PPT_cm"]] <- weatherData1[["PPT_cm"]]
    }
  }

  # Set values outside original time periods to missing
  weatherData2[ids_startend, weather_dataColumns()] <- NA

  is_miss2 <- is_missing_weather(weatherData2[, weather_dataColumns()])
  meta[!is_miss2 & is_miss1] <- sprintf(
    "interpolateLinear (<= %d days)",
    nmax_interp
  )

  if (length(is_pptFixedValue) > 0L) {
    meta[is_pptFixedValue, "PPT_cm"] <- "fixedValue"
  }


  #--- Use subData
  # * for missing values before first observation day
  # * for variables missing weatherData but available in subData
  # * for missing precipitation
  if (is.null(subData)) {
    weatherData3 <- weatherData2
    is_miss3 <- is_miss2

  } else {
    subData <- if (dbW_check_weatherData(subData, check_all = FALSE)) {
      dbW_weatherData_to_dataframe(subData)
    } else {
      upgrade_weatherDF(subData)
    }

    stopifnot(
      c(vars_time, weather_dataColumns()) %in% colnames(subData)
    )

    subData1 <- dbW_convert_to_GregorianYears(
      weatherData = subData,
      new_startYear = new_startYear,
      new_endYear = new_endYear,
      type = "asis"
    )

    weatherData3 <- dbW_substituteWeather(
      weatherData = weatherData2,
      subData = subData1,
      return_weatherDF = TRUE
    )

    is_miss3 <- is_missing_weather(weatherData3[, weather_dataColumns()])
    meta[!is_miss3 & is_miss2] <- "substituteData"
  }


  #--- Use long-term daily means to impute the rest
  # - for variables not in subData before first observed value in weatherData
  # - values after end of observed values in weatherData
  dif_wd3 <- rSOILWAT2::calc_dailyInputFlags(weatherData3)
  vars_wd3 <- names(dif_wd3)[dif_wd3]

  if (any(is_missing_weather(weatherData3[, vars_wd3]))) {
    daymeans <- data.frame(
      Year = NA,
      aggregate(
        weatherData1[, weather_dataColumns()],
        by = weatherData1["DOY"],
        FUN = mean,
        na.rm = TRUE
      )
    )


    if (!is.null(subData)) {
      dif_wd <- rSOILWAT2::calc_dailyInputFlags(weatherData1)
      dif_sd <- rSOILWAT2::calc_dailyInputFlags(subData)

      tmp_vars <- setdiff(names(dif_sd)[dif_sd], names(dif_wd)[dif_wd])

      if (length(tmp_vars) > 0L) {
        sd_daymeans <- data.frame(
          Year = NA,
          aggregate(
            subData[, weather_dataColumns()],
            by = subData["DOY"],
            FUN = mean,
            na.rm = TRUE
          )
        )

        daymeans[, tmp_vars] <- sd_daymeans[, tmp_vars]
      }
    }

    # Linear interpolate any missing long-term daily means
    daymeans2 <- suppressWarnings(
      dbW_imputeWeather(
        daymeans,
        use_wg = FALSE,
        method_after_wg = "interp",
        nmax_run = Inf,
        return_weatherDF = TRUE
      )
    )

    # Create object with long-term daily means repeated for each requested year
    daymeans_years <- do.call(
      rbind,
      args = lapply(
        seq(new_startYear, new_endYear),
        function(year) {
          tmp <- daymeans2
          tmp[["Year"]] <- year
          tmp
        }
      )
    )

    weatherData4 <- dbW_substituteWeather(
      weatherData = weatherData3,
      subData = daymeans_years,
      return_weatherDF = TRUE
    )

    is_miss4 <- is_missing_weather(weatherData4[, weather_dataColumns()])
    meta[!is_miss4 & is_miss3] <- "longTermDailyMean"

  } else {
    weatherData4 <- weatherData3
  }


  #--- Return
  list(
    weatherData = if (isTRUE(as.logical(return_weatherDF[[1L]]))) {
      weatherData4
    } else {
      dbW_dataframe_to_weatherData(weatherData4)
    },
    meta = meta
  )
}
