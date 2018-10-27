#------ FUNCTIONS THAT DEAL WITH SIMULATION TIME

isLeapYear <- function(y) {
  #from package: tis
  y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
}

#' Create a sequence of each day between and including two calendar years
#'
#' @param start_year An integer value. The first year.
#' @param end_year An integer value. The last year.
#'
#' @examples
#' length(days_in_years(1980, 1980)) == 366
#'
#' @export
days_in_years <- function(start_year, end_year) {
  seq(from = ISOdate(start_year, 1, 1, tz = "UTC"),
      to = ISOdate(end_year, 12, 31, tz = "UTC"), by = "1 day")
}

#' The sequence of month numbers for each day in the period from - to
#'
#' @examples
#' \dontrun{
#'  month1 <- function() as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
#'     to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1
#'  month2 <- function() seq_month_ofeach_day(list(1980, 1, 1),
#'    list(2010, 12, 31), tz = "UTC")
#'
#'    if (requireNamespace("microbenchmark", quietly = TRUE))
#'      # barely any difference
#'      microbenchmark::microbenchmark(month1(), month2())
#'  }
seq_month_ofeach_day <- function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS",
    tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
}




#' Calculate indices along different time steps for simulation time
#'
#'
#' @param useyrs A numeric vector. The ordered vector of simulation years.
#' @param sim_tscales A vector of character strings. One or multiple of
#'   \code{c("daily", "weekly", "monthly", "yearly")}.
#' @param use_doy_range A logical value. If \code{TRUE}, then the result is
#'   additional daily indices indicating whether the \var{DOY} is within the
#'   days indicated in the \code{doy_ranges}.
#' @param doy_ranges A named list. Aggregation output variables and the daily
#'   \code{c(min, max)} of days you wish to calculate the aggregation over.
#' @param latitude A numeric value. The latitude in decimal degrees for which a
#'   hemispheric adjustment is requested; however, the code extracts only the
#'   sign. Positive values are interpreted as from the northern hemisphere;
#'   negative latitudes as from the southern hemisphere.
#' @param account_NorthSouth A logical value. If \code{TRUE}, then the result is
#'   corrected for locations on the southern vs. northern hemisphere.
#' @return A named list.
#'
#' @export
simTiming_ForEachUsedTimeUnit <- function(useyrs,
  sim_tscales = c("daily", "weekly", "monthly", "yearly"),
  use_doy_range = FALSE,  doy_ranges = list(),
  latitude = 90, account_NorthSouth = TRUE) {

  res <- list()

  no_useyr <- length(useyrs)

  if (any(sim_tscales == "daily")) {
    temp <- as.POSIXlt(days_in_years(min(useyrs), max(useyrs)))

    res$doy_ForEachUsedDay <- temp$yday + 1
    res$month_ForEachUsedDay <- temp$mon + 1
    res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900

    if (latitude < 0 && account_NorthSouth) {
      #- Shift doys
      # New month either at end of year or in the middle because the two
      # halfs (6+6 months) of a year are of unequal length
      # (182 (183 if leap year) and 183 days): I chose to have a new month at
      # end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec;
      # but, 1 Jan -> July 3/4): and instead of a day with doy = 366,
      # there are two with doy = 182
      dshift <- as.POSIXlt(ISOdate(useyrs, 6, 30, tz = "UTC"))$yday + 1
      res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq_along(useyrs),
        function(x) {
          temp <- useyrs[x] == res$year_ForEachUsedDay
          temp1 <- res$doy_ForEachUsedDay[temp]
          temp2 <- 1:dshift[x]
          c(temp1[-temp2], temp1[temp2])
        }))

      #- Shift months
      temp <- paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj,
        sep = "-")
      res$month_ForEachUsedDay_NSadj <- strptime(temp, format = "%Y-%j")$mon + 1

      #- Shift years
      temp1 <- length(res$year_ForEachUsedDay)
      delta <- if (dshift[1] == 182) 2 else 3
      temp2 <- dshift[1] + delta
      res$year_ForEachUsedDay_NSadj <- c(
        # add previous calendar year for shifted days of first simulation year
        rep(useyrs[1] - 1, times = temp2),
        # remove a corresponding number of days at end of simulation period
        res$year_ForEachUsedDay[- ((temp1 - temp2 + 1):temp1)]
      )
      res$useyrs_NSadj <- unique(res$year_ForEachUsedDay_NSadj)
      res$no.useyr_NSadj <- length(res$useyrs_NSadj)

    } else {
      res$doy_ForEachUsedDay_NSadj <- res$doy_ForEachUsedDay
      res$month_ForEachUsedDay_NSadj <- res$month_ForEachUsedDay
      res$year_ForEachUsedDay_NSadj <- res$year_ForEachUsedDay
      res$useyrs_NSadj <- useyrs
      res$no.useyr_NSadj <- no_useyr
    }

    #Adjust years to water-years
    # In North, Water year starting Oct 1 - Using DOY 274, which is Oct 1st in
    #   Leap Years, but Oct 2nd in typical years
    # In South, Water year starting April 1 - Using DOY 92, which is April 1st
    #   in Leap Years, but April 2nd in typical years

    temp <- res$doy_ForEachUsedDay[1] == res$doy_ForEachUsedDay_NSadj[1]
    FirstDOY_WaterYear <- if (temp) 274 else 92

    temp <- res$doy_ForEachUsedDay_NSadj > FirstDOY_WaterYear
    res$year_ForEachUsedDay_NSadj_WaterYearAdj <- # nolint
      res$year_ForEachUsedDay_NSadj + ifelse(temp, 1, 0)

    if (isTRUE(use_doy_range)) {
      # North or Southern hemisphere? eliminate unnecessary water years values
      if (latitude > 0) {
        Idx <- grep("_S", names(doy_ranges))
        doy_ranges[Idx] <- NULL
      } else {
        Idx <- grep("_N", names(doy_ranges))
        doy_ranges[Idx] <- NULL
      }

      for (dr in seq_along(doy_ranges)) {
        if (!is.null(doy_ranges[[dr]])) {
          # for all non-NULL doy_range values
          doy_range_values <- doy_ranges[[dr]]

          # Create daily logical vector indicating whether that doy is within
          # range or not
          res[[paste0("doy_NSadj_", names(doy_ranges[dr]), "_doyRange")]] <-
            if (doy_range_values[1] > doy_range_values[2]) {
              temp <- c(doy_range_values[1]:366, 1:doy_range_values[2])
              res$doy_ForEachUsedDay_NSadj %in% temp
            } else {
              temp <- doy_range_values[1]:doy_range_values[2]
              res$doy_ForEachUsedDay_NSadj %in% temp
            }
        }
      }
    }
  }



  if (any(sim_tscales == "weekly")) {

  }

  if (any(sim_tscales == "monthly")) {
    res$yearno_ForEachUsedMonth <- res$yearno_ForEachUsedMonth_NSadj <-
      rep(seq_len(no_useyr), each = 12)
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <-
      rep(rSW2_glovars[["st_mo"]], times = no_useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <-
        (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
}


#' Check requested years
#'
#' @param start_year An integer value. The requested first year to extract
#'   weather data.
#' @param end_year An integer value. The requested last year to extract weather
#'   data.
#' @param has_start_year An integer value. The available first year of the
#'   weather data.
#' @param has_end_year An integer value. The available last year of the weather
#'   data.
#' @param temp_call A character string. An identifier of the calling function
#'   used for printing.
#' @param verbose A logical value. If \code{TRUE} prints statements if first or
#'   last year were updated.
#'
#' @return A list with two named elements \itemize{ \item \code{start_year} to
#'   updated first year no smaller than \code{has_start_year} \item
#'   \code{end_year} to updated last year no larger than \code{has_end_year} }
#'
#' @export
update_requested_years <- function(start_year, end_year, has_start_year,
  has_end_year, temp_call = NULL, verbose = FALSE) {

  if (start_year < has_start_year) {
    if (verbose) {
      print(paste0(shQuote(temp_call), ": covers years ", has_start_year, "-",
        has_end_year, "; requested start year ", start_year, " was changed to ",
        has_start_year, "."))
    }
    start_year <- as.integer(has_start_year)

  } else {
    start_year <- as.integer(start_year)
  }

  if (end_year > has_end_year) {
    if (verbose) {
      print(paste0(shQuote(temp_call), ": covers years ", has_start_year, "-",
        has_end_year, "; requested end year ", end_year, " was changed to ",
        has_end_year, "."))
    }
    end_year <- as.integer(has_end_year)

  } else {
    end_year <- as.integer(end_year)
  }


  list(start_year = start_year, end_year = end_year)
}
