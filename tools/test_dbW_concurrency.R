#--- Test concurrent writers/readers of a weather database ------

# Required packages not part of `rSOILWAT2`
stopifnot(
  requireNamespace("callr"),
  requireNamespace("ggplot2")
)


# Note:
# Concurrent write/read processes will use the installed version of `rSOILWAT2`

#--- Setup ------
verbose <- FALSE
t_test_duration <- 60 # observe for X seconds


N_writers <- 4 # number of concurrent writers
N_readers <- 40 # number of concurrent readers

rwouts <- lapply(seq_len(N_writers), function(k) tempfile())
rrouts <- lapply(seq_len(N_readers), function(k) tempfile())


#--- Size of weather database
# Balance size with number of readers and writers and duration of test
#   - too small relative to writers --> they soon run out of stuff to write
#   - too large relative to readers --> they don't have enough to read
N_sites <- 1000
N_scenarios <- 5


# Weather database content
scenarios <- c("Current", paste0("Scenario", seq_len(N_scenarios)))

req_cols <- c("Latitude", "Longitude", "Label")
site_ids <- seq_len(N_sites)
site_data <- data.frame(
  Site_id = site_ids,
  Latitude = -45 + sample(site_ids, N_sites) * 90 / N_sites,
  Longitude = -100 + sample(site_ids, N_sites) * 200 / N_sites,
  Label = paste0("TestSite_id", site_ids),
  stringsAsFactors = FALSE
)



#--- Functions ------
# Function to monitor progress and status of writers/readers
get_out_msg <- function(x, tag, count, verbose) {
  counts <- lapply(
    count,
    function(pattern) sum(grepl(pattern, x))
  )

  if (verbose) {
    message(
      tag, ": ",
      paste(names(counts), "=", counts, collapse = " | ")
    )
  }

  counts
}

calc_rates_per_sampling <- function(data, cats) {
  tmp <- lapply(
    cats,
    function(x) {
      aggregate(data[, x], by = data["Process_ID"], FUN = diff)
    }
  )

  res <- as.data.frame(sapply(tmp, function(x) as.vector(x$x)))
  colnames(res) <- cats
  tmp_dim <- dim(tmp[[1]]$x)

  data.frame(
    Process = rep(seq_len(tmp_dim[1]), times = tmp_dim[2]),
    Sampling = rep(seq_len(tmp_dim[2]), each = tmp_dim[1]),
    res
  )
}



# Function to continuously write to database
# It writes a message with tag `: write:` or `: failed-write:`
dbwriter <- function(wid, fdbWeather, N_sites, N_scenarios) {

  rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  set.seed(wid)

  while (TRUE) {
    Sys.sleep(runif(1, max = 0.1))

    id_site <- sample(N_sites, 1)
    id_scen <- sample(N_scenarios, 1)

    do_it <- try(
      rSOILWAT2::dbW_has_weatherData(
        Site_ids = id_site,
        Scenario_ids = id_scen
      ),
      silent = TRUE
    )

    if (!inherits(do_it, "try-error") && isFALSE(do_it)) {
      tmp_sw_weather <- rSOILWAT2::weatherData

      # Encode unique information
      tmp_sw_weather[[1]]@data[1, 1:3] <- c(wid, id_site, id_scen)

      tmp <- try(
        rSOILWAT2::dbW_addWeatherData(
          Site_id = id_site,
          weatherData = tmp_sw_weather,
          Scenario_id = id_scen,
          overwrite = TRUE,
          verbose = FALSE # TRUE
        ),
        silent = FALSE
      )

      if (isTRUE(tmp)) {
        message(Sys.time(), ": write: ", paste(wid, id_site, id_scen, sep = "/"))
      } else {
        message(Sys.time(), ": failed-write: ", shQuote(tmp))
      }
    }
  }

  rSOILWAT2::dbW_disconnectConnection()
}



# Function to continuously read from database
# It writes a message with tag `: read:` or `: failed-read:`
dbreader <- function(rid, fdbWeather, N_sites, N_scenarios) {

  rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  set.seed(rid)

  while (TRUE) {
    Sys.sleep(runif(1, max = 0.1))
    id_site <- sample(N_sites, 1)
    id_scen <- sample(N_scenarios, 1)

    do_it <- try(
      rSOILWAT2::dbW_has_weatherData(
        Site_ids = id_site,
        Scenario_ids = id_scen
      ),
      silent = TRUE
    )

    if (!inherits(do_it, "try-error") && isTRUE(do_it)) {
      tmp <- try(
        rSOILWAT2::dbW_getWeatherData(
          Site_id = id_site,
          Scenario_id = id_scen
        ),
        silent = TRUE
      )

      if (!inherits(tmp, "try-error")) {
        message(Sys.time(), ": read: ", paste(rid, id_site, id_scen, sep = "/"))
      } else {
        message(Sys.time(), ": failed-read: ", shQuote(tmp))
      }
    }
  }

  rSOILWAT2::dbW_disconnectConnection()
}


#--- Start tests ------
set.seed(127)

test_that("Concurrent read/write weather database", {
  #--- Create database ------
  # These tests assume an empty database so that writers can add new data

  fdbWeather <- tempfile(fileext = ".sqlite3")
  tmp_args <- list(
    fdbWeather = fdbWeather, N_sites = N_sites, N_scenarios = N_scenarios
  )
  rSOILWAT2::dbW_createDatabase(
    fdbWeather,
    site_data = site_data,
    Scenarios = scenarios,
    scen_ambient = scenarios[1]
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), fdbWeather)
  tmp <- DBI::dbExecute(con, "PRAGMA journal_mode = WAL")


  #--- Simulate concurrent writers and readers ------
  rws <- lapply(
    seq_len(N_writers),
    function(k) {
      callr::r_bg(
        dbwriter,
        args = c(tmp_args, wid = k),
        stdout = rwouts[[k]],
        stderr = rwouts[[k]], # "2>&1"
        supervise = TRUE
      )
    }
  )

  rrs <- lapply(
    seq_len(N_readers),
    function(k) {
      callr::r_bg(
        dbreader,
        args = c(tmp_args, rid = k * 10),
        stdout = rrouts[[k]],
        stderr = rrouts[[k]], # "2>&1"
        supervise = TRUE
      )
    }
  )


  #--- Observe progress of writers and readers ------
  rSOILWAT2::dbW_setConnection(fdbWeather)

  res_writers <- res_readers <- list()
  kw <- kr <- 1

  t_start <- Sys.time()

  while (t_test_duration > difftime(Sys.time(), t_start, units = "sec")) {
    Sys.sleep(1)

    if (verbose) {
      tmp <- rSOILWAT2::dbW_has_weatherData(
        Site_ids = seq_len(N_sites),
        Scenario_ids = seq_len(N_scenarios)
      )
      message(Sys.time(), ": n(data) = ", sum(tmp))
    }

    for (k in seq_along(rwouts)) {
      res_writers[[kw]] <- c(
        Process = "Writer",
        Process_ID = k,
        time_delta = difftime(Sys.time(), t_start, units = "sec"),
        get_out_msg(
          readLines(con = rwouts[[k]]),
          tag = paste("Writer", k),
          count = list(
            success = ": write:",
            failure = ": failed-write:",
            locked = "Error : database is locked"
          ),
          verbose = verbose
        )
      )
      kw <-  kw + 1
    }

    for (k in seq_along(rrouts)) {
      res_readers[[kr]] <- c(
        Process = "Reader",
        Process_ID = k,
        time_delta = difftime(Sys.time(), t_start, units = "sec"),
        get_out_msg(
          readLines(con = rrouts[[k]]),
          tag = paste("Reader", k),
          count = list(
            success = ": read:",
            failure = ": failed-read:",
            locked = "Error : database is locked"
          ),
          verbose = verbose
        )
      )
      kr <- kr + 1
    }
  }

  rSOILWAT2::dbW_disconnectConnection()


  #--- Clean processes ------
  # Note: these processes often don't die easily
  tmp <- sapply(rws, function(r) r$kill())
  tmp <- tools::pskill(
    sapply(rws, function(r) r$get_pid()),
    signal = tools::SIGKILL
  )

  tmp <- sapply(rrs, function(r) r$kill())
  tmp <- tools::pskill(
    sapply(rrs, function(r) r$get_pid()),
    signal = tools::SIGKILL
  )


  #--- Convert output logs to data.frames
  res_writers <- do.call(rbind, res_writers)
  res_writers2 <- data.frame(
    array(dim = dim(res_writers), dimnames = list(NULL, colnames(res_writers)))
  )
  for (k in colnames(res_writers)) {
    res_writers2[, k] <- unlist(res_writers[, k])
  }


  res_readers <- do.call(rbind, res_readers)
  res_readers2 <- data.frame(
    array(dim = dim(res_readers), dimnames = list(NULL, colnames(res_readers)))
  )
  for (k in colnames(res_readers)) {
    res_readers2[, k] <- unlist(res_readers[, k])
  }


  #--- Calculate rates per sampling event ------
  tmp_cats <- c("success", "failure", "locked")

  wrates <- calc_rates_per_sampling(res_writers2, tmp_cats)
  rrates <- calc_rates_per_sampling(res_readers2, tmp_cats)

  grates <- rbind(
    cbind(Action = "Read", reshape2::melt(rrates, measure.vars = tmp_cats)),
    cbind(Action = "Write", reshape2::melt(wrates, measure.vars = tmp_cats))
  )
  tmp_var <- c("failure", "locked")
  ids <- grates[, "variable"] %in% tmp_var & grates[, "value"] == 0
  grates[ids, "value"] <- NA


  #--- Test expectations ------

  #--- Test that no failures or locked database
  expect_equal(
    sum(
      grates[grates[, "variable"] %in% c("failure", "locked"), "value"],
      na.rm = TRUE
    ),
    0
  )

  #--- Test that no read process is stuck at 0 success rate
  # particularly later on
  # (read success rate may be 0 early on because there is nothing yet
  # available to read)
  ids <- grates[, "Action"] == "Read" &
    grates[, "variable"] == "success" &
    grates[, "Sampling"] >= 4 / 5 * max(grates[, "Sampling"])

  expect_false(any(aggregate(
    grates[ids, "value"],
    by = list(grates[ids, "Process"]),
    function(x) {
      tmp <- rle(x == 0)
      if (any(tmp[["values"]])) {
        any(tmp[["lengths"]][tmp[["values"]]] > 1)
      } else {
        FALSE
      }
    }
  )[, "x"]))


  #--- Test that no write process is stuck at 0 success rate
  # particularly early on
  # (write success rate may be 0 later on because all/most data exist)
  ids <- grates[, "Action"] == "Write" &
    grates[, "variable"] == "success" &
    grates[, "Sampling"] <= 1 / 5 * max(grates[, "Sampling"])

  expect_false(any(aggregate(
    grates[ids, "value"],
    by = list(grates[ids, "Process"]),
    function(x) {
      tmp <- rle(x == 0)
      if (any(tmp[["values"]])) {
        any(tmp[["lengths"]][tmp[["values"]]] > 1)
      } else {
        FALSE
      }
    }
  )[, "x"]))


  #--- Visualize expectations ------
  tmp <- ggplot2::ggplot(grates) +
    ggplot2::facet_grid(~ Action) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = jitter(Sampling),
        y = jitter(value),
        col = variable
      ),
      alpha = 0.7
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::ylab("# new events") +
    ggplot2::theme_minimal()

  suppressWarnings(plot(tmp))


  #--- Clean up ------
  # Delete logs
  tmp <- unlink(unlist(rwouts))
  tmp <- unlink(unlist(rrouts))

  # Clean up database
  tmp <- DBI::dbExecute(con, "PRAGMA optimize")
  tmp <- DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
  tmp <- DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
  tmp <- DBI::dbDisconnect(con)

  tmp <- unlink(fdbWeather)
})
