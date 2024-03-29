
#--- INPUTS ------
path_extdata <- file.path("..", "..", "inst", "extdata")
if (!dir.exists(path_extdata)) {
  path_extdata <- system.file("extdata", package = "rSOILWAT2")
}

fdbWeather <- tempfile(fileext = ".sqlite3")
fdbWeather2 <- tempfile(fileext = ".txt")
write(NA, file = fdbWeather2)
fdbWeather3 <- file.path("/Fantasy", "Volume", "test.sqlite3")

dir_test_data <- file.path("..", "test_data")
tmp <- list.files(dir_test_data, pattern = "Ex")
tmp <- sapply(strsplit(tmp, "_", fixed = TRUE), function(x) x[[1]])
tests <- unique(tmp)
test_that("Test data availability", {
  expect_gt(length(tests), 0)
})

sw_weather <- lapply(
  tests,
  function(it) readRDS(file.path(dir_test_data, paste0(it, "_weather.rds")))
)
sw_years <- lapply(
  sw_weather,
  function(x) range(get_years_from_weatherData(x))
)
scenarios <- c("Current", paste0("TestScenario", tests))
scenarios_added <- c(
  scenarios,
  paste0(scenarios[1], "_new"),
  tolower(scenarios[3])
)

req_cols <- c("Latitude", "Longitude", "Label")
site_N <- 5
site_ids <- seq_len(site_N)
site_data1 <- data.frame(
  Site_id = site_ids,
  Latitude = 40 + site_ids,
  Longitude = -100 - site_ids,
  Label = paste0("TestSite_id", site_ids),
  stringsAsFactors = FALSE
)

i_update2 <- 2
site_data2 <- site_data1
site_data2[i_update2, "Label"] <- NA

site_data3 <- data.frame(
  Site_id = site_N + site_ids,
  Latitude = 40 + site_N + site_ids,
  Longitude = -100 - site_N + site_ids,
  Label = paste0("TestSite_id", site_N + site_ids),
  stringsAsFactors = FALSE
)



# This function is needed for appveyor: for some reason 'dbW_createDatabase'
# doesn't remove (in any situation) failed disk files 'fdbWeather'; this is not
# a problem on travis or on my local macOS
# It doesn't work on Github Actions Windows 64bit
unlink_forcefully <- function(x, recursive = TRUE, force = TRUE, info = NULL) {
  if (file.exists(x)) {
    message(
      info, ": file ", x, " should not exists, but it does - ",
      "so we delete it."
    )
    unlink(x, recursive = recursive, force = force)
  }
  if (file.exists(x)) {
    message(
      info, ": file ", x, " should not exists because we just ",
      "attempted to delete it."
    )
  } else {
    message(info, ": file ", x, " sucessfully deleted.")
  }
}


#---TESTS
test_that("Disk file write and delete permissions", {
  # remove once issue #43 is fixed (unit tests for 'test_dbW_functionality.R'
  # fail on appveyor but not on travis)
  skip_on_appveyor()
  skip_on_os("windows")

  tmp <- try(write(NA, file = fdbWeather), silent = TRUE)
  has_fdbW <- !inherits(tmp, "try-error") && file.exists(fdbWeather)
  expect_true(
    has_fdbW,
    info = paste("Failed to create file", fdbWeather)
  )

  tmp <- try(
    suppressMessages(unlink_forcefully(fdbWeather, info = "1st")),
    silent = TRUE
  )

  hasnot_fdbW <- !inherits(tmp, "try-error") && !file.exists(fdbWeather)
  expect_true(
    hasnot_fdbW,
    info = paste("Failed to create file", fdbWeather)
  )
})



#--- Unit: dbW creation ------
test_that("dbW creation", {
  # remove once issue #43 is fixed (unit tests for 'test_dbW_functionality.R'
  # fail on appveyor but not on travis)
  skip_on_appveyor()
  skip_on_os("windows")

  #--- * Attempt to connect to (no) weather database ------
  unlink(fdbWeather)
  expect_false(dbW_setConnection(fdbWeather, create_if_missing = FALSE))
  expect_message(
    dbW_setConnection(fdbWeather, create_if_missing = FALSE, verbose = TRUE),
    regexp = "does not exist"
  )
  expect_true(
    dbW_setConnection(fdbWeather, create_if_missing = TRUE, verbose = FALSE)
  )
  expect_true(file.exists(fdbWeather))
  expect_true(dbW_IsValid())
  unlink(fdbWeather)

  expect_false(dbW_setConnection(fdbWeather2, create_if_missing = TRUE))
  expect_message(
    dbW_setConnection(fdbWeather2, create_if_missing = TRUE, verbose = TRUE),
    regexp = "exists but is likely not a SQLite-database"
  )
  expect_false(dbW_setConnection(fdbWeather3, create_if_missing = TRUE))
  expect_false(
    dbW_setConnection(fdbWeather3, create_if_missing = TRUE, verbose = FALSE)
  )
  expect_false(file.exists(fdbWeather3))
  expect_false(dbW_IsValid())


  #--- * Create weather database and check that connection ------
  # Warnings: arguments ignored/deprecated 'ARG_DOESNT_EXIST'
  expect_true(
    suppressMessages(
      dbW_createDatabase(
        fdbWeather,
        site_data = site_data1,
        Scenarios = scenarios,
        scen_ambient = scenarios[1],
        verbose = FALSE,
        ARG_DOESNT_EXIST = 1:3
      )
    )
  )
  expect_true(file.exists(fdbWeather))
  expect_true(dbW_IsValid())
  unlink(fdbWeather)

  # Minimal inputs
  expect_true(dbW_createDatabase(fdbWeather))
  # expect that deletion does not result in an error
  expect_false(
    inherits(
      try(
        suppressMessages(unlink_forcefully(fdbWeather, info = "1st")),
        silent = TRUE
      ),
      "try-error"
    )
  )


  # this is a warning coming from 'normalizePath':
  #   - on 'unix': regexp = "No such file or directory"
  #   - on 'windows': regexp = "The system cannot find the path specified" or
  #     similar
  expect_warning(
    dbW_createDatabase(
      fdbWeather3,
      site_data = site_data1,
      Scenarios = scenarios,
      scen_ambient = scenarios[1]
    )
  )
  expect_message(
    unlink_forcefully(fdbWeather, info = "3rd"),
    regexp = "sucessfully deleted"
  )
  expect_false(
    dbW_createDatabase(
      fdbWeather,
      site_data = NA,
      Scenarios = scenarios,
      scen_ambient = scenarios[1]
    )
  )
  expect_message(
    unlink_forcefully(fdbWeather, info = "4th"),
    regexp = "sucessfully deleted"
  )

  # 'dbW_createDatabase': deletes db-file due to failure.
  dbW_createDatabase(
    fdbWeather,
    site_data = NA,
    Scenarios = scenarios,
    scen_ambient = scenarios[1],
    verbose = FALSE
  )
  expect_false(file.exists(fdbWeather))
  expect_false(dbW_IsValid())
  unlink(fdbWeather)

  expect_true(
    dbW_createDatabase(
      fdbWeather,
      site_data = site_data1,
      Scenarios = scenarios,
      scen_ambient = scenarios[1]
    )
  )
  expect_true(dbW_setConnection(fdbWeather))
  expect_true(dbW_IsValid())
  expect_true(.dbW_setConnection(fdbWeather))
  expect_true(dbW_IsValid())
  expect_true(dbW_disconnectConnection())
  expect_false(dbW_IsValid())
  expect_true(dbW_setConnection(fdbWeather))

  #--- Check on status of weather database
  expect_equal(dbW_version(), numeric_version(rSW2_glovars$dbW_version))
  expect_equal(dbW_compression(), rSW2_glovars$default_blob_compression_type)

  #--- Check on site/scenario tables
  expect_equal(dbW_getSiteTable()[, req_cols], site_data1[, req_cols])
  expect_equal(dbW_getScenariosTable()[, "Scenario"], scenarios)

  #--- Check on site/scenario table content
  expect_true(all(dbW_has_sites(site_data1[, "Label"])))
  expect_true(all(dbW_has_siteIDs(site_ids)))
  expect_false(dbW_has_siteIDs(site_N + 1))
  expect_false(dbW_has_siteIDs(0))
  expect_true(all(dbW_has_scenarios(scenarios)))
  expect_false(dbW_has_scenarios("has_not"))
  expect_true(all(dbW_has_scenarioIDs(seq_along(scenarios))))
  expect_false(dbW_has_scenarioIDs(length(scenarios) + 1))
  expect_false(dbW_has_scenarioIDs(0))
})


#--- Unit: dbW site/scenario tables manipulation ------
test_that("dbW site/scenario tables manipulation", {
  #remove once issue #43 is fixed (unit tests for 'test_dbW_functionality.R'
  # fail on appveyor but not on travis)
  skip_on_appveyor()
  skip_on_os("windows")

  #--- * Obtain site_id all at once ------
  site_id1 <- dbW_getSiteId(
    lat = site_data1[, "Latitude"],
    long = site_data1[, "Longitude"]
  )
  expect_error(dbW_getSiteId(
    lat = site_data1[, "Latitude"],
    long = site_data1[site_ids[-1], "Longitude"])
  )
  site_id2 <- dbW_getSiteId(Labels = site_data1[, "Label"], ignore.case = FALSE)
  expect_equal(site_id1, site_id2)
  site_id3 <- dbW_getSiteId(
    Labels = tolower(site_data1[, "Label"]),
    ignore.case = TRUE
  )
  expect_equal(site_id1, site_id3)
  site_id4 <- dbW_getSiteId(
    Labels = tolower(site_data1[, "Label"]),
    ignore.case = FALSE
  )
  expect_equal(site_id4, rep(NA_integer_, site_N))

  site_id5a <- dbW_getSiteId(
    lat = c(NA, site_data1[, "Latitude"], NA),
    long = c(NA, site_data1[, "Longitude"], NA)
  )
  expect_equal(site_id5a, c(NA_integer_, site_id1, NA_integer_))

  site_id5b <- dbW_getSiteId(
    Labels = c("not there", site_data1[, "Label"], "not there either"),
    ignore.case = FALSE
  )
  expect_equal(site_id5b, c(NA_integer_, site_id1, NA_integer_))


  for (k in seq_len(site_N)) {
    #--- * Obtain site_id one by one ------
    site_id1 <- dbW_getSiteId(
      lat = site_data1[k, "Latitude"],
      long = site_data1[k, "Longitude"]
    )
    site_id2 <- dbW_getSiteId(
      Labels = site_data1[k, "Label"],
      ignore.case = FALSE
    )
    expect_equal(site_id1, site_id2)
    site_id3 <- dbW_getSiteId(
      Labels = tolower(site_data1[k, "Label"]),
      ignore.case = TRUE
    )
    expect_equal(site_id1, site_id3)
    site_id4 <- dbW_getSiteId(
      Labels = tolower(site_data1[k, "Label"]),
      ignore.case = FALSE
    )
    expect_equal(site_id4, NA_integer_)

    #--- * Attempt to add new site ------
    # Site already exists
    expect_true(dbW_addSites(site_data = site_data1[k, ], verbose = FALSE))
    expect_message(
      dbW_addSites(site_data = site_data1[k, ], verbose = TRUE),
      regexp = "sites are already in database, labels"
    )
    # Data missing
    expect_error(dbW_addSites(site_data = site_data1[k, "Site_id"]))

    #--- Add new sites
    expect_true(dbW_addSites(site_data = site_data3[k, ], verbose = FALSE))
  }

  #--- * Update site information ------
  expect_equal(
    dbW_getSiteTable()[, req_cols],
    rbind(site_data1, site_data3)[, req_cols]
  )
  expect_true(
    dbW_updateSites(Site_ids = i_update2, site_data = site_data2[i_update2, ])
  )
  expect_equal(
    dbW_getSiteTable()[, req_cols],
    rbind(site_data2, site_data3)[, req_cols]
  )
  expect_true(
    dbW_updateSites(Site_ids = i_update2, site_data = site_data1[i_update2, ])
  )
  expect_equal(
    dbW_getSiteTable()[, req_cols],
    rbind(site_data1, site_data3)[, req_cols]
  )
  expect_true(
    dbW_updateSites(Site_ids = -1, site_data = site_data1[i_update2, ])
  )
  expect_equal(
    dbW_getSiteTable()[, req_cols],
    rbind(site_data1, site_data3)[, req_cols]
  )
  # Update multiple sites at once
  tmp_sites <- site_data1
  tmp_sites[, "Label"] <- paste0(tmp_sites[, "Label"], "-mtest")
  expect_true(
    dbW_updateSites(Site_ids = site_data1[, "Site_id"], site_data = tmp_sites)
  )
  tmp <- dbW_getSiteTable()
  expect_equal(
    tmp[tmp[, "Site_id"] %in% site_data1[, "Site_id"], req_cols],
    tmp_sites[, req_cols]
  )
  expect_true(
    dbW_updateSites(Site_ids = site_data1[, "Site_id"], site_data = site_data1)
  )
  tmp <- dbW_getSiteTable()
  expect_equal(
    dbW_getSiteTable()[, req_cols],
    rbind(site_data1, site_data3)[, req_cols]
  )


  #--- * Add scenarios ------
  # Obtain scenario id
  expect_equal(
    dbW_getScenarioId(scenarios_added),
    c(
      seq_along(scenarios),
      rep(NA_integer_, length(scenarios_added) - length(scenarios))
    )
  )

  for (k in seq_along(scenarios)) {
    expect_equal(dbW_getScenarioId(scenarios[k]), k)
  }

  # Scenario already exists
  expect_true(dbW_addScenarios(scenarios[1], verbose = FALSE))
  expect_message(
    dbW_addScenarios(scenarios[1], verbose = TRUE),
    regexp = "Scenarios are already in database"
  )
  expect_true(dbW_addScenarios(tolower(scenarios[3]), ignore.case = TRUE))
  # New scenario
  expect_true(dbW_addScenarios(paste0(scenarios[1], "_new")))
  expect_true(dbW_addScenarios(tolower(scenarios[3]), ignore.case = FALSE))
  expect_equal(dbW_getScenariosTable()[, "Scenario"], scenarios_added)
  # Obtain scenario id
  expect_equal(dbW_getScenarioId(scenarios_added), seq_along(scenarios_added))
  expect_equal(dbW_getScenarioId(NULL), integer(0))


  #--- * Obtain site and scenario IDs ------
  site_id1 <- dbW_getSiteId(
    lat = site_data1[, "Latitude"],
    long = site_data1[, "Longitude"]
  )
  expect_error(
    dbW_getSiteId(
      lat = site_data1[, "Latitude"],
      long = site_data1[site_ids[-1], "Longitude"]
    )
  )
  site_id2 <- dbW_getSiteId(Labels = site_data1[, "Label"], ignore.case = FALSE)
  expect_equal(site_id1, site_id2)
  site_id3 <- dbW_getSiteId(
    Labels = tolower(site_data1[, "Label"]),
    ignore.case = TRUE
  )
  expect_equal(site_id1, site_id3)
  site_id4 <- dbW_getSiteId(
    Labels = tolower(site_data1[, "Label"]),
    ignore.case = FALSE
  )
  expect_equal(site_id4, rep(NA_integer_, site_N))
  site_id5 <- dbW_getSiteId(
    Labels = c("not there", site_data1[, "Label"], "not there either"),
    ignore.case = FALSE
  )
  expect_equal(site_id5, c(NA_integer_, site_id1, NA_integer_))

  id1 <- dbW_getIDs(
    long = c(NA, site_data1[, "Longitude"], NA),
    lat = c(NA, site_data1[, "Latitude"], NA),
    scenario = c("not here", scenarios_added, "not anywhere"),
    add_if_missing = FALSE
  )
  id2 <- dbW_getIDs(
    site_label = c("not there", site_data1[, "Label"], "not there either"),
    scenario = c("not here", scenarios_added, "not anywhere"),
    add_if_missing = FALSE
  )
  id3 <- dbW_getIDs(
    site_id = c(NA, site_data1[, "Site_id"], NA),
    scenario_id = c(100, seq_along(scenarios_added), NA),
    add_if_missing = FALSE
  )

  expect_equal(id1, id2)
  expect_equal(id1, id3)
  expect_equal(id1[["site_id"]], c(NA, site_data1[, "Site_id"], NA))
  expect_equal(id1[["scenario_id"]], c(NA, seq_along(scenarios_added), NA))


  #--- Add sites via `dbW_getIDs()`
  expect_true(dbW_deleteSite(Site_ids = site_data3[, "Site_id"]))

  id4a <- dbW_getIDs(
    site_id = site_data3[, "Site_id"],
    scenario = scenarios_added,
    add_if_missing = FALSE
  )
  expect_equal(id4a[["site_id"]], rep(NA_integer_, nrow(site_data3)))
  expect_equal(id4a[["scenario_id"]], seq_along(scenarios_added))

  id4b <- dbW_getIDs(
    site_id = site_data3[, "Site_id"],
    site_label = site_data3[, "Label"],
    long = site_data3[, "Longitude"],
    lat = site_data3[, "Latitude"],
    scenario = scenarios_added,
    add_if_missing = TRUE
  )

  expect_false(anyNA(id4b[["site_id"]]))
  expect_length(id4b[["site_id"]], nrow(site_data3))
  expect_false(anyNA(id4b[["scenario_id"]]))
  expect_length(id4b[["scenario_id"]], length(scenarios_added))
})


#--- Unit: dbW weather data manipulation ------
test_that("dbW weather data manipulation", {
  #remove once issue #43 is fixed (unit tests for 'test_dbW_functionality.R'
  # fail on appveyor but not on travis)
  skip_on_appveyor()
  skip_on_os("windows")

  #--- * Add weather data ------
  # Use 'Site_id' as identifier
  expect_true(
    dbW_addWeatherData(
      Site_id = 1,
      weatherData = sw_weather[[1]],
      Scenario = scenarios[1]
    )
  )
  expect_true(
    dbW_addWeatherData(
      Site_id = 1,
      weatherData = sw_weather[[1]],
      Scenario = scenarios[2]
    )
  )
  # Use 'Label' as identifier
  expect_true(
    dbW_addWeatherData(
      Label = site_data1[2, "Label"],
      weatherData = sw_weather[[2]],
      Scenario = scenarios[1]
    )
  )
  # Use 'lat'/'long' as identifier
  expect_true(
    dbW_addWeatherData(
      lat = site_data1[3, "Latitude"],
      long = site_data1[3, "Longitude"],
      weatherData = sw_weather[[2]],
      Scenario = scenarios[2]
    )
  )

  #--- * Add duplicate weather data entry ------
  # `dbW_addWeatherData()` prevents adding duplicate entries
  expect_error(
    dbW_addWeatherData(
      Site_id = 1,
      weatherData = sw_weather[[1]],
      Scenario = scenarios[1]
    )
  )

  # `dbW_addWeatherDataNoCheck()` does not prevent adding duplicate entries
  expect_identical(
    dbW_addWeatherDataNoCheck(
      Site_id = 1,
      Scenario_id = 1,
      StartYear = sw_years[[1]][1],
      EndYear = sw_years[[1]][2],
      weather_blob = dbW_weatherData_to_blob(sw_weather[[1]])
    ),
    1L
  )


  #--- * Check presence of weather data ------
  # Check one site x one scenario
  expect_true(
    dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1)[1, 1]
  )

  expect_equal(
    dbW_have_sites_all_weatherData(site_ids = 1, scen_ids = 1),
    dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1)[1, 1]
  )

  # Check one site x multiple scenarios
  expect_equal(
    as.vector(dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1:2)),
    c(TRUE, TRUE)
  )

  expect_equal(
    dbW_have_sites_all_weatherData(site_ids = 1, scen_ids = 1:2),
    unname(apply(
      dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1:2),
      MARGIN = 1,
      all
    ))
  )

  expect_equal(
    as.vector(dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1:3)),
    c(TRUE, TRUE, FALSE)
  )

  expect_equal(
    dbW_have_sites_all_weatherData(site_ids = 1, scen_ids = 1:3),
    unname(apply(
      dbW_has_weatherData(Site_ids = 1, Scenario_ids = 1:3),
      MARGIN = 1,
      all
    ))
  )

  # Check multiple sites x one scenario
  expect_equal(
    as.vector(dbW_has_weatherData(Site_ids = 1:2, Scenario_ids = 1)),
    c(TRUE, TRUE)
  )

  expect_equal(
    as.vector(
      dbW_has_weatherData(Site_ids = c(1:2, 4), Scenario_ids = 1)
    ),
    c(TRUE, TRUE, FALSE)
  )

  expect_equal(
    dbW_have_sites_all_weatherData(site_ids = c(1:2, 4), scen_ids = 1),
    unname(apply(
      dbW_has_weatherData(Site_ids = c(1:2, 4), Scenario_ids = 1),
      MARGIN = 1,
      all
    ))
  )


  # Check multiple sites x multiple scenarios
  res_exp <- matrix(
    c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, rep(FALSE, 6)),
    nrow = 3,
    ncol = 4,
    dimnames = list(
      paste("Site", 1:3, sep = "_"),
      paste("Scenario", 1:4, sep = "_")
    )
  )
  expect_equal(
    dbW_has_weatherData(Site_ids = 1:3, Scenario_ids = 1:4),
    res_exp
  )


  # Check multiple sites (including non-existing ones) x multiple scenarios
  expect_equal(
    dbW_have_sites_all_weatherData(site_ids = c(1:2, 4, 100), scen_ids = 1:2),
    unname(apply(
      dbW_has_weatherData(Site_ids = c(1:2, 4, 100), Scenario_ids = 1:2),
      MARGIN = 1,
      all
    ))
  )



  #--- * Retrieve weather data ------
  expect_equal(
    suppressWarnings(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[1])),
    sw_weather[[1]]
  )
  expect_equal(
    dbW_getWeatherData(Site_id = 1, Scenario = scenarios[2]),
    sw_weather[[1]]
  )
  expect_equal(
    dbW_getWeatherData(Site_id = 2, Scenario = scenarios[1]),
    sw_weather[[2]]
  )
  expect_equal(
    dbW_getWeatherData(Site_id = 3, Scenario = scenarios[2]),
    sw_weather[[2]]
  )

  #--- Retrieve weather data from multiple sites at once
  expect_equal(
    suppressWarnings(
      dbW_getWeatherData(
        Site_id = 1:3,
        Scenario = scenarios[1],
        stop_if_missing = FALSE
      )
    ),
    list(sw_weather[[1]], sw_weather[[2]], NULL)
  )

  #--- Retrieve weather data from multiple scenarios at once
  expect_equal(
    suppressWarnings(
      dbW_getWeatherData(
        Site_id = 1,
        Scenario = c(scenarios[1], NA, scenarios[2]),
        stop_if_missing = FALSE
      )
    ),
    list(sw_weather[[1]], NULL, sw_weather[[1]])
  )

  #--- Retrieve weather data from multiple sites each with a specific scenario
  expect_equal(
    suppressWarnings(
      dbW_getWeatherData(
        Site_id = c(1:3, 100),
        Scenario = c(scenarios[1], NA, scenarios[2], scenarios[1]),
        stop_if_missing = FALSE
      )
    ),
    list(sw_weather[[1]], NULL, sw_weather[[2]], NULL)
  )



  #--- Adding data to the same Site_id x Scenario Name combination will fail
  expect_error(
    dbW_addWeatherData(
      Site_id = 1,
      weatherData = sw_weather[[1]],
      Scenario = scenarios[1]
    )
  )


  #--- * Remove data ------
  # Delete one site and all associated weather data
  expect_true(dbW_has_siteIDs(3))
  expect_true(dbW_deleteSite(Site_ids = 3))
  expect_false(dbW_has_siteIDs(3))
  expect_error(dbW_getWeatherData(Site_id = 3, Scenario = scenarios[2]))
  expect_null(suppressWarnings(
    dbW_getWeatherData(
      Site_id = 3,
      Scenario = scenarios[2],
      stop_if_missing = FALSE
    )
  ))

  # Delete weather data of one site x scenario combination
  expect_true(dbW_has_siteIDs(1))
  expect_true(dbW_deleteSiteData(Site_id = 1, Scenario_id = 2))
  expect_true(dbW_has_siteIDs(1))
  expect_equal(
    suppressWarnings(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[1])),
    sw_weather[[1]]
  )
  expect_error(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[2]))


  #--- Delete duplicate entries
  # Delete the one duplicate entry
  expect_equal(dbW_delete_duplicated_weatherData(), 1)
  expect_equal(dbW_delete_duplicated_weatherData(), 0)

  # Add kmax duplicate entries and delete them all
  kmax <- 5
  for (k in seq_len(kmax)) {
    dbW_addWeatherDataNoCheck(
      Site_id = 1,
      Scenario_id = 1,
      StartYear = sw_years[[1]][1],
      EndYear = sw_years[[1]][2],
      weather_blob = dbW_weatherData_to_blob(sw_weather[[1]])
    )
  }

  expect_equal(dbW_delete_duplicated_weatherData(), kmax)
  expect_equal(dbW_delete_duplicated_weatherData(), 0)


  # Add multiple entries that differ in EndYear --> no duplicates to delete
  for (k in seq_len(kmax)) {
    dbW_addWeatherDataNoCheck(
      Site_id = 1,
      Scenario_id = 1,
      StartYear = sw_years[[1]][1],
      EndYear = sw_years[[1]][2] + k,
      weather_blob = dbW_weatherData_to_blob(sw_weather[[1]])
    )
  }

  expect_equal(dbW_delete_duplicated_weatherData(), 0)

  # Add multiple entries that differ in weather data
  # --> considered duplicated only if check_values is FALSE
  for (k in seq_len(kmax)) {
    tmp <- sw_weather[[1]]
    tmp[[1]]@data[1, 2] <- k

    dbW_addWeatherDataNoCheck(
      Site_id = 1,
      Scenario_id = 1,
      StartYear = sw_years[[1]][1],
      EndYear = sw_years[[1]][2],
      weather_blob = dbW_weatherData_to_blob(tmp)
    )
  }

  # Expect warning that multiple weather data objects are present
  expect_warning(
    dbW_getWeatherData(Site_id = 1, Scenario = scenarios[1]),
    "More than one weather data object exists"
  )

  expect_equal(dbW_delete_duplicated_weatherData(check_values = TRUE), 0)
  expect_equal(dbW_delete_duplicated_weatherData(check_values = FALSE), kmax)
  expect_equal(dbW_delete_duplicated_weatherData(check_values = FALSE), 0)


  # Add kmax duplicate entries for site 1 and for site 2 and delete site by site
  for (k in seq_len(kmax)) {
    for (k2 in 1:2) {
      dbW_addWeatherDataNoCheck(
        Site_id = k2,
        Scenario_id = 1,
        StartYear = sw_years[[1]][1],
        EndYear = sw_years[[1]][2],
        weather_blob = dbW_weatherData_to_blob(sw_weather[[1]])
      )
    }
  }

  expect_equal(dbW_delete_duplicated_weatherData(site_id = 1), kmax)
  expect_equal(
    dbW_delete_duplicated_weatherData(site_id = 1, carefully = TRUE),
    0
  )
  expect_equal(
    dbW_delete_duplicated_weatherData(site_id = 2, check_values = FALSE),
    kmax
  )
  expect_equal(dbW_delete_duplicated_weatherData(), 0)
})


#--- CLEAN UP dbW ------
dbW_disconnectConnection()
unlink(fdbWeather)
unlink(fdbWeather2, force = TRUE)


#--- Non-dbW functions ------
test_that("Manipulate weather data: years", {

  datA <- getWeatherData_folders(
    LookupWeatherFolder = file.path(path_extdata, "example1", "Input"),
    weatherDirName = "data_weather",
    filebasename = "weath"
  )
  datA_yrs <- get_years_from_weatherData(datA)

  # Unit tests for function 'get_years_from_weatherDF'
  datA_DF <- dbW_weatherData_to_dataframe(datA)
  datA_DF_noyrs  <- datA_DF[, -1]
  datA_yrs_ts <- datA_DF[, 1]

  datA_DF_result_con1 <- get_years_from_weatherDF(
    weatherDF = datA_DF,
    years = datA_yrs_ts,
    weatherDF_dataColumns = "DOY"
  )
  expect_equal(datA_DF_result_con1[["years"]], datA_yrs)
  expect_equal(datA_DF_result_con1[["year_ts"]], datA_yrs_ts)

  datA_DF_result_con2 <- get_years_from_weatherDF(
    weatherDF = datA_DF,
    years = datA_yrs,
    weatherDF_dataColumns = "DOY"
  )
  expect_equal(datA_DF_result_con2[["years"]], datA_yrs)
  expect_equal(datA_DF_result_con2[["year_ts"]], datA_yrs_ts)

  expect_error(
    get_years_from_weatherDF(datA_DF, datA_yrs[2:20], "DOY")
  ) #con 3

  datA_DF_result_con4 <- get_years_from_weatherDF(
    weatherDF = datA_DF,
    years = NULL,
    weatherDF_dataColumns = "DOY"
  )
  expect_equal(datA_DF_result_con4[["years"]], datA_yrs)
  expect_equal(datA_DF_result_con4[["year_ts"]], datA_yrs_ts)

  expect_error(
    get_years_from_weatherDF(
      weatherDF = datA_DF_noyrs,
      years = NULL,
      weatherDF_dataColumns = "DOY"
    )
  ) #con 5

  for (k in seq_along(tests)) {
    # skip test if weather generator is turned on due to missing weather data
    sw_input <- readRDS(
      file.path(dir_test_data, paste0(tests[k], "_input.rds"))
    )

    if (!swWeather_UseMarkov(sw_input)) {
      datB <- sw_weather[[k]]
      datB_yrs <- get_years_from_weatherData(datB)
      yrs_joint <- intersect(datA_yrs, datB_yrs)

      # exclude calculated variables from comparison
      ids <- c(
        1:2,
        2L + which(slot(slot(sw_input, "weather"), "dailyInputFlags"))
      )

      expect_equal(
        dbW_weatherData_to_dataframe(
          datA[select_years(datA_yrs, min(yrs_joint), max(yrs_joint))]
        )[, ids, drop = FALSE],
        dbW_weatherData_to_dataframe(
          datB[select_years(datB_yrs, min(yrs_joint), max(yrs_joint))]
        )[, ids, drop = FALSE],
        tolerance = 1e-3
      )
    }
  }
})


test_that("Convert calendar years", {
  wdata <- rSOILWAT2::weatherData

  dailyInputFlags <- calc_dailyInputFlags(wdata)
  ids_vars <- 2L + which(dailyInputFlags)
  ids_cols <- c(1:2, ids_vars)

  ## Transfer to different years (partially overlapping)
  wnew <- dbW_convert_to_GregorianYears(
    wdata,
    new_startYear = 2000,
    new_endYear = 2020
  )
  expect_equal(unique(wnew[, "Year"]), 2000:2020)
  expect_false(anyNA(wnew[wnew[, "Year"] %in% names(wdata), ids_cols]))
  expect_true(anyNA(wnew[, ids_cols]))

  ## Transfer to a subset of years (i.e., subset)
  wnew <- dbW_convert_to_GregorianYears(
    wdata,
    new_startYear = 2000,
    new_endYear = 2005
  )
  expect_equal(unique(wnew[, "Year"]), 2000:2005)
  expect_false(anyNA(wnew[, ids_cols]))

  ## Correct/convert from a non-leap to a Gregorian calendar
  wempty <- dbW_weatherData_to_dataframe(weatherHistory())[1:365, ]

  wnew <- dbW_convert_to_GregorianYears(
    wempty,
    new_startYear = 2016,
    new_endYear = 2016
  )
  expect_equal(unique(wnew[, "Year"]), 2016:2016)
  expect_equal(nrow(wnew), 366) # leap year
  expect_true(anyNA(wnew[, ids_cols]))


  wnew <- dbW_convert_to_GregorianYears(
    wdata["1981"],
    new_startYear = 2016,
    new_endYear = 2016,
    type = "sequential"
  )
  expect_equal(unique(wnew[, "Year"]), 2016:2016)
  expect_equal(nrow(wnew), 366) # leap year
  # variables on leap day are missing
  expect_equal(sum(is.na(wnew[, ids_cols])), length(ids_vars))
})
