context("rSOILWAT2 weather database")

#---INPUTS
path_extdata <- file.path("..", "..", "inst", "extdata")
if (!dir.exists(path_extdata)) {
  path_extdata <- system.file("extdata", package = "rSOILWAT2")
}

fdbWeather <- tempfile(fileext = ".sqlite3")
fdbWeather2 <- tempfile(fileext = ".txt")
write(NA, file = fdbWeather2)
fdbWeather3 <- file.path("/Fantasy", "Volume", "test.sqlite3")

tests <- c("Ex1", "Ex2")
sw_weather <- lapply(tests, function(it) readRDS(paste0(it, "_weather.rds")))
scenarios <- c("Current", paste0("TestScenario", tests))
scenarios_added <- c(scenarios, paste0(scenarios[1], "_new"), tolower(scenarios[1]))

site_N <- 5
site_ids <- seq_len(site_N)
site_data1 <- data.frame(
  Site_id = site_ids,
  Latitude = 40 + site_ids,
  Longitude = -100 - site_ids,
  Label = paste0("TestSite_id", site_ids),
  stringsAsFactors = FALSE)

i_update2 <- 2
site_data2 <- site_data1
site_data2[i_update2, "Label"] <- NA

site_data3 <- data.frame(
  Site_id = site_N + site_ids,
  Latitude = 40 + site_N + site_ids,
  Longitude = -100 - site_N + site_ids,
  Label = paste0("TestSite_id", site_N + site_ids),
  stringsAsFactors = FALSE)


#---TESTS
test_that("dbW creation", {
  #--- Attempt to connect to (no) weather database
  expect_false(dbW_setConnection(fdbWeather, create_if_missing = FALSE))
  expect_message(dbW_setConnection(fdbWeather, create_if_missing = FALSE, 
    verbose = TRUE), regexp = "does not exist")
  expect_message(dbW_setConnection(fdbWeather, create_if_missing = TRUE, 
    verbose = TRUE), regexp = "creating a new database")
  unlink(fdbWeather)
  expect_false(dbW_setConnection(fdbWeather2, create_if_missing = TRUE)) 
  expect_message(dbW_setConnection(fdbWeather2, create_if_missing = TRUE, 
    verbose = TRUE), regexp = "exists but is likely not a SQLite-database")
  expect_false(dbW_setConnection(fdbWeather3, create_if_missing = TRUE)) 
  expect_message(dbW_setConnection(fdbWeather3, create_if_missing = TRUE, 
    verbose = TRUE), regexp = "cannot be created likely because the path does not exist")
  expect_false(dbW_IsValid())

  #--- Create weather database and check that connection
  expect_message(dbW_createDatabase(fdbWeather, site_data = site_data1,
    scenarios = scenarios, scen_ambient = scenarios[1], 
    verbose = TRUE, ARG_DOESNT_EXIST = 1:3), regexp = "arguments ignored/deprecated")
  expect_false(dbW_createDatabase(fdbWeather))
  expect_message(dbW_createDatabase(fdbWeather, verbose = TRUE),
    regexp = "does already exist")
  unlink(fdbWeather)
  expect_false(dbW_createDatabase(fdbWeather3, site_data = site_data1,
    scenarios = scenarios, scen_ambient = scenarios[1]))
  expect_message(dbW_createDatabase(fdbWeather3, site_data = site_data1,
    scenarios = scenarios, scen_ambient = scenarios[1], verbose = TRUE),
    regexp = "was not able to create a new database and connect to the file")
  expect_false(dbW_createDatabase(fdbWeather, site_data = NA,
    scenarios = scenarios, scen_ambient = scenarios[1]))
  expect_message(dbW_createDatabase(fdbWeather, site_data = NA,
    scenarios = scenarios, scen_ambient = scenarios[1], verbose = TRUE),
    regexp = "because of errors in the table data")
  
  unlink(fdbWeather)
  expect_true(dbW_createDatabase(fdbWeather, site_data = site_data1,
    scenarios = scenarios, scen_ambient = scenarios[1]))
  expect_true(dbW_setConnection(fdbWeather))
  expect_true(dbW_IsValid())
  expect_true(dbW_disconnectConnection())
  expect_false(dbW_IsValid())
  expect_true(dbW_setConnection(fdbWeather))

  #--- Check on status of weather database
  expect_equal(dbW_version(), numeric_version(con.env$dbW_version))
  expect_equal(dbW_compression(), con.env$default_blob_compression_type)

  #--- Check on site/scenario tables
  expect_equal(dbW_getSiteTable(), site_data1)
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

test_that("dbW site/scenario tables manipulation", {
  for (k in seq_len(site_N)) {
    #--- Obtain site_id
    site_id <- dbW_getSiteId(lat = site_data1[k, "Latitude"],
      long = site_data1[k, "Longitude"])
    expect_equal(site_id, site_data1[k, "Site_id"])
    site_id <- suppressMessages(dbW_getSiteId(Label = site_data1[k, "Label"],
      ignore.case = FALSE))
    expect_equal(site_id, site_data1[k, "Site_id"])
    site_id <- suppressMessages(dbW_getSiteId(Label = tolower(site_data1[k, "Label"]),
      ignore.case = TRUE))
    expect_equal(site_id, site_data1[k, "Site_id"])
    site_id <- suppressMessages(dbW_getSiteId(Label = tolower(site_data1[k, "Label"]),
      ignore.case = FALSE))
    expect_equal(site_id, NULL)

    #--- Attempt to add new site
    # Provide site_id: site already exists, and data correct
    expect_equal(dbW_addSite(Site_id = site_data1[k, "Site_id"],
      lat = site_data1[k, "Latitude"], long = site_data1[k, "Longitude"],
      Label = site_data1[k, "Label"]), site_data1[k, "Site_id"])
    # Provide site_id: site already exists, but data incorrect
    expect_error(dbW_addSite(Site_id = site_data1[k, "Site_id"],
      lat = 91, long = site_data1[k, "Longitude"], Label = site_data1[k, "Label"]))
    # Provide site_id: site already exists, but data missing
    expect_error(dbW_addSite(Site_id = site_data1[k, "Site_id"],
      lat = NA, long = site_data1[k, "Longitude"], Label = site_data1[k, "Label"]))
    # Missing site_id: site already exists, and data correct
    expect_equal(dbW_addSite(Label = site_data1[k, "Label"],
      lat = site_data1[k, "Latitude"], long = site_data1[k, "Longitude"]),
      site_data1[k, "Site_id"])
    # Missing site_id: site already exists, but data incorrect
    expect_error(dbW_addSite(Label = site_data1[k, "Label"],
      lat = 91, long = site_data1[k, "Longitude"]))

    #--- Add new sites
    expect_equal(dbW_addSite(Site_id = site_data3[k, "Site_id"],
      lat = site_data3[k, "Latitude"], long = site_data3[k, "Longitude"],
      Label = site_data3[k, "Label"]),
      site_data3[k, "Site_id"])
    expect_equal(dbW_addSite(Label = site_data3[k, "Label"],
      lat = site_data3[k, "Latitude"], long = site_data3[k, "Longitude"]),
      site_data3[k, "Site_id"])
  }

  #--- Update site information
  expect_equal(dbW_getSiteTable(), rbind(site_data1, site_data3))
  expect_true(dbW_updateSites(site_ids = i_update2, new_data = site_data2[i_update2, ]))
  expect_equal(dbW_getSiteTable(), rbind(site_data2, site_data3))
  expect_true(dbW_updateSites(site_ids = i_update2, new_data = site_data1[i_update2, ]))
  expect_equal(dbW_getSiteTable(), rbind(site_data1, site_data3))
  expect_true(dbW_updateSites(site_ids = -1, new_data = site_data1[i_update2, ]))
  expect_equal(dbW_getSiteTable(), rbind(site_data1, site_data3))

  #--- Add scenarios
  # Scenario already exists
  expect_true(dbW_addScenarios(scenarios[1]))
  expect_true(dbW_addScenarios(tolower(scenarios[1]), ignore.case = TRUE))
  # New scenario
  expect_true(dbW_addScenarios(paste0(scenarios[1], "_new")))
  expect_true(dbW_addScenarios(tolower(scenarios[1]), ignore.case = FALSE))
  expect_equal(dbW_getScenariosTable()[, "Scenario"], scenarios_added)
})

test_that("dbW weather data manipulation", {
  #--- Add weather data
  # Use 'Site_id' as identifier
  expect_true(dbW_addWeatherData(Site_id = 1, weatherData = sw_weather[[1]],
    ScenarioName = scenarios[1]))
  expect_true(dbW_addWeatherData(Site_id = 1, weatherData = sw_weather[[1]],
    ScenarioName = scenarios[2]))
  # Use 'Label' as identifier
  expect_true(dbW_addWeatherData(label = site_data1[2, "Label"],
    weatherData = sw_weather[[2]], ScenarioName = scenarios[1]))
  # Use 'lat'/'long' as identifier
  expect_true(dbW_addWeatherData(lat = site_data1[3, "Latitude"],
    long = site_data1[3, "Longitude"], weatherData = sw_weather[[2]],
    ScenarioName = scenarios[2]))

  # Retrieve weather data
  expect_equal(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[1]), sw_weather[[1]])
  expect_equal(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[2]), sw_weather[[1]])
  expect_equal(dbW_getWeatherData(Site_id = 2, Scenario = scenarios[1]), sw_weather[[2]])
  expect_equal(dbW_getWeatherData(Site_id = 3, Scenario = scenarios[2]), sw_weather[[2]])

  # Adding data to the same Site_id x ScenarioName combination will fail
  expect_error(dbW_addWeatherData(Site_id = 1, weatherData = sw_weather[[1]],
    ScenarioName = scenarios[1]))

  #--- Remove data
  # Delete one site and all associated weather data
  expect_true(dbW_has_siteIDs(3))
  expect_true(dbW_deleteSite(Site_id = 3))
  expect_false(dbW_has_siteIDs(3))
  expect_error(dbW_getWeatherData(Site_id = 3, Scenario = scenarios[2]))

  # Delete weather data of one site x scenario combination
  expect_true(dbW_has_siteIDs(1))
  expect_true(dbW_deleteSiteData(Site_id = 1, Scenario_id = 2))
  expect_true(dbW_has_siteIDs(1))
  expect_equal(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[1]), sw_weather[[1]])
  expect_error(dbW_getWeatherData(Site_id = 1, Scenario = scenarios[2]))
})


#---CLEAN UP
dbW_disconnectConnection()
unlink(fdbWeather)
unlink(fdbWeather2)
unlink(fdbWeather3)


#--- Non-dbW functions
test_that("Manipulate weather data", {
  for (k in seq_along(tests)) {
    datA <- getWeatherData_folders(LookupWeatherFolder = file.path(path_extdata,
      paste0("example", k), "Input"), weatherDirName = "data_weather",
      filebasename = "weath")
    datB <- sw_weather[[k]]
    datA_yrs <- get_years_from_weatherData(datA)
    datB_yrs <- get_years_from_weatherData(datB)
    yrs_joint <- intersect(datA_yrs, datB_yrs)
    expect_equal(datA[select_years(datA_yrs, min(yrs_joint), max(yrs_joint))],
      datB[select_years(datB_yrs, min(yrs_joint), max(yrs_joint))], tol = 1e-3)
  }
})

