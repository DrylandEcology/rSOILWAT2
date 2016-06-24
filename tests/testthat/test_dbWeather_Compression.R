context("Test dbWeather compression types")

# Create reference objects
do_benchmark <- FALSE
year_start <- 1979
year_end <- 2010
path_demo <- system.file("extdata", "example1", package = "Rsoilwat31")

dbWs3 <- list(gzip = "dbWeatherData_test1_gzip.sqlite3",
			  bzip2 = "dbWeatherData_test1_bzip2.sqlite3",
			  xy = "dbWeatherData_test1_xy.sqlite3",
			  none = "dbWeatherData_test1_none.sqlite3")

sw_weath <- getWeatherData_folders(
	LookupWeatherFolder = file.path(path_demo, "Input"),
	weatherDirName = "data_weather",
	filebasename = "weath", startYear = year_start, endYear = year_end)

site_ids <- seq_len(5)
site_data <- data.frame(Site_id = site_ids,
						Latitude = site_ids,
						Longitude = site_ids,
						Label = paste0("site", site_ids))

for (it in seq_along(dbWs3)) {
	dbW_createDatabase(dbFilePath = dbWs3[[it]],
		site_data = site_data,
		scenarios = data.frame(Scenario = "Current"),
		compression_type = names(dbWs3)[it])	
	dbW_setConnection(dbFilePath = dbWs3[[it]])
	
	for (iw in site_ids) {
		dbW_addWeatherData(Site_id = iw,
			weatherData = sw_weath,
			ScenarioName = "Current")
	}
	dbW_disconnectConnection()
}

print("Compression rates of dbWeather:")
dbW_disk <- sapply(dbWs3, function(dbW) file.size(dbW))
print(1 - signif(dbW_disk / max(dbW_disk, na.rm = TRUE), 2))


# Test object identity
test_that("Test weather object coherence", {
	skip_on_cran()

	for (it in seq_along(dbWs3)) {
		dbW_setConnection(dbFilePath = dbWs3[[it]])
		
		for (iw in site_ids) {
			expect_equal(sw_weath,
				dbW_getWeatherData(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current"))
		}
		dbW_disconnectConnection()
	}
})


# Compare speed
test_that("Benchmark dbWeather retrieval speed", {
	skip_on_cran()
	skip_if_not_installed("microbenchmark")
	skip_if_not(do_benchmark, "Benchmarking is turned off.")
	
	retrievals <- function(dbW, site_ids) {
		dbW_setConnection(dbFilePath = dbW)

		for (iw in site_ids) {
			temp <- dbW_getWeatherData(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current")
		}
		dbW_disconnectConnection()
	}

	expect_s3_class(bench <- microbenchmark::microbenchmark(list = lapply(dbWs3, function(dbW)
			call("retrievals", dbW = dbW, site_ids = site_ids))),
		"microbenchmark")
	print(bench)
})

# Clean up
temp <- sapply(dbWs3, function(dbW) unlink(dbW))
