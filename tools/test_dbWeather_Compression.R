context("Test dbWeather compression types")

# Create reference objects
do_benchmark <- FALSE
year_start <- 1979
year_end <- 2010
path_demo <- system.file("extdata", "example1", package = "Rsoilwat31")

dbWs3 <- list(list(fname = "dbWeatherData_test1_gzip_old.sqlite3", type = "gzip", mode = "old"),
			  list(fname = "dbWeatherData_test1_gzip_new.sqlite3", type = "gzip", mode = "new"),
			  list(fname = "dbWeatherData_test1_bzip2_old.sqlite3", type = "bzip2", mode = "old"),
			  list(fname = "dbWeatherData_test1_bzip2_new.sqlite3", type = "bzip2", mode = "new"),
			  list(fname = "dbWeatherData_test1_xy_old.sqlite3", type = "xy", mode = "old"),
			  list(fname = "dbWeatherData_test1_xy_new.sqlite3", type = "xy", mode = "new"),
			  list(fname = "dbWeatherData_test1_none_old.sqlite3", type = "none", mode = "old"),
			  list(fname = "dbWeatherData_test1_none_new.sqlite3", type = "none", mode = "new"))

sw_weath <- getWeatherData_folders(
	LookupWeatherFolder = file.path(path_demo, "Input"),
	weatherDirName = "data_weather",
	filebasename = "weath", startYear = year_start, endYear = year_end)

site_ids <- seq_len(10)
site_data <- data.frame(Site_id = site_ids,
						Latitude = site_ids,
						Longitude = site_ids,
						Label = paste0("site", site_ids))

for (it in seq_along(dbWs3)) {
	dbW_createDatabase(dbFilePath = dbWs3[[it]]$fname,
		site_data = site_data,
		scenarios = data.frame(Scenario = "Current"),
		compression_type = dbWs3[[it]]$type)	
	dbW_setConnection(dbFilePath = dbWs3[[it]]$fname)
	
	for (iw in site_ids) {
		if (dbWs3[[it]]$mode == "new") {
			dbW_addWeatherData(Site_id = iw,
				weatherData = sw_weath,
				ScenarioName = "Current")
		} else {
			Rsoilwat31:::dbW_addWeatherData_old(Site_id = iw,
				weatherData = sw_weath,
				ScenarioName = "Current")
		}
	}
	dbW_disconnectConnection()
}

print("Compression rates of dbWeather:")
dbW_disk <- sapply(dbWs3, function(dbW) file.size(dbW$fname))
dbW_disk <- matrix(1 - signif(dbW_disk / max(dbW_disk, na.rm = TRUE), 2), ncol = 1)
dimnames(dbW_disk) <- list(sapply(dbWs3, function(x) paste(x$type, x$mode, sep = "-")),
							"Compression_ratio")

print(dbW_disk)


# Test object identity
test_that("Test weather object coherence", {
	skip_on_cran()

	for (it in seq_along(dbWs3)) {
		dbW_setConnection(dbFilePath = dbWs3[[it]]$fname)
		
		for (iw in site_ids) {
			expect_equal(sw_weath,
				if (dbWs3[[it]]$mode == "new") {
					dbW_getWeatherData(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current")
				} else {
					Rsoilwat31:::dbW_getWeatherData_old(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current")
				})
		}
		dbW_disconnectConnection()
	}
})


# Compare speed
test_that("Benchmark dbWeather retrieval speed", {
	skip_on_cran()
	skip_if_not_installed("microbenchmark")
	skip_if_not(do_benchmark, "Benchmarking is turned off.")
	
	retrievals <- function(path_dbW, site_ids, mode) {
		dbW_setConnection(dbFilePath = path_dbW)

		for (iw in site_ids) {
			temp <- if (mode == "new") {
					dbW_getWeatherData(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current")
				} else {
					Rsoilwat31:::dbW_getWeatherData_old(Site_id = iw,
									startYear = year_start,
									endYear = year_end,
									Scenario = "Current")
				}
		}
		dbW_disconnectConnection()
	}

	to_bench <- lapply(dbWs3, function(dbW)
			call("retrievals", path_dbW = dbW$fname, site_ids = site_ids, mode = dbW$mode))
	names(to_bench) <- sapply(dbWs3, function(x) paste(x$type, x$mode, sep = "-"))
	
	expect_s3_class(bench <- microbenchmark::microbenchmark(list = to_bench, times = 200), "microbenchmark")
	print(bench)
})

# Clean up
temp <- sapply(dbWs3, function(dbW) unlink(dbW$fname))
