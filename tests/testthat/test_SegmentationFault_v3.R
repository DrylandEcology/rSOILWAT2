context("Rsoilwat segfault")

# Running from command line --> segfaults
#	r --slave -f Test_Rsoilwat31_SegmentationFault_v3.R
#	r -d "valgrind --dsymutil=yes --leak-check=full --track-origins=yes --expensive-definedness-checks=yes" --slave -f Test_Rsoilwat31_SegmentationFault_v3.R
#	r -d "valgrind --vgdb-error=0"  --slave -f Test_Rsoilwat31_SegmentationFault_v3.R

# Running interactively with source() --> segfaults on the second time source() is called
#	r -d lldb # run
#	r via r.app GUI

#options(warn=0, error=traceback)
#options(CBoundsCheck = TRUE) #https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Debugging-compiled-code
#gctorture(FALSE) #https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Using-gctorture


# Create reference objects
path_demo <- system.file("extdata", "example1", package = "Rsoilwat31")

a <- sw_inputDataFromFiles(dir = path_demo, files.in = file.path(path_demo, "files_v31.in"))

# Location of segfaults with upper bound of b
#	- (eo/debug/!interactive()): step 2 >= 558151 > step 3 >= 558105 > step 4 >= 555193 > no segfault
b <- 1:1e6
b2 <- 1:1e7
b3 <- 1:1e7

test_that("Test for segfault", {
	expect_silent({
		elems <- ls(envir = .GlobalEnv)
		if (length(elems) > 0) for (i in seq_along(elems)) x <- get(elems[i])
	})
})

