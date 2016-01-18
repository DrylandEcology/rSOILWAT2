# Running from command line --> segfaults
#	r --slave -f Test_Rsoilwat31_SegmentationFault_v3.R
#	r -d "valgrind --dsymutil=yes --leak-check=full --track-origins=yes --expensive-definedness-checks=yes" --slave -f Test_Rsoilwat31_SegmentationFault_v3.R
#	r -d "valgrind --vgdb-error=0"  --slave -f Test_Rsoilwat31_SegmentationFault_v3.R

# Running interactively with source() --> segfaults on the second time source() is called
#	r -d lldb # run
#	r via r.app GUI

options(warn=0, error=traceback)
options(CBoundsCheck = TRUE) #https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Debugging-compiled-code
gctorture(FALSE) #https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Using-gctorture

#comp <- "eo" # "err"
#.libPaths(new=file.path(getwd(), "Packages", comp, "debug"))
#.libPaths(new=file.path(getwd(), "Packages", comp, "normal"))
library(Rsoilwat31)

print(sessionInfo())

print("Step 1")			
a <- sw_inputDataFromFiles(dir = dirname(system.file("extdata", "files_v31.in", package = "Rsoilwat31")), files.in = "files_v31.in")

print("Step 2")
# Location of segfaults with upper bound of b
#	- (eo/debug/!interactive()): step 2 >= 558151 > step 3 >= 558105 > step 4 >= 555193 > no segfault
b <- 1:1e6
b2 <- 1:1e7
b3 <- 1:1e7

print("Step 3")
elems <- ls()

print("Step 4")			
if(length(elems) > 0){
	for(i in seq_along(elems)){
		x <- get(elems[i])
		print(paste(i, elems[i], object.size(x)))
	}
}

print("No segmentation fault")

