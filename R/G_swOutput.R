# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################


print("swOutput")
ObjType = c("FileManagement","Model","Weather","Site","SoilWater","VegetationEstab","VegetationProduction","Output")
OutKey = c("AllWthr","Temp","Precip","SoilInf","Runoff","allH2O","SWC","SWCM","SWP","SWA","SurfaceWater","Transp","EvapSoil","EvapSurface","Interception","LyrDrain","HydRed","ET","AET","PET","WetDays","SnowPack","DeepSWC","SoilTemp","AllVeg","Esab")
KEY =  c("WTHR", "TEMP", "PRECIP", "SOILINFILT", "RUNOFF", "ALLH2O", "SWC", "SWCMCM", "SWP", "SWA", "SURFACEWATER", "TRANSP", "EVAPSOIL",
		"EVAPSURFACE", "INTERCEPTION", "LYRDRAIN", "HYDRED", "ET", "AET", "PET", "WETDAY", "SNOWPACK", "DEEPSWC", "SOILTEMP", "ALLVEG", "ESTABL")
OutPeriod = c("Day","Week","Month","Year")
timePeriods <- c("dy","wk","mo","yr")
OutSum = c("off","sum","avg","fnl")
#######
swOUT_key <- setClass(Class="swOUT_key",representation(mykey="integer",myobj="integer",period="integer",sumtype="integer",use="logical",first="integer",last="integer",first_orig="integer",last_orig="integer",outfile="character"),
		prototype=prototype(mykey=as.integer(c(0:4,0,6:16,0,18:23,0,25)),myobj=as.integer(c(0,rep(2,4),0,rep(4,11),0,rep(4,6),0,5)),period=as.integer(c(rep(0,26))),sumtype=as.integer(c(0,2,1,1,1,0,2,2,2,2,2,1,1,1,1,1,1,0,1,1,1,2,1,2,0,1)),use=c(FALSE,rep(TRUE,4),FALSE,rep(TRUE,11),FALSE,rep(TRUE,6),FALSE,TRUE),
				first=as.integer(rep(0,26)),last=as.integer(rep(0,26)),first_orig=as.integer(c(0,rep(1,4),0,rep(1,11),0,rep(1,6),0,1)),last_orig=as.integer(c(0,rep(366,4),0,rep(366,11),0,rep(366,6),0,366)),outfile=c("","temp","precip","infiltration","runoff","","swc","vwc","swp","swa","surface_water","transp","evap_soil","evap_surface","interception","percolation","hydred","","aet","pet","wetdays","snowpack","deep_drain","soil_temp","","estabs")  ))
swOUT_key_validity<-function(object){
	temp<-c(length(mykey), length(myobj), length(period), length(sumtype), length(use), length(first), length(last), length(first_orig), length(last_orig), length(outfile))
	if(length(unique(temp)) != 1)
		return("Missing values...")
	TRUE
}
setValidity("swOUT_key",swOUT_key_validity)

setMethod(f="swClear",
		signature="swOUT_key",
		definition=function(object) {
			object@mykey=integer(26)
			object@myobj=integer(26)
			object@period=integer(26)
			object@sumtype=integer(26)
			object@use=logical(26)
			object@first=integer(26)
			object@last=integer(26)
			object@first_orig=integer(26)
			object@last_orig=integer(26)
			object@outfile=character(26)
			return(object)
		})

###########################OUTSETUP.IN########################################

swOUT <- setClass(Class="swOUT",representation(outputSeparator="character",timePeriods="integer"),prototype=prototype(outputSeparator="\t",timePeriods=as.integer(c(3,2,1,0))),contains="swOUT_key")
setMethod(f="swClear",
		signature="swOUT",
		definition=function(object) {
			object@outputSeparator="\t"
			object@timePeriods=integer(4)
			object@mykey=integer(26)
			object@myobj=integer(26)
			object@period=integer(26)
			object@sumtype=integer(26)
			object@use=logical(26)
			object@first=integer(26)
			object@last=integer(26)
			object@first_orig=integer(26)
			object@last_orig=integer(26)
			object@outfile=character(26)
			return(object)
		})

setMethod("get_swOUT", "swOUT", function(object) {return(object)})
setMethod("swOUT_TimeStep","swOUT",function(object) {return(object@timePeriods)})
setMethod("swOUT_OutputSeparator","swOUT",function(object) {return(object@outputSeparator)})

setReplaceMethod(f="set_swOUT",signature="swOUT",function(object,value) {object <- value; return(object)})
setReplaceMethod(f="swOUT_TimeStep",signature="swOUT",function(object,value) {value<-as.integer(value); object@timePeriods <- value; return(object)})
setReplaceMethod(f="swOUT_OutputSeparator",signature="swOUT",function(object,value) {object@outputSeparator <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swOUT", file="character"), definition=function(object, file) {
			infilename <- file.path(file)
			infiletext <- character(44+sum(object@use))
			infiletext[1] = "# Output setup file for SOILWAT v27 compiled on Ubuntu (07122013)"
			infiletext[2] = "#"
			infiletext[3] = "# Notes:"
			infiletext[4] = "# Time periods available:  DY,WK,MO,YR "
			infiletext[5] = "#   eg, if DY is chosen then 100,200 would mean to use the second hundred days"
			infiletext[6] = "#   But if YR is chosen, start and end numbers are in days so only those days"
			infiletext[7] = "#   are reported for the yearly average."
			infiletext[8] = "# Some keys from older versions (fortran and the c versions mimicking the fortran"
			infiletext[9] = "#   version) are not currently implemented:"
			infiletext[10] = "#   ALLH20, WTHR."
			infiletext[11] = "#"
			infiletext[12] = "# ESTABL only produces yearly output, namely, DOY for each species requested."
			infiletext[13] = "#   Thus, to minimize typo errors, all flags are ignored except the filename."
			infiletext[14] = "#   Output is simply the day of the year establishment occurred for each species"
			infiletext[15] = "#   in each year of the model run.  Refer to the estabs.in file for more info."
			infiletext[16] = "#"
			infiletext[17] = "# DEEPSWC produces output only if the deepdrain flag is set in siteparam.in."
			infiletext[18] = "#"
			infiletext[19] = "# Filename prefixes should not have a file extension."
			infiletext[20] = "# Case is unimportant."
			infiletext[21] = "#"
			infiletext[22] = "# SUMTYPEs are one of the following:"
			infiletext[23] = "#  OFF - no output for this variable"
			infiletext[24] = "#  SUM - sum the variable for each day in the output period"
			infiletext[25] = "#  AVG - average the variable over the output period"
			infiletext[26] = "#  FIN - output value of final day in the period; soil water variables only."
			infiletext[27] = "# Note that SUM and AVG are the same if timeperiod = dy."
			infiletext[28] = "#"
			infiletext[29] = "# (3-Sep-03) OUTSEP key indicates the output separator.  This method"
			infiletext[30] = "# allows older files to work with the new version.  The default is a "
			infiletext[31] = "# tab.  Other options are 's' or 't' for space or tab (no quotes)"
			infiletext[32] = "# or any other printable character as itself (eg, :;| etc).  The given"
			infiletext[33] = "# separator will apply to all of the output files.  Note that only lowercase"
			infiletext[34] = "# letters 's' or 't' are synonyms."
			infiletext[35] = "#"
			infiletext[36] = "# (01/17/2013) TIMESTEP key indicates which periods you want to output."
			infiletext[37] = "# You can output all the periods at a time, just one, or however many"
			infiletext[38] = "# you want. To change which periods to output type 'dy' for day,"
			infiletext[39] = "# 'wk' for week, 'mo' for month, and 'yr' for year after TIMESTEP"
			infiletext[40] = "# in any order. For example: 'TIMESTEP mo wk' will output for month and week"
			infiletext[41] = paste("OUTSEP ",ifelse(object@outputSeparator=="\t","t","s"),sep="")
			infiletext[42] = paste("TIMESTEP ",paste(timePeriods[object@timePeriods + 1],collapse = " "),sep="")
			
			infiletext[44] = "# key			SUMTYPE		start	end		filename_prefix"
			j=1
			for(i in 1:26) {
				if(object@use[i]) {
					infiletext[44+j] = paste(KEY[object@mykey[i]+1],"\t",OutSum[object@sumtype[i]+1],"\t",as.character(object@first_orig[i]),"\t",ifelse(object@last_orig[i]==366,"end",as.character(object@last_orig[i])),"\t",object@outfile[i],sep="")
					j=j+1
				}
			}
			
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swOUT",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			if(temp<-strsplit(infiletext[41],split=" ")[[1]][2] == "t")
				object@outputSeparator="\t"
			else if(temp == "s")
				object@outputSeparator=" "
			else
				object@outputSeparator="\t"
			
			temp<-strsplit(x=infiletext[42],split=" ")[[1]][-1]
			object@timePeriods = as.integer(sapply(1:length(temp), FUN=function(i) which(temp[i] == timePeriods))-1)
			
			for(i in 45:length(infiletext)) {
				if(infiletext[i] != "") {
					temp<-strsplit(x=infiletext[i],split="\t")[[1]]
					temp<-unlist(strsplit(x=temp,split=" "))
					temp <- temp[temp != ""][1:5]
					mykey<- as.integer(grep(pattern=temp[1],x=KEY)[1])
					sumtype <- as.integer(grep(pattern=temp[2],x=OutSum))-1
					start <- as.integer(temp[3])
					if(grepl(pattern="end",x=temp[4])) {
						end <- as.integer(366)
					} else {
						end <- as.integer(temp[4])
					}
					object@mykey[mykey] = as.integer(mykey-1)
					object@sumtype[mykey] = as.integer(sumtype)
					object@first_orig[mykey] = start
					object@last_orig[mykey] = end
					object@outfile[mykey] = temp[5]
					if(object@sumtype[mykey] != 0) {
						object@use[mykey] = TRUE
					} else {
						object@use[mykey] = FALSE
					}
				}
			}
			return(object)
		})

