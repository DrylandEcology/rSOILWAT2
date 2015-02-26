# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################


print("swEstab")
swEstabSpecies <- setClass(Class="swEstabSpecies",representation(fileName="character",estab_lyrs="integer",barsGERM="numeric",barsESTAB="numeric",min_pregerm_days="integer",max_pregerm_days="integer",min_wetdays_for_germ="integer",
				max_drydays_postgerm="integer",min_wetdays_for_estab="integer",min_days_germ2estab="integer",max_days_germ2estab="integer",min_temp_germ="integer",max_temp_germ="numeric",min_temp_estab="numeric",max_temp_estab="numeric"))
swEstabSpecies_validity<-function(object){
	temp<-c(length(fileName), length(estab_lyrs), length(barsGERM), length(barsESTAB), length(min_pregerm_days), length(max_pregerm_days), length(min_wetdays_for_germ), 
			length(max_drydays_postgerm), length(min_wetdays_for_estab), length(min_days_germ2estab), length(max_days_germ2estab), length(min_temp_germ), length(max_temp_germ), length(min_temp_estab), length(max_temp_estab))
	if(length(unique(temp)) != 1)
		return("Missing values...")
	TRUE
}
setValidity("swEstabSpecies",swEstabSpecies_validity)

setMethod(f="swClear",
		signature="swEstabSpecies",
		definition=function(object) {
			object@fileName = character(0)
			object@estab_lyrs = integer(0)
			object@barsGERM = numeric(0)
			object@barsESTAB = numeric(0)
			object@min_pregerm_days = integer(0)
			object@max_pregerm_days = integer(0)
			object@min_wetdays_for_germ = integer(0)
			object@max_drydays_postgerm = integer(0)
			object@min_wetdays_for_estab = integer(0)
			object@min_days_germ2estab = integer(0)
			object@max_days_germ2estab = integer(0)
			object@min_temp_germ = integer(0)
			object@max_temp_germ = numeric(0)
			object@min_temp_estab = numeric(0)
			object@max_temp_estab = numeric(0)
			return(object)
		})
setMethod("swWriteLines", signature=c(object="swEstabSpecies", file="character"), definition=function(object, file) {
			#Match the file to the spp to write. Kind of guessing since I have no examples
			index<-grep(pattern=strsplit(x=object@fileName,split=".")[1],x=object@fileName,value=F)[1]
			infilename <- file.path(file)
			infiletext <- character(15)
			infiletext[1] = object@fileName[index]
			infiletext[2] = object@estab_lyrs[index]
			infiletext[3] = object@barsGERM[index]
			infiletext[4] = object@barsESTAB[index]
			infiletext[5] = object@min_pregerm_days[index]
			infiletext[6] = object@max_pregerm_days[index]
			infiletext[7] = object@min_wetdays_for_germ[index]
			infiletext[8] = object@max_drydays_postgerm[index]
			infiletext[9] = object@min_wetdays_for_estab[index]
			infiletext[10] = object@min_days_germ2estab[index]
			infiletext[11] = object@max_days_germ2estab[index]
			infiletext[12] = object@min_temp_germ[index]
			infiletext[13] = object@max_temp_germ[index]
			infiletext[14] = object@barsESTAB[index]
			infiletext[15] = object@barsESTAB[index]
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swEstabSpecies",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			index<-length(object@fileName)+1
			object@fileName[index] = infiletext[1]
			object@estab_lyrs[index] = readInteger(infiletext[2])
			object@barsGERM[index] = readNumeric(infiletext[3])
			object@barsESTAB[index] = readNumeric(infiletext[4])
			object@min_pregerm_days[index] = readInteger(infiletext[5])
			object@max_pregerm_days[index] = readInteger(infiletext[6])
			object@min_wetdays_for_germ[index] = readInteger(infiletext[7])
			object@max_drydays_postgerm[index] = readInteger(infiletext[8])
			object@min_wetdays_for_estab[index] = readInteger(infiletext[9])
			object@min_days_germ2estab[index] = readInteger(infiletext[10])
			object@max_days_germ2estab[index] = readInteger(infiletext[11])
			object@min_temp_germ[index] = readInteger(infiletext[12])
			object@max_temp_germ[index] = readNumeric(infiletext[13])
			object@min_temp_estab[index] = readNumeric(infiletext[14])
			object@max_temp_estab[index] = readNumeric(infiletext[15])
			return(object)
		})

#############################ESTAB.IN#########################################
swEstab <- setClass(Class="swEstab",representation(useEstab="logical"),prototype=prototype(useEstab=FALSE),contains="swEstabSpecies")
setMethod(f="swClear",
		signature="swEstab",
		definition=function(object) {
			object@useEstab = logical(1)
			object@fileName = character(0)
			object@estab_lyrs = integer(0)
			object@barsGERM = numeric(0)
			object@barsESTAB = numeric(0)
			object@min_pregerm_days = integer(0)
			object@max_pregerm_days = integer(0)
			object@min_wetdays_for_germ = integer(0)
			object@max_drydays_postgerm = integer(0)
			object@min_wetdays_for_estab = integer(0)
			object@min_days_germ2estab = integer(0)
			object@max_days_germ2estab = integer(0)
			object@min_temp_germ = integer(0)
			object@max_temp_germ = numeric(0)
			object@max_temp_estab = numeric(0)
			object@max_temp_estab = numeric(0)
			return(object)
		})
setMethod("swEstab_useEstab", "swEstab", function(object) {return(object@useEstab)})
setReplaceMethod(f="swEstab_useEstab", signature="swEstab", definition=function(object,value) {object@useEstab <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swEstab", file="character"), definition=function(object, file) {
			infilename <- file.path(file)
			infiletext <- character(9+length(object@fileName))
			infiletext[1] = "# list of filenames for which to check establishment"
			infiletext[2] = "# each filename pertains to a species and contains the"
			infiletext[3] = "# soil moisture and timing parameters required for the"
			infiletext[4] = "# species to establish in a given year."
			infiletext[5] = "# There is no limit to the number of files in the list."
			infiletext[6] = "# to suppress checking establishment, comment all the"
			infiletext[7] = "# lines below."
			infiletext[9] = paste(as.character(as.integer(object@useEstab)),"\t# use flag; 1=check establishment, 0=don't check, ignore following",sep="")
			for(i in 1:object@count) {
				infiletext[9+i] = object@fileName[i]
			}
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swEstab",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)
			index<-length(object@fileName)+1
			object@useEstab = readLogical(infiletext[9])
			object@count = 0L
			if(object@useEstab) {
				infiletext <- infiletext[-c(1:9)]
				infiletext <- infiletext[infiletext != ""]
				for(i in 1:length(infiletext)) {
					#see if the line is commented out
					line<-strsplit(x=infiletext[i],split=c("#"," "))[[1]][1]
					if(line != "") {
						object@count <- object@count + 1L
						as(object,"swEstabSpecies") <- swReadLines(as(object,"swEstabSpecies"),paste(line,".estab",sep=""))
					}
				}
			}
			return(object)
		})

