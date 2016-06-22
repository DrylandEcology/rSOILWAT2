###############################################################################
#Rsoilwat and Rsoilwat31
#    Copyright (C) {2009-2016}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


# TODO: Add comment
# 
# Author: Ryan J. Murphy (2013)
###############################################################################


print("swEstab")
swEstabSpecies <- setClass(Class="swEstabSpecies",representation(fileName="character",Name="character",estab_lyrs="integer",barsGERM="numeric",barsESTAB="numeric",min_pregerm_days="integer",max_pregerm_days="integer",min_wetdays_for_germ="integer",
				max_drydays_postgerm="integer",min_wetdays_for_estab="integer",min_days_germ2estab="integer",max_days_germ2estab="integer",min_temp_germ="numeric",max_temp_germ="numeric",min_temp_estab="numeric",max_temp_estab="numeric"),
	prototype=prototype(fileName=c("Input/bouteloua.estab","Input/bromus.estab"), Name=c("bogr","brte"), estab_lyrs=c(2L,3L), barsGERM=c(10,10), barsESTAB=c(15,15), min_pregerm_days=c(60L,200L), max_pregerm_days=c(180L,365L), min_wetdays_for_germ=c(2L,6L),
			max_drydays_postgerm=c(40L,45L), min_wetdays_for_estab=c(5L,6L), min_days_germ2estab=c(15L,15L), max_days_germ2estab=c(75L,90L), min_temp_germ=c(5,10), max_temp_germ=c(20,30), min_temp_estab=c(0,3), max_temp_estab=c(20,30)))
swEstabSpecies_validity<-function(object){
	temp<-c(length(fileName),length(Name), length(estab_lyrs), length(barsGERM), length(barsESTAB), length(min_pregerm_days), length(max_pregerm_days), length(min_wetdays_for_germ), 
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
			object@Name=character(0)
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
			object@min_temp_germ = numeric(0)
			object@max_temp_germ = numeric(0)
			object@min_temp_estab = numeric(0)
			object@max_temp_estab = numeric(0)
			return(object)
		})
setMethod("swWriteLines", signature=c(object="swEstabSpecies", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			index<-grep(pattern=basename(file),x=object@fileName,value=F)[1]
			infilename <- file.path(file)
			infiletext <- character(15)
			infiletext[1] = paste(object@Name[index], "# 4-char name of species", sep="\t")
			infiletext[2] = "# soil layer parameters"
			infiletext[3] = paste(object@estab_lyrs[index], "# number of layers affecting establishment", sep="\t")
			infiletext[4] = paste(object@barsGERM[index], "# SWP (bars) requirement for germination (top layer)", sep="\t")
			infiletext[5] = paste(object@barsESTAB[index], "# SWP (bars) requirement for establishment (average of top layers)", sep="\t")
			infiletext[6] = "# timing parameters in days"
			infiletext[7] = paste(object@min_pregerm_days[index], "# first possible day of germination", sep="\t")
			infiletext[8] = paste(object@max_pregerm_days[index], "# last possible day of germination", sep="\t")
			infiletext[9] = paste(object@min_wetdays_for_germ[index], "# min number of consecutive \"wet\" days for germination to occur", sep="\t")
			infiletext[10] = paste(object@max_drydays_postgerm[index], "# max number of consecutive \"dry\" days after germination allowing estab", sep="\t")
			infiletext[11] = paste(object@min_wetdays_for_estab[index], "# min number of consecutive \"wet\" days after germination before establishment", sep="\t")
			infiletext[12] = paste(object@min_days_germ2estab[index], "# min number of days between germination and establishment", sep="\t")
			infiletext[13] = paste(object@max_days_germ2estab[index], "# max number of days between germination and establishment", sep="\t")
			infiletext[14] = "# temperature parameters in C"
			infiletext[15] = paste(object@min_temp_germ[index], "# min temp threshold for germination", sep="\t")
			infiletext[16] = paste(object@max_temp_germ[index], "# max temp threshold for germination", sep="\t")
			infiletext[17] = paste(object@min_temp_estab[index], "# min temp threshold for establishment", sep="\t")
			infiletext[18] = paste(object@max_temp_estab[index], "# max temp threshold for establishment", sep="\t")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swEstabSpecies",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file)

			object@Name = c(object@Name, gsub("[[:space:]]","",strsplit(x=infiletext[1],split = c("#"," ", "\t"),fixed=F)[[1]][1]))
			object@estab_lyrs = c(object@estab_lyrs,readInteger(infiletext[3]))
			object@barsGERM = c(object@barsGERM,readNumeric(infiletext[4]))
			object@barsESTAB = c(object@barsESTAB,readNumeric(infiletext[5]))
			object@min_pregerm_days = c(object@min_pregerm_days,readInteger(infiletext[7]))
			object@max_pregerm_days = c(object@max_pregerm_days,readInteger(infiletext[8]))
			object@min_wetdays_for_germ = c(object@min_wetdays_for_germ,readInteger(infiletext[9]))
			object@max_drydays_postgerm = c(object@max_drydays_postgerm,readInteger(infiletext[10]))
			object@min_wetdays_for_estab = c(object@min_wetdays_for_estab,readInteger(infiletext[11]))
			object@min_days_germ2estab = c(object@min_days_germ2estab,readInteger(infiletext[12]))
			object@max_days_germ2estab = c(object@max_days_germ2estab,readInteger(infiletext[13]))
			object@min_temp_germ = c(object@min_temp_germ,readInteger(infiletext[15]))
			object@max_temp_germ = c(object@max_temp_germ,readNumeric(infiletext[16]))
			object@min_temp_estab = c(object@min_temp_estab,readNumeric(infiletext[17]))
			object@max_temp_estab = c(object@max_temp_estab,readNumeric(infiletext[18]))
			return(object)
		})

#############################ESTAB.IN#########################################
swEstab <- setClass(Class="swEstab",representation(useEstab="logical",count="integer"),prototype=prototype(useEstab=TRUE,count=2L),contains="swEstabSpecies")
setMethod(f="swClear",
		signature="swEstab",
		definition=function(object) {
			object@useEstab = logical(1)
			object@fileName = character(0)
			object@Name = character(0)
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
setMethod("swEstab_useEstab", "swEstab", function(object) {return(object@useEstab)})
setReplaceMethod(f="swEstab_useEstab", signature="swEstab", definition=function(object,value) {object@useEstab <- value; return(object)})

setMethod("swWriteLines", signature=c(object="swEstab", file="character"), definition=function(object, file) {
			dir.create(path=dirname(file),showWarnings = FALSE, recursive = TRUE)
			infilename <- file.path(file)
			infiletext <- character(9 + ifelse(object@useEstab,object@count,0))
			infiletext[1] = "# list of filenames for which to check establishment"
			infiletext[2] = "# each filename pertains to a species and contains the"
			infiletext[3] = "# soil moisture and timing parameters required for the"
			infiletext[4] = "# species to establish in a given year."
			infiletext[5] = "# There is no limit to the number of files in the list."
			infiletext[6] = "# to suppress checking establishment, comment all the"
			infiletext[7] = "# lines below."
			infiletext[9] = paste(as.character(as.integer(object@useEstab)),"\t# use flag; 1=check establishment, 0=don't check, ignore following",sep="")
			if(object@useEstab) {
				for(i in 1:object@count) {
					infiletext[9+i] <- object@fileName[i]
				}
			}
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
		})
setMethod("swReadLines", signature=c(object="swEstab",file="character"), definition=function(object,file) {
			infiletext <- readLines(con = file[1])
			index<-length(object@fileName)+1
			object@useEstab = readLogical(infiletext[9])
			object@count = 0L
			if(object@useEstab) {
				infiletext <- infiletext[-c(1:9)]
				infiletext <- infiletext[infiletext != ""]
				for(i in 1:length(infiletext)) {
					#see if the line is commented out
					line<-gsub("[[:space:]]","",strsplit(x=infiletext[i],split=c("#"))[[1]][1])
					if(line != "") {
						object@fileName <- c(object@fileName, line)
						object@count <- object@count + 1L
						as(object,"swEstabSpecies") <- swReadLines(as(object,"swEstabSpecies"),file.path(file[2],line))
					}
				}
			}
			return(object)
		})

