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


# This File is the object that will hold all the output from Soilwat.
# 
# Author: Ryan Murphy
###############################################################################
tLayers <- 8
tevapLayers <- 3
tVegEstabCount <- 2
numPeriods <- 4
###################Generic Class to Hold One Output KEY########################
swOutput_KEY <- setClass(Class="swOutput_KEY",representation(Title="character",TimeStep="integer",Columns="integer",Day="matrix",Week="matrix",Month="matrix",Year="matrix"))
setMethod("swOutput_KEY_Period","swOutput_KEY", function(object,index) {slot(object,slotNames(object)[-(1:3)][index])})
setMethod("swOutput_KEY_TimeStep","swOutput_KEY", function(object) { if(length(object@TimeStep)==1 & object@TimeStep <= 4) return(object@TimeStep) else stop("TimeStep for WTHR to long or out of Range.") })
setMethod("swOutput_KEY_Columns","swOutput_KEY", function(object) { return(object@Columns) })

setReplaceMethod(f="swOutput_KEY_Period", signature="swOutput_KEY", definition=function(object,index,value) {slot(object,slotNames(object)[-(1:3)][index]) <- value; return(object)})

swOutput_WTHR <- new("swOutput_KEY",Title="",TimeStep=4L,Columns=0L)
swOutput_TEMP <- new("swOutput_KEY",Title="temp_air",TimeStep=1L,Columns=4L)
swOutput_PRECIP <- new("swOutput_KEY",Title="precip",TimeStep=2L,Columns=5L)
swOutput_SOILINFILT <- new("swOutput_KEY",Title="infiltration",TimeStep=3L,Columns=1L)
swOutput_RUNOFF <- new("swOutput_KEY",Title="runoff",TimeStep=1L,Columns=3L)
swOutput_ALLH2O <- new("swOutput_KEY",Title="",TimeStep=4L,Columns=0L)
swOutput_VWCBULK <- new("swOutput_KEY",Title="vwc_bulk",TimeStep=2L,Columns=as.integer(tLayers))
swOutput_VWCMATRIC <- new("swOutput_KEY",Title="vwc_matric",TimeStep=3L,Columns=as.integer(tLayers))
swOutput_SWCBULK <- new("swOutput_KEY",Title="swc_bulk",TimeStep=0L,Columns=as.integer(tLayers))
swOutput_SWPMATRIC <- new("swOutput_KEY",Title="swp_matric",TimeStep=1L,Columns=as.integer(tLayers))
swOutput_SWABULK <- new("swOutput_KEY",Title="swa_bulk",TimeStep=2L,Columns=as.integer(tLayers))
swOutput_SWAMATRIC <- new("swOutput_KEY",Title="swa_matric",TimeStep=3L,Columns=as.integer(tLayers))
swOutput_SURFACEWATER <- new("swOutput_KEY",Title="surface_water",TimeStep=0L,Columns=1L)
swOutput_TRANSP <- new("swOutput_KEY",Title="transp",TimeStep=3L,Columns=as.integer(tLayers)*5L)
swOutput_EVAPSOIL <- new("swOutput_KEY",Title="evap_soil",TimeStep=0L,Columns=as.integer(tevapLayers))
swOutput_EVAPSURFACE <- new("swOutput_KEY",Title="evap_surface",TimeStep=1L,Columns=7L)
swOutput_INTERCEPTION <- new("swOutput_KEY",Title="interception",TimeStep=2L,Columns=7L) # was 6L
swOutput_LYRDRAIN <- new("swOutput_KEY",Title="percolation",TimeStep=0L,Columns=as.integer(tLayers-1))
swOutput_HYDRED <- new("swOutput_KEY",Title="hydred",TimeStep=1L,Columns=as.integer(tLayers*5))
swOutput_ET <- new("swOutput_KEY",Title="",TimeStep=4L,Columns=0L)
swOutput_AET <- new("swOutput_KEY",Title="aet",TimeStep=3L,Columns=1L)
swOutput_PET <- new("swOutput_KEY",Title="pet",TimeStep=0L,Columns=1L)
swOutput_WETDAY <- new("swOutput_KEY",Title="wetdays",TimeStep=0L,Columns=as.integer(tLayers))
swOutput_SNOWPACK <- new("swOutput_KEY",Title="snowpack",TimeStep=1L,Columns=2L)
swOutput_DEEPSWC <- new("swOutput_KEY",Title="deep_drain",TimeStep=2L,Columns=1L)
swOutput_SOILTEMP <- new("swOutput_KEY",Title="temp_soil",TimeStep=2L,Columns=as.integer(tLayers))
swOutput_ALLVEG <- new("swOutput_KEY",Title="",TimeStep=4L,Columns=0L)
swOutput_ESTABL <- new("swOutput_KEY",Title="estabs",TimeStep=3L,Columns=as.integer(tVegEstabCount))
##################Main Storage##################
swOutput <- setClass(Class="swOutput",representation(yr_nrow="integer",mo_nrow="integer",wk_nrow="integer",dy_nrow="integer",WTHR="swOutput_KEY",TEMP="swOutput_KEY",PRECIP="swOutput_KEY",SOILINFILT="swOutput_KEY",RUNOFF="swOutput_KEY",ALLH2O="swOutput_KEY",VWCBULK="swOutput_KEY",
				VWCMATRIC="swOutput_KEY",SWCBULK="swOutput_KEY",SWABULK="swOutput_KEY",SWAMATRIC="swOutput_KEY",SWPMATRIC="swOutput_KEY",SURFACEWATER="swOutput_KEY",TRANSP="swOutput_KEY",
				EVAPSOIL="swOutput_KEY",EVAPSURFACE="swOutput_KEY",INTERCEPTION="swOutput_KEY",LYRDRAIN="swOutput_KEY",HYDRED="swOutput_KEY",ET="swOutput_KEY",AET="swOutput_KEY",PET="swOutput_KEY",WETDAY="swOutput_KEY",SNOWPACK="swOutput_KEY",
				DEEPSWC="swOutput_KEY",SOILTEMP="swOutput_KEY",ALLVEG="swOutput_KEY",ESTABL="swOutput_KEY"), prototype(WTHR=swOutput_WTHR, TEMP=swOutput_TEMP, PRECIP=swOutput_PRECIP,SOILINFILT=swOutput_SOILINFILT,RUNOFF=swOutput_RUNOFF, ALLH2O=swOutput_ALLH2O, VWCBULK=swOutput_VWCBULK,
				VWCMATRIC=swOutput_VWCMATRIC, SWCBULK=swOutput_SWCBULK, SWPMATRIC=swOutput_SWPMATRIC, SWABULK=swOutput_SWABULK, SWAMATRIC=swOutput_SWAMATRIC, SURFACEWATER=swOutput_SURFACEWATER, TRANSP=swOutput_TRANSP,
				EVAPSOIL=swOutput_EVAPSOIL,EVAPSURFACE=swOutput_EVAPSURFACE,INTERCEPTION=swOutput_INTERCEPTION,LYRDRAIN=swOutput_LYRDRAIN,HYDRED=swOutput_HYDRED,ET=swOutput_ET,AET=swOutput_AET,PET=swOutput_PET,WETDAY=swOutput_WETDAY,SNOWPACK=swOutput_SNOWPACK,
				DEEPSWC=swOutput_DEEPSWC,SOILTEMP=swOutput_SOILTEMP,ALLVEG=swOutput_ALLVEG,ESTABL=swOutput_ESTABL) )

setMethod("$","swOutput",function(x,name) {slot(x,name)})
setMethod("swOutput_getKEY","swOutput", function(object,index) {slot(object,slotNames(object)[-(1:4)][index])})

setReplaceMethod(f="swOutput_getKEY", signature=c(object="swOutput",value="swOutput_KEY"), definition=function(object,index,value) {slot(object,slotNames(object)[-(1:4)][index]) <- value; return(object)})

setMethod(f="swClear",
		signature="swOutput",
		definition=function(object) {
			return(object)
		})
DaysInYear <- function(year) {
	if(year %% 4 == 0) {
		if(year %% 100 == 0) {
			if(year %% 400 == 0) {
				return(366L)
			} else {
				return(365L)
			}
		} else {
			return(366L)
		}
	} else {
		return(365L)
	}
}
setMethod(f="initialize",signature="swOutput",definition=function(.Object,Layers=8L, VegEstabCount=2L, EvapLayers=3L, StartYear=1982L,EndYear=1990L,FDOFY=1L,EDOEY=365L,useTimeStep=TRUE,timePeriods=c(3,2,1,0),period=as.integer(c(4,1,2,3,1,4,2,3,0,1,2,3,0,3,0,1,2,0,1,4,3,0,0,1,2,2,4,3)),swInputData=NULL){#weatherYearsIn=NULL
			if(!is.null(swInputData)) {
				Layers <- as.integer(nrow(swSoils_Layers(swInputData)))
				VegEstabCount <- as.integer(swInputData@estab@count)
				EvapLayers <- as.integer(length(swSoils_Layers(swInputData)[swSoils_Layers(swInputData)[,4]>0,4]))
				StartYear <- as.integer(swYears_StartYear(swInputData))
				EndYear <- as.integer(swYears_EndYear(swInputData))
				FDOFY <- as.integer(swYears_FDOFY(swInputData))
				EDOEY <- as.integer(swYears_EDOEY(swInputData))
				useTimeStep <- swOUT_useTimeStep(swInputData)
				timePeriods <- as.integer(swOUT_TimeStep(swInputData))
				period <- as.integer(swInputData@output@period)
				Key_use <- swInputData@output@use
			}
			#Calculate the rows
			Years <- as.integer(c(StartYear:EndYear))
			nYears <- as.integer(length(c(StartYear:EndYear)))
			yr_nrow <- nYears
			mo_nrow <- nYears * 12L
			wk_nrow <- nYears * 53L
			dy_nrow <- 0L
			for(i in 1:nYears) {
				days <- DaysInYear(Years[i])
				if(i == 1) {
					days <- length(FDOFY:days)
				} else if(i==nYears && EDOEY == 365 && days==366) {
					days <- 366
				} else if(i==nYears) {
					days <- length(1:EDOEY)
				}
				dy_nrow <- dy_nrow + days
			}
			.Object@yr_nrow <- as.integer(yr_nrow)
			.Object@mo_nrow <- as.integer(mo_nrow)
			.Object@wk_nrow <- as.integer(wk_nrow)
			.Object@dy_nrow <- as.integer(dy_nrow)
			
			nrow <- c(dy_nrow,wk_nrow,mo_nrow,yr_nrow)
			coladd <- c(2,2,2,1)
			#Calculate the Columns
			.Object@WTHR@Columns <- 0L
			.Object@TEMP@Columns <- 4L
			.Object@PRECIP@Columns <- 5L
			.Object@SOILINFILT@Columns <- 1L
			.Object@RUNOFF@Columns <- 3L
			.Object@ALLH2O@Columns <- 0L
			.Object@VWCBULK@Columns <- Layers
			.Object@VWCMATRIC@Columns <- Layers
			.Object@SWCBULK@Columns <- Layers
			.Object@SWPMATRIC@Columns <- Layers
			.Object@SWABULK@Columns <- Layers
			.Object@SWAMATRIC@Columns <- Layers
			.Object@SURFACEWATER@Columns <- 1L
			.Object@TRANSP@Columns <- Layers*5L
			.Object@EVAPSOIL@Columns <- EvapLayers
			.Object@EVAPSURFACE@Columns <- 7L
			.Object@INTERCEPTION@Columns <- 7L	# was 6L
			.Object@LYRDRAIN@Columns <- Layers-1L
			.Object@HYDRED@Columns <- Layers*5L
			.Object@ET@Columns <- 0L
			.Object@AET@Columns <- 1L
			.Object@PET@Columns <- 1L
			.Object@WETDAY@Columns <- Layers
			.Object@SNOWPACK@Columns <- 2L
			.Object@DEEPSWC@Columns <- 1L
			.Object@SOILTEMP@Columns <- Layers
			.Object@ALLVEG@Columns <- 0L
			.Object@ESTABL@Columns <- VegEstabCount
			
#			for(i in 1:28) {#loop through KEYS
#				if(Key_use[i]) {
#					#only create matrix for time periods used.
#					if(!useTimeStep) {
#						index<-swOutput_KEY_TimeStep(swOutput_getKEY(.Object,i))+1
#						temp <- swOutput_getKEY(.Object,i)
#						#swOutput_KEY_Period(temp,index) <- matrix(data=999,nrow=nrow[index],ncol=swOutput_KEY_Columns(temp)+coladd[index],byrow=TRUE)
#						swOutput_getKEY(.Object,i) <- temp
#					} else {
#						for(j in 1:length(timePeriods)) {#Loop through Time Period dy,wk,mo,yr
#							index<-timePeriods[j]+1
#							temp<-swOutput_getKEY(.Object,i)
#							#swOutput_KEY_Period(temp,index) <- matrix(data=999,nrow=nrow[index],ncol=swOutput_KEY_Columns(temp)+coladd[index],byrow=TRUE)
#							swOutput_getKEY(.Object,i) <- temp
#						}
#					}
#				}
#			}
			
			validObject(.Object)
			return(.Object)
		})





