#Markov script that generates two site-specific files (mkv_covar.in and mkv_prob.in) required by the markov weather generator in SOILWAT2


#' Estimate coefficients for use by \var{SOILWAT2} weather generator
#'
#' Generates two site-specific files (\var{mkv_covar.in} and \var{mkv_prob.in})
#' required by the markov weather generator in \var{SOILWAT2}
#'
#' @section Note: This code is based on a script from \var{rSFSTEP2} at
#'   2019-Feb-10
#'   https://github.com/DrylandEcology/rSFSTEP2/commit/cd9e161971136e1e56d427a4f76062bbb0f3d03a
#' @param weatherData A list of elements of class
#'   \code{\linkS4class{swWeatherData}}.
#' @param yr A numeric value. The number of years covered by \code{weatherData}.
#'
#' @examples
#' res <- dbW_estimate_WeatherGenerator_coefficients(rSOILWAT2::weatherData,
#'   yr = length(rSOILWAT2::weatherData))
#'
#' @extern
dbW_estimate_WeatherGenerator_coefficients <- function(weatherData, yr = 30) {

    ############ make mkv_prob.in file #############
    #input data for particular site and scenario
    DGF<-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(weatherData))
    #add WET column
    DGF<-within(DGF, WET<-FALSE)
    DGF[(DGF$PPT_cm>0),"WET"]<-TRUE

    #add WET given WET or WET given DRY column
    for (i in 1:nrow(DGF))
    {
      if(i==1)
      {
        if((DGF$WET[i]==TRUE)&(DGF$WET[nrow(DGF)]==TRUE))
        {
          DGF$WW[i]=1
          DGF$WD[i]=0

        }
        else if((DGF$WET[i]==TRUE)&(DGF$WET[nrow(DGF)]==FALSE))
        {
          DGF$WW[i]=0
          DGF$WD[i]=1
        }
        else{
          DGF$WW[i]=0
          DGF$WD[i]=0
        }
      }

      else{

      if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==TRUE))
      {
        DGF$WW[i]=1
        DGF$WD[i]=0      }
      else if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==FALSE))
      {
        DGF$WW[i]=0
        DGF$WD[i]=1      }
      else{
        DGF$WW[i]=0
        DGF$WD[i]=0
      }
      }
    }

        #create vectors to store data
        DOY<-vector();p_W_W<-vector();p_W_D<-vector();PPT_avg<-vector();PPT_sd<-vector();CF.max.w<-vector();
        CF.max.d<-vector();CF.min.w<-vector();CF.min.d<-vector()

        #make a dataframe for storing mkv_prob.in data
        DF<-data.frame(DOY,p_W_W,p_W_D,PPT_avg,PPT_sd,CF.max.w,CF.max.d,CF.min.w,CF.min.d)

        #celcius to kelvin conversion
        DGF$Tmax_C<-DGF$Tmax_C+273.15
        DGF$Tmin_C<-DGF$Tmin_C+273.15

      for (i in 1:366) #loop through all possible days in all years
      {
          #probability of wet|wet is the number of wet given wet years for that day divided by the number
          #of total wet days from the previous day

          #prbability of wet|dry is the number of wet given dry years for that day divdied by the number of
          #total years (yrs identified by user) minus the total number of wet days from the previous day
          #or the number of dry days

          #sum the number of wet given wet years for that day and the number of wet given dry years for that day
		      p_W_W<-sum(DGF[(DGF$WW==1) & (DGF$DOY==i), 7])
          p_W_D<-sum(DGF[(DGF$WD==1) & (DGF$DOY==i), 8])

          #DOY 1: use the last day of the year to determine wet given wet
          if(i==1)
          {
			      #make sure that we are not dividing by 0, which would result in p_W_W of NaN (undefined)
            totalwet.doy1 <- ifelse(DGF$WET[nrow(DGF)], 1, 0)
            for(j in 1:(nrow(DGF) - 1)){
              if((DGF$DOY[j] > DGF$DOY[j + 1]) & DGF$WET[j]){
                totalwet.doy1 <- totalwet.doy1 + 1
              }
            }
            p_W_W<-ifelse(totalwet.doy1>0, p_W_W/totalwet.doy1, 0)
            p_W_D<-p_W_D/(yr-(sum(DGF[(DGF$WW==1) & (DGF$DOY==i+364), 7])+sum(DGF[(DGF$WD==1) & (DGF$DOY==i+364), 8])))

          #for all other DOY
          }else
          {
			      #make sure that we are not dividing by 0, which would result in p_W_W of NaN (undefined)
            totalwet.notdoy1=sum(DGF[(DGF$WW==1) & (DGF$DOY==i-1), 7]) + sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1), 8])
            p_W_W<-ifelse(totalwet.notdoy1>0, p_W_W/totalwet.notdoy1, 0)
            p_W_D<-p_W_D / (yr - (sum(DGF[(DGF$WW==1) & (DGF$DOY==i-1), 7]) + sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1), 8])))
          }

          CF.max.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),3])/mean(DGF[(DGF$DOY==i), 3]))) + (mean(DGF[(DGF$WET=="TRUE"), 3]) - mean(DGF[(DGF$DOY==i), 3])) / mean(DGF[(DGF$DOY==i), 3])
          if (CF.max.w=='NaN'){CF.max.w<-1}
          if (CF.max.w > 1.0) {CF.max.w<-1}
          CF.max.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),3])/mean(DGF[(DGF$DOY==i), 3]))) + (mean(DGF[(DGF$WET=="FALSE"), 3]) - mean(DGF[(DGF$DOY==i), 3])) / mean(DGF[(DGF$DOY==i), 3])
          if (CF.max.d=='NaN'){CF.max.d<-1}
          if (CF.max.d < 1.0) {CF.max.d<-1}
          CF.min.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),4])/mean(DGF[(DGF$DOY==i), 4]))) + (mean(DGF[(DGF$WET=="TRUE"), 4]) - mean(DGF[(DGF$DOY==i), 4])) / mean(DGF[(DGF$DOY==i), 4])
          if (CF.min.w=='NaN'){CF.min.w<-1}
          if (CF.min.w > 1.0) {CF.min.w<-1}
          CF.min.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),4])/mean(DGF[(DGF$DOY==i), 4]))) + (mean(DGF[(DGF$WET=="FALSE"), 4]) - mean(DGF[(DGF$DOY==i), 4])) / mean(DGF[(DGF$DOY==i), 4])
          if (CF.min.d=='NaN'){CF.min.d<-1}
          if (CF.min.d < 1.0) {CF.min.d<-1}
          PPT_avg <- ifelse((sum(DGF$DOY==i & DGF$WET) > 0), (sum(DGF$PPT_cm[DGF$DOY == i]) / sum(DGF$DOY==i & DGF$WET)), 0) #average the ppt across all the years for that day
          PPT_sd<-(sd((DGF[(DGF$DOY==i), 5]))) #standard deviation the ppt across all the years for that day

          # PPT_sd == na iff there were no wet days for the previous day. This will set PPT_sd to 0 if this occurs.
	 	      if(is.na(PPT_sd)){PPT_sd <- 0}
          CF.max.w<-CF.max.w
          CF.max.d<-CF.max.d
          CF.min.w<-CF.min.w
          CF.min.d<-CF.min.d

          newrow<-data.frame(DOY=i,p_W_W=p_W_W,p_W_D=p_W_D,PPT_avg=PPT_avg,PPT_sd=PPT_sd,CF.max.w=CF.max.w,CF.max.d=CF.max.d,CF.min.w=CF.min.w,CF.min.d=CF.min.d)
          DF<-rbind(DF,newrow)
      }

###########################################################################

    ################## Write mkv_covar.in FILE  ##############################
    DGF_covar<-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(weatherData))
    for (k in 1:nrow(DGF_covar))
    {
      tempdate<-strptime(paste(DGF_covar$Year[k],DGF_covar$DOY[k]),format="%Y %j")
      #tempdate<-strptime(paste(DGF_covar$year[k],DGF_covar$DOY[k]),format="%Y %j")
      DGF_covar$WEEK[k]<-as.numeric(strftime(tempdate,format="%W")) # if PPT >0 the day is labeled wet
      DGF_covar$WEEK[k]<-DGF_covar$WEEK[k]+1
    }

    WEEK<-vector();T.MAX.C<-vector();T.MIN.C<-vector();cov_MINMIN<-vector();cov_MAXMIN<-vector();cov_MINMAX<-vector();
    cov_MAXMAX<-vector();

    #make a dataframe for storing mkv_prob.in data
    DF_covar<-data.frame(WEEK,T.MAX.C,T.MIN.C,cov_MINMIN,cov_MAXMIN,cov_MINMAX,cov_MAXMAX)

    for (w in 1:53) {
    WEEK<-w
    min<-(DGF_covar[(DGF_covar$WEEK==w),4])
    max<-(DGF_covar[(DGF_covar$WEEK==w),3])
    MIN.MAX<-cov(min,max) #covariance between min and max temp over all days in week for all years
    MIN.MIN<-cov(min,min) #covariance between min temps over all days in week for all years
    MAX.MAX<-cov(max,max) #covariance between max temps over all days in week for all years
    MAX.MIN<-MIN.MAX
    T.MAX.C<-mean(max) #mean max temp for the week
    T.MIN.C<-mean(min) #mean min temp for the week

    newrow<-data.frame(WEEK=WEEK,T.MAX.C=T.MAX.C,T.MIN.C=T.MIN.C,cov_MINMIN=MIN.MIN,cov_MAXMIN=MAX.MIN,cov_MINMAX=MIN.MAX,cov_MAXMAX=MAX.MAX)
    DF_covar<-rbind(DF_covar,newrow)

    }

    list(DF = DF, DF_covar = DF_covar)
}

#' Print Markov weather generator files as required by \var{SOILWAT2}
#'
#' @examples
#' res <- dbW_estimate_WeatherGenerator_coefficients(rSOILWAT2::weatherData)
#' print_mkv_files(DF = res[["DF"]], DF_covar = res[["DF_covar"]],
#'   path = normalizePath("."))
#'
#' @extern
print_mkv_files <- function(DF, DF_covar, path) {
  # print out the probability file
  colnames(DF)<-c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")# relabel the columns names
  rownames(DF)<- NULL
  write.table(format(DF, digits=5), file=file.path(path, "mkv_prob.in"), sep="\t", row.names=F,quote=F) #write your year file

  #Then write the files
  #rename columns
  colnames(DF_covar)<-c("#WEEK","T.MAX.C", "T.MIN.C","cov[MIN.MIN]","cov[MAX.MIN]","cov[MIN.MAX]","cov[MAX.MAX]")# relabel the columns names
  rownames(DF_covar)<- NULL #make rownames null (need this or else will have an extra column)
  write.table(format(DF_covar, digits=5), file=file.path(path, "mkv_covar.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory
}
