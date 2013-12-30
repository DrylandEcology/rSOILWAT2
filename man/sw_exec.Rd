% File ./man/load.Rd
     \name{sw_exec}
     \alias{SoilWat}
     \title{SoilWat Wrapper}
     \description{
       Executes and returns SoilWat data via List.
     }
     \usage{
     sw_exec(colNames=FALSE)
     }
     \arguments{
       \item{colNames}{If true, will set the List names and colnames on the List.}
     }
     \examples{
     ## No column names
     run1<-sw_exec()
     run1[[1]][[1]] #aet yr
     ## Column Names
     run2<-sw_exec(colNames)
     run2$aet$yr
     
     }
     \keyword{sw_exec}