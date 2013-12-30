print("Rsw")
sw_exec <- function(data=NULL,weatherList=NULL,dir="", file.in="files_v27.in", echo=FALSE, quiet=FALSE, colNames=FALSE) {
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(file.in!="")
		input<-c(input,"-f", file.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	data <- .Call("start",input,data,weatherList)
	if(colNames)
		.Call("onSetNames",data)
	return(data)
}

sw_inputDataFromFiles <- function(dir="", file.in="files_v27.in", echo=FALSE, quiet=FALSE, colNames=FALSE) {
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(file.in!="")
		input<-c(input,"-f", file.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	data <- .Call("onGetInputDataFromFiles",input)
	if(colNames)
		.Call("onSetNames",data)
	return(data)
}

sw_inputData <- function() {
	temp<-swInputData()
	temp@weatherHistory <- list('1982'=swWeatherData(data=year1982,year=as.integer(1982)),
						'1983'=swWeatherData(data=year1983,year=as.integer(1983)),
						'1984'=swWeatherData(data=year1984,year=as.integer(1984)),
						'1985'=swWeatherData(data=year1985,year=as.integer(1985)),
						'1986'=swWeatherData(data=year1986,year=as.integer(1986)))
	return(temp)
}
