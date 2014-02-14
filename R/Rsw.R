print("Rsw")
sw_exec <- function(inputData=NULL,weatherList=NULL,dir="", files.in="files_v30.in", echo=FALSE, quiet=FALSE) {
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(files.in!="")
		input<-c(input,"-f", files.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	if(is.null(inputData)) {
		inputData<-sw_inputDataFromFiles(dir=dir,files.in=files.in)
	}
	#if(colNames)
	#	.Call("onSetNames",data)
	return(.Call("start",input,inputData,weatherList))
}

sw_inputDataFromFiles <- function(dir="", files.in="files_v30.in") {
	echo=FALSE
	quiet=FALSE
	colNames=FALSE
	input <- c("sw_v27")
	if(dir!="")
		input<-c(input,"-d", dir)
	if(files.in!="")
		input<-c(input,"-f", files.in)
	if(echo)
		input<-c(input,"-e")
	if(quiet)
		input<-c(input,"-q")
	data <- .Call("onGetInputDataFromFiles",input)
	if(colNames)
		.Call("onSetNames",data)
	return(data)
}

sw_outputData <- function(inputData) {
	.Call("onGetOutput",inputData)
}

sw_inputData <- function() {
	temp<-swInputData()
	data(package="Rsoilwat31", weatherData)
	temp@weatherHistory <- weatherData
	return(temp)
}
