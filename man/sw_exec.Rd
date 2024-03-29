% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rsw.R
\name{sw_exec}
\alias{sw_exec}
\title{Execute a \pkg{rSOILWAT2} simulation run}
\usage{
sw_exec(
  inputData = NULL,
  weatherList = NULL,
  dir = ".",
  files.in = "files.in",
  echo = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{inputData}{an object of the \var{S4} class
\code{\linkS4class{swInputData}} which is generated from
\code{\link{sw_inputData}} or \code{\link{sw_inputDataFromFiles}}.}

\item{weatherList}{a list of weather data generated via
\code{\link{dbW_getWeatherData}} or \code{\link{getWeatherData_folders}}.}

\item{dir}{a character vector that represents the path to the input data. Use
with \code{files.in}}

\item{files.in}{A character string. The file name (and path relative to
\code{dir}) of the \var{files} input file that contains information
about the remaining input files.}

\item{echo}{logical. This option will echo the inputs to the \var{logfile}.
Helpful for debugging.}

\item{quiet}{logical. Quiet mode hides any \pkg{SOILWAT2} messages,
see \code{\link{sw_verbosity}}.}
}
\value{
An object of class \code{\linkS4class{swOutput}}.
}
\description{
Run the simulation and get the output data.  Executes the \pkg{SOILWAT2}
simulation model.  Uses \code{\link[base]{.Call}} to pass data to/from the C
library.
}
\details{
The input data for a simulation run can be passed to the function
\code{\link{sw_exec}} either as \code{\linkS4class{swInputData}} and
\code{weatherList} or as text files organized in the folder \code{dir}
and explained in \code{files.in}.

There are three ways to pass daily forcing weather data to the simulation:
\itemize{
  \item as values in the slot of the argument
        \code{\linkS4class{swInputData}},
  \item as separate \code{weatherList} which takes precedence over the
        weather data contained in \code{\linkS4class{swInputData}}, or
  \item as instructions to read in values from files if \code{dir} and
        \code{files.in} are set.
}

If you have missing weather data, then you have to impute yourself or use the
built-in Markov weather generator (see examples section). If you use the
weather generator, then you have to provide appropriate values for the input
(files) \var{mkv_covar.in} and \var{mkv_prob.in} for your simulation run -
see \code{\link{dbW_estimate_WGen_coefs}} or
\code{\link{dbW_generateWeather}}.
}
\examples{

## ------ Simulation with demonstration data ------------
## Access demonstration data (including daily weather forcing)
sw_in <- rSOILWAT2::sw_exampleData

## Slots of the input object of \code{\linkS4class{swInputData}}
utils::str(sw_in, max.level = 2)

## Execute the simulation run
sw_out <- sw_exec(inputData = sw_in)


## ------ Directory to a SOLWAT project used in the following examples
path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")

## ------ Simulation with data read from disk during execution ------------
## Execute the simulation run
sw_out1 <- sw_exec(dir = path_demo, files.in = "files.in", quiet = TRUE)

## Slots of the output object of class 'swOutput'
utils::str(sw_out1, max.level=2)


## ------ Simulation with data prepared beforehand ------------
## Read inputs from files on disk (including daily weather forcing)
sw_in2 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")

## Slots of the input object of \code{\linkS4class{swInputData}}
utils::str(sw_in2, max.level = 2)

## Execute the simulation run
sw_out2 <- sw_exec(inputData = sw_in2, quiet = TRUE)


## ------ Simulation with data prepared beforehand and separate weather data
## Read inputs from files on disk
sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")

## Read forcing weather data from files on disk (there are also functions
##   to set up a SQLite database for the weather data)
sw_weath3 <- getWeatherData_folders(
   LookupWeatherFolder = file.path(path_demo, "Input"),
   weatherDirName = "data_weather",
   filebasename = "weath",
   startYear = 1979,
   endYear = 2010
)

## List of the slots of the input objects of class 'swWeatherData'
utils::str(sw_weath3, max.level = 1)

## Execute the simulation run
sw_out3 <- sw_exec(inputData = sw_in3, weatherList = sw_weath3, quiet = TRUE)


## ------ Simulation with manipulated input data ------------
sw_in4 <- sw_in3

## Set the vegetation composition to 40\% grass and 60\% shrubs
swProd_Composition(sw_in4) <- c(0.4, 0.6, 0, 0, 0)

## Execute the simulation run
sw_out4 <- sw_exec(inputData = sw_in4, weatherList = sw_weath3, quiet = TRUE)



## ------ Simulation with CO2-effects ------------
sw_in5 <- sw_in3

## Turn on the CO2-effects and set the CO2-concentration scenario
swCarbon_Scenario(sw_in5) <- "RCP85"
swCarbon_Use_Bio(sw_in5) <- 1L
swCarbon_Use_WUE(sw_in5) <- 1L

## Execute the simulation run
sw_out5 <- sw_exec(inputData = sw_in5, weatherList = sw_weath3, quiet = TRUE)



## ------ Simulation with reduced set of outputs ------------
sw_in6 <- sw_in3

swof <- rSOILWAT2::sw_out_flags()
tmp <- c(
  "sw_temp", "sw_precip", "sw_snow",
  "sw_inf_soil", "sw_deepdrain",
  "sw_swcbulk", "sw_swp", "sw_soiltemp",
  "sw_aet", "sw_pet",
  "sw_veg"
)

slot(slot(sw_in6, "output"), "use")[] <- FALSE
activate_swOUT_OutKey(sw_in6) <- swof[names(swof) \%in\% tmp]
rSOILWAT2::swOUT_TimeStepsForEveryKey(sw_in6) <-
  c(daily = 0, monthly = 2, yearly = 3)

## Execute the simulation run
sw_out6 <- sw_exec(inputData = sw_in6, weatherList = sw_weath3, quiet = TRUE)

print(round(as.numeric(object.size(sw_out6) / object.size(sw_out5)), 2))


## ------ Simulation with different SWRC ------------
if (requireNamespace("curl") && curl::has_internet()) {
  sw_in7 <- sw_in3
  swSite_SWRCflags(sw_in7) <- c("vanGenuchten1980", "Rosetta3")

  sw_out7 <- sw_exec(inputData = sw_in7, weatherList = sw_weath3)
}

## See help(package = "rSOILWAT2") for a full list of functions

}
\references{
Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth (2014)
  Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The
  consequences of climate change and disturbance.  \emph{Ecosystems}
  \bold{17}:590--605.

  Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford (2012)
  Ecohydrological niche of sagebrush ecosystems.  \emph{Ecohydrology}
  \bold{5}:453--466.

  Parton, W.J. (1978).  \emph{Abiotic section of ELM}. In: Grassland
  simulation model (ed. Innis, G.S.).  Springer New York, NY, pp. 31--53.

  Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992) Long-term soil-water
  dynamics in the shortgrass steppe.  \emph{Ecology} \bold{73}:1175--1181.
}
\seealso{
\itemize{
  \item \code{\link{sw_exec}} for running a simulation
  \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}}
        for data input
  \item \code{\link{dbW_getWeatherData}} and
        \code{\link{getWeatherData_folders}} for weather data input.
}
}
