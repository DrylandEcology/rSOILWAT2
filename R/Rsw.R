###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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


sw_args <- function(dir, files.in, echo, quiet) {
  input <- c("SOILWAT2")

  if (dir != "")
    input <- c(input, "-d", dir)
  if (files.in != "")
    input <- c(input, "-f", files.in)
  if (echo)
    input <- c(input, "-e")
  if (quiet)
    input <- c(input, "-q")

  input
}



#' Execute a SOILWAT simulation run
#'
#' Run the simulation and get the output data.  Executes Soil Water simulator
#' and returns Soil Water data via List.  Uses .Call to pass data to/from C
#' library.
#'
#' The input data for a simulation run can be passed to the function
#' \code{\link{sw_exec}} either as \code{swInputData} and \code{weatherList} or
#' as text files organized in a folder \code{dir} and explained in
#' \code{files.in}.
#'
#' The weather data can be a part of the input data S4 class or separate as a
#' list to reduce the input object size.  The option 'weatherList' if set will
#' use the weatherList provided and not the inputdata object's weather data if
#' it has it. Weather data can also be read in when 'dir' and 'files.in' are
#' set.
#'
#' SOILWAT does not handle missing data. If you have missing data, then you
#' have to impute yourself or use the built-in Markov weather generator (see
#' examples section). If you use the weather generator, then you have to
#' provide appropriate values for the input (files) 'mkv_covar.in' and
#' 'mkv_prob.in' for your simulation run - currently, SOILWAT does not contain
#' code to estimate these values.
#'
#' @param inputData an object of the S4 class \code{swInputData} which is
#' generated from \code{\link{sw_inputData}} or
#' \code{\link{sw_inputDataFromFiles}}.
#' @param weatherList a list of weather data generated via
#' \code{\link{dbW_getWeatherData}} or \code{\link{getWeatherData_folders}}.
#' @param dir a character vector that represents the path to the input data.
#' Use with files.in
#' @param files.in a character vector that represents the partial path of the
#' 'files.in' file
#' @param echo logical. This option will echo the inputs to the Log Data.
#' Helpful for debugging.
#' @param quiet logical. Quiet mode doesn't print messages to the logfile.
#' @return A S4 Object Containing 28 outputkey slots.  Each of those has slots
#' for Yr Mo Wk Dy.
#'
#' NOTE: WTHR ALLH2O ET ALLVEG do not work.
#'
#' The output also contains slots for the number of rows for each time period.
#' Individual outputKeys also have slots for time periods used. Title from
#' outputsetup. They also have the number of data columns.
#'
#' These elements contain the output data from soilwat based on the
#' outputSetup.  Output columns should all be labeled.
#' @author Ryan Murphy
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @references Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth (2014)
#' Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The
#' consequences of climate change and disturbance.  \emph{Ecosystems}
#' \bold{17}:590--605.
#'
#' Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford (2012)
#' Ecohydrological niche of sagebrush ecosystems.  \emph{Ecohydrology}
#' \bold{5}:453--466.
#'
#' Parton, W.J. (1978).  \emph{Abiotic section of ELM}. In: Grassland
#' simulation model (ed. Innis, G.S.).  Springer New York, NY, pp. 31--53.
#'
#' Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992) Long-term soil-water
#' dynamics in the shortgrass steppe.  \emph{Ecology} \bold{73}:1175--1181.
#' @examples
#'
#' ## ------ Simulation with demonstration data ------------
#' ## Access demonstration data (including daily weather forcing)
#' sw_in <- rSOILWAT2::sw_exampleData
#'
#' ## Slots of the input object of \code{\linkS4class{swInputData}}
#' str(sw_in, max.level = 2)
#'
#' ## Execute the simulation run
#' \dontrun{sw_out <- sw_exec(inputData = sw_in)}
#'
#'
#' ## ------ Directory to a SOLWAT project used in the following examples
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#'
#' ## ------ Simulation with data read from disk during execution ------------
#' ## Execute the simulation run
#' sw_out1 <- sw_exec(dir = path_demo, files.in = "files.in", quiet = TRUE)
#'
#' ## Slots of the output object of class 'swOutput'
#' str(sw_out1, max.level=2)
#'
#'
#' ## ------ Simulation with data prepared beforehand ------------
#' ## Read inputs from files on disk (including daily weather forcing)
#' sw_in2 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#'
#' ## Slots of the input object of \code{\linkS4class{swInputData}}
#' str(sw_in2, max.level = 2)
#'
#' ## Execute the simulation run
#' sw_out2 <- sw_exec(inputData = sw_in2, quiet = TRUE)
#'
#'
#' ## ------ Simulation with data prepared beforehand and separate weather data ------------
#' ## Read inputs from files on disk
#' sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#'
#' ## Read forcing weather data from files on disk (there are also functions to set up a
#' ##   SQLite database for the weather data)
#' sw_weath3 <- getWeatherData_folders(LookupWeatherFolder=file.path(path_demo, "Input"),
#'    weatherDirName = "data_weather", filebasename = "weath", startYear = 1979,
#'    endYear = 2010)
#'
#' ## List of the slots of the input objects of class 'swWeatherData'
#' str(sw_weath3, max.level = 1)
#'
#' ## Execute the simulation run
#' sw_out3 <- sw_exec(inputData = sw_in3, weatherList = sw_weath3, quiet = TRUE)
#'
#'
#' ## ------ Simulation with manipulated input data ------------
#' sw_in4 <- sw_in3
#'
#' ## Set the vegetation composition to 40% grass and 60% shrubs
#' swProd_Composition(sw_in4) <- c(0.4, 0.6, 0, 0, 0)
#'
#' ## Execute the simulation run
#' sw_out4 <- sw_exec(inputData = sw_in4, weatherList = sw_weath3, quiet = TRUE)
#'
#'
#'
#' ## ------ Simulation with CO2-effects ------------
#' sw_in5 <- sw_in3
#'
#' ## Turn on the CO2-effects and set the CO2-concentration scenario
#' swCarbon_Scenario(sw_in5) <- "RCP85"
#' swCarbon_Use_Bio(sw_in5) <- 1L
#' swCarbon_Use_WUE(sw_in5) <- 1L
#'
#' ## Execute the simulation run
#' sw_out4 <- sw_exec(inputData = sw_in4, weatherList = sw_weath3, quiet = TRUE)
#'
#'
#' ## See help(package = "rSOILWAT2") for a full list of functions
#'
#' @export
sw_exec <- function(inputData = NULL, weatherList = NULL, dir = "",
  files.in = "files.in", echo = FALSE, quiet = FALSE) {

  dir_prev <- getwd()
  on.exit(setwd(dir_prev), add = TRUE)

  input <- sw_args(dir, files.in, echo, quiet)

  if (is.null(inputData)) {
    inputData <- sw_inputDataFromFiles(dir = dir, files.in = files.in)
  }

  res <- .Call(C_start, input, inputData, weatherList, as.logical(quiet))

  if (.Call(C_tempError)) {
    # Error during soil temperature calculations
    # Re-initialize soil temperature output to 0
    st_name <- rSW2_glovars[["kSOILWAT2"]][["OutKeys"]][["SW_SOILTEMP"]]
    tempd <- slot(res, st_name)

    for (k in rSW2_glovars[["sw_TimeSteps"]]) {
      temp <- slot(tempd, k)
      np <- dim(temp)
      if (np[1] > 0) {
        icols <- seq.int(np[2] - slot(tempd, "Columns") + 1L, np[2])
        temp[, icols] <- matrix(0L, nrow = np[1], ncol = length(icols))
        slot(tempd, k) <- temp
      }
    }

    slot(res, st_name) <- tempd
  }

  res
}



#' rSOILWAT2 sw_inputDataFromFiles
#'
#' Reads in a SoilWat Project's input data.  Returns swInputData Object.
#'
#' swInputData Object is a S4 object containing slots for all input data.
#' Calling sw_inputDataFromFiles generates a new swInputData object with data
#' from the Project files given.
#'
#' The data returned can be directly used to run the simulation if weather Data
#' was included in the Project Directory. If weather data was not in the
#' project directory then use onGetWeatherData_database or
#' onGetWeatherData_folders to obtain weatherDataList and pass both to sw_exec.
#'
#' swInputData consists of slots for each file that is read in. These slots can
#' be accessed via the following functions: \tabular{ll}{ \code{get_Markov}
#' \tab #get markov prop and conv\cr \code{get_swCloud} \tab #get cloud\cr
#' \code{get_swFiles} \tab #get files.in\cr \code{get_swOUT} \tab #get
#' outpusetup.in\cr \code{get_swProd} \tab #get prod.in\cr \code{get_swSite}
#' \tab #get site.in\cr \code{get_swSoils} \tab #get soils.in\cr
#' \code{get_swSWC} \tab #get swcSetup.in\cr \code{get_swWeather} \tab #get
#' weatherSetup.in\cr \code{get_swWeatherData} \tab #get individual year\cr
#' \code{get_swYears} \tab #get years.in data object\cr
#' \code{get_WeatherHistory} \tab #get S4 of WeatherHistory\cr }
#'
#' generic functions to get/set individual elements follow a format: 'sw' +
#' file name + '_' + option, e.g.  \itemize{ \item \code{swFiles_Cloud} \item
#' \code{swProd_Albedo} } Remember tab complete is your friend.
#'
#' SOILWAT does not handle missing weather data. If you have missing data, then
#' you have to impute yourself or use the built-in Markov weather generator
#' (see examples for \code{\link{sw_exec}}).
#'
#' @param dir String. The path to the Project Directory. Use with files.in
#' @param files.in String. The partial path, after project dir path, and file
#' name to files.in file.
#' @return Returns a S4 class of type swInputData. This is a container for the
#' input S4 objects used to run soilwat. The data for the given project is
#' loaded.
#' @author Ryan Murphy
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @examples
#'
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#'
#'
#' ## ------ Simulation with data prepared beforehand ------------
#' ## Read inputs from files on disk (including daily weather forcing)
#' sw_in2 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#'
#' ## Slots of the input object of \code{\linkS4class{swInputData}}
#' str(sw_in2, max.level=2)
#'
#' ## Execute the simulation run
#' \dontrun{sw_out2 <- sw_exec(inputData = sw_in2)}
#'
#'
#' @export
sw_inputDataFromFiles <- function(dir = "", files.in = "files.in") {

  dir_prev <- getwd()
  on.exit(setwd(dir_prev), add = TRUE)

  input <- sw_args(dir, files.in, echo = FALSE, quiet = FALSE)

  .Call(C_onGetInputDataFromFiles, input)
}


#' Return output data
#'
#' @param inputData An object of class \code{\linkS4class{swInputData}}.
#' @return An object of class \code{\linkS4class{swOutput}}.
#' @export
sw_outputData <- function(inputData) {

  dir_prev <- getwd()
  on.exit(setwd(dir_prev), add = TRUE)

	.Call(C_onGetOutput, inputData)
}



#' rSOILWAT2 sw_inputData
#'
#' Default data Set for SoilWat.  Use this as a template or for testing the
#' package.
#'
#' swInputData Object is a S4 object containing slots for all input data.
#' Calling sw_inputData() generates a new swInputData object with test project
#' data loaded into them. This can be used for testing or as a template to
#' build another project off of.
#'
#' The data returned can be directly used to run the simulation. Use the
#' function sw_exec and pass it the output from this function to run SW.
#'
#' \code{showMethods(class="swInputData")} # Will show all generic functions
#' that be applied to the inputData. These include getters/setters for slots.
#'
#' swInputData consists of slots for each file that is read in. These slots can
#' be accessed via the following functions: \tabular{ll}{ \code{get_Markov}
#' \tab #get markov prop and conv\cr \code{get_swCloud} \tab #get cloud\cr
#' \code{get_swFiles} \tab #get files.in\cr \code{get_swOUT} \tab #get
#' outpusetup.in\cr \code{get_swProd} \tab #get prod.in\cr \code{get_swSite}
#' \tab #get site.in\cr \code{get_swSoils} \tab #get soils.in\cr
#' \code{get_swSWC} \tab #get swcSetup.in\cr \code{get_swWeather} \tab #get
#' weatherSetup.in\cr \code{get_swWeatherData} \tab #get individual year\cr
#' \code{get_swYears} \tab #get years.in data object\cr
#' \code{get_WeatherHistory} \tab #get S4 of WeatherHistory\cr }
#'
#' generic functions to get/set individual elements follow a format: 'sw' +
#' file name + '_' + option, e.g.  \itemize{ \item \code{swFiles_Cloud} \item
#' \code{swProd_Albedo} } Remember tab complete is your friend.
#'
#' SOILWAT does not handle missing data. If you have missing data, then you
#' have to impute yourself or use the built-in Markov weather generator (see
#' examples for \code{\link{sw_exec}}).
#'
#' @return Returns a S4 class of type swInputData. This is a container for the
#' input S4 objects used to run soilwat. The data for a test project is loaded.
#' @author Ryan Murphy
#' @seealso \itemize{ \item \code{\link{sw_exec}} for running a simulation
#' \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for
#' data input \item \code{\link{dbW_getWeatherData}} and
#' \code{\link{getWeatherData_folders}} for weather data input }
#' @examples
#'
#' ## ------ Simulation with demonstration data ------------
#' ## Access demonstration data (including daily weather forcing)
#' sw_in <- rSOILWAT2::sw_exampleData
#'
#' ## Slots of the input object of class \code{\linkS4class{swInputData}}
#' str(sw_in, max.level=2)
#'
#' ## Execute the simulation run
#' \dontrun{sw_out <- sw_exec(inputData = sw_in)}
#'
#' @export
sw_inputData <- function() {
  dir_prev <- getwd()
  on.exit(setwd(dir_prev), add = TRUE)

  temp <- new("swInputData") # data are from calls to `initialize`-methods
  data(package = "rSOILWAT2", "weatherData", envir = environment())
  slot(temp, "weatherHistory") <- get("weatherData", envir = environment())

  temp
}


#' Check if soil temperature simulation failed
#'
#' @return A logical value
#' @export
has_soilTemp_failed <- function() {
  .Call(C_tempError)
}



#' Assign requested values to (scalar) input flags
#'
#' @param swIn An object of class \code{\linkS4class{swInputData}}.
#' @param tag A character string. This string is used to partially match names of parameter
#'  \code{use} which indicates which of the values should be manipulated.
#' @param use A logical named vector.
#' @param values A vector.
#' @param fun A character string. Identifies the method to extract and replace values.
#' @param reset A logical value.
#' @param default A scalar value.
#'
#' @section Details: If \code{reset} is \code{TRUE}, then function resets flags identified
#'  by \code{tag} and turned off as identified by \code{use} to \code{default}.
#   If \code{reset} is \code{FALSE}, then code sets flags identified by \code{tag} and
#'  turned on as identified by \code{use} to corresponding elements of \code{values};
#'  other flags are not changed.
#'
#' @return An updated version of \code{swIn}.
#' @export
set_requested_flags <- function(swIn, tag, use, values, fun, reset = TRUE, default = NA) {

  if (!inherits(swIn, "swInputData")) {
    stop(paste("ERROR: argument 'swIn' is not a class 'swInputData' object."))
  }

  val_names <- names(use)
  i_flags <- grepl(tag, val_names)
  i_fuse <- i_flags & use

  if (any(i_fuse)) {
    i_fuse <- which(i_fuse)
    val_names <- val_names[i_fuse]
    vals <- unlist(values[i_fuse])
    temp_bad <- !is.finite(as.numeric(vals))

    if (any(temp_bad)) {
      stop(paste("ERROR: column(s) of", tag,
        paste(shQuote(val_names[temp_bad]), "=", vals[temp_bad], collapse = " / "),
        "contain(s) unsuitable values"))

    } else {
      def <- get(fun)(swIn)

      def_mode <- mode(def)
      if (!identical(def_mode, mode(vals))) {
        vals <- as(vals, def_mode)
      }

      # Check dimensional agreement
      ndim_gt1_vals <- sum(dim(data.frame(vals)) > 1)
      ndim_gt1_def <- sum(dim(data.frame(def)) > 1)
      if (!(ndim_gt1_vals == 1 && ndim_gt1_def == 1)) {
        stop(paste("ERROR:", paste(shQuote(val_names), collapse = ", "),
          "are not represented as 1-dimensional objects in class 'swInputData'."))

      } else {
        # Transfer values
        itemp <- sapply(names(def), function(x) {
          k <- grep(substr(x, 1, 4), val_names)
          if (length(k) == 1) k else 0})
        def[itemp > 0] <- vals[itemp]

        if (reset) {
          def[itemp == 0] <- default
        }

        swIn <- get(paste0(fun, "<-"))(swIn, def)
      }
    }
  }

  swIn
}
