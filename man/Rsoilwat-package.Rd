% 
% TODO: Review
% 
\name{Rsoilwat}
\alias{Rsoilwat}
\alias{Rsoilwat-package}
\title{Package `Rsoilwat': summary information}
\description{
  The package `Rsoilwat' is a R implementation of SoilWat Modeling Software.
  The C code was converted into a library with input/output functions to R.
  SoilWat is a simulator of the dynamics of soil water. Originally written
  in the 1900's by a highly trained team of Botany's finest. It is now
  maintained by Dr. William Lauenroth's Lab and Dr. Daniel Rodolphe Schlaepfer.
  The input data structure is S4. Generic functions that apply to the input 
  container or individual object control setting/getting input data. Output
  data is in the form of lists.
}

\section{Version}{
The version level of the package is given by the command 
\code{packageDescription("circular")}.
The most recent version of the package can be obtained from
github at \url{https://github.com/Burke-Lauenroth-Lab/Rsoilwat}
}

\section{Author}{
Ryan Murphy, Department of Botany, Electrical Engineer,
University of Wyoming, Laramie, Wyoming
(\url{http://www.google})

Dr. Daniel Rodolphe Schlaepfer, Department of Botany,
University of Wyoming, Laramie, Wyoming, USA
(\url{https://sites.google.com/site/drschlaepfer/home})
}

\section{Licence}{
This package and its documentation are usable under the terms of the 
"GNU General Public License", a copy of which is distributed with the 
package. While the software is freely usable, it would be appreciated
if a reference is inserted in publications or other work
which makes use of it; for this purpose, see the command
\code{citation("Rsoilwat")}. 
}
\section{Acknowledgements}{
The package has evolved through several versions, developed over some months.

Many thanks to all that points out bugs, provide suggestions and comments.
}
\seealso{
	\code{\link{sw_exec}}
	\code{\link{sw_inputData}}
	\code{\link{sw_inputDataFromFiles}}
	\code{\link{getWeatherData_database}}
	\code{\link{getWeatherData_folders}}
	\code{\link{swWriteLines}}
	\code{\link{swReadLines}}
	\code{\link{swClear}}
}
\keyword{Rsoilwat}
\keyword{SoilWater}
\keyword{soilwat}

