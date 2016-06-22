% 
% TODO: Review
% 
\name{Rsoilwat31}
\alias{Rsoilwat31}
\alias{Rsoilwat31-package}
\title{Package `Rsoilwat31': summary information}
\description{
  The package `Rsoilwat' / 'Rsoilwat31' is a R implementation of SoilWat Modeling Software.
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
\code{packageDescription("Rsoilwat31")}.
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
\code{citation("Rsoilwat31")}. 
}

\references{
	Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth (2014)
	Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The consequences of climate change and disturbance.
	\emph{Ecosystems} \bold{17}:590--605.
	
	Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford (2012)
	Ecohydrological niche of sagebrush ecosystems.
	\emph{Ecohydrology} \bold{5}:453--466.

	Parton, W.J. (1978).
	\emph{Abiotic section of ELM}. In: Grassland simulation model (ed. Innis, G.S.).
	Springer New York, NY, pp. 31--53.
	
	Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992)
	Long-term soil-water dynamics in the shortgrass steppe.
	\emph{Ecology} \bold{73}:1175--1181.
}

\section{Acknowledgements}{
The package has evolved through several versions, developed over some months.

Many thanks to all that points out bugs, provide suggestions and comments.
}
\seealso{
	\itemize{
		\item \code{\link{sw_exec}} for running a simulation
		\item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}} for data input
		\item \code{\link{dbW_getWeatherData}} and \code{\link{getWeatherData_folders}} for weather data input
	}
}
