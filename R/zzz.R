###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
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


.onAttach <- function(library, pkgname) {
  if (interactive())  {
    packageStartupMessage(
      "Package ", shQuote(pkgname),
      " v", utils::packageVersion(pkgname),
      " (daily weather database v", rSW2_glovars$dbW_version, ")",
      " attached/loaded."
    )
  }

  invisible()
}

.onLoad <- function(libname, pkgname) {
  #--- Define package level variables that should be hidden from package user
  # 'rSW2_glovars' is defined in rSOILWAT2-package.R

  # Variables for interaction with SOILWAT2
  assign("kSOILWAT2", .Call(C_sw_consts), envir = rSW2_glovars)

  # Variables for weather database functionality
  assign("con", NULL, envir = rSW2_glovars)
  assign("dbW_version", "3.2.0", envir = rSW2_glovars)
  assign("default_blob_compression_type", "gzip", envir = rSW2_glovars)
  assign("blob_compression_type", NULL, envir = rSW2_glovars)
  assign("tol", sqrt(.Machine$double.eps), envir = rSW2_glovars)
  assign("st_mo", seq_len(12L), envir = rSW2_glovars)

  # Print SOILWAT2 messages to the console (by default); may turn off later
  sw_verbosity(TRUE)

  invisible()
}


.onUnload <- function(libpath) {
  #--- Clean up C code
  library.dynam.unload("rSOILWAT2", libpath)

  invisible()
}
