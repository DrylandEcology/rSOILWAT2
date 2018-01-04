###############################################################################
#rSOILWAT2
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


.onAttach <- function(library, pkgname) {
  if (interactive())  {
    meta <- utils::packageDescription(pkgname)
    packageStartupMessage("Package ", shQuote(pkgname), " v", meta$Version, " (",
      meta$Date,") attached/loaded.")
    packageStartupMessage("Daily weather database version ", rSW2_glovars$dbW_version)
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


  invisible()
}


.onUnload <- function(libpath) {
  #--- Clean up C code
  library.dynam.unload("rSOILWAT2", libpath)

  invisible()
}
