.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat31")
    packageStartupMessage("Package 'Rsoilwat_31', ", meta$Version, " (", meta$Date, ").")
	print("Added generic get/set function for forb monthly biomass value")
  }
  invisible()
}
