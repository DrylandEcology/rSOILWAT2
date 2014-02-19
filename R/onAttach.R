.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat31")
    packageStartupMessage("Package 'Rsoilwat_31', ", meta$Version, " (", meta$Date, ").")
	print("Added generic get/set function for forb monthly biomass value")
	print("new Scaling Parameters added in weatherSetup for scaling Cloud.in variables (sky,wind,rH,trans).")
  }
  invisible()
}
