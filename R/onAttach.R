.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat31")
    packageStartupMessage("Package 'Rsoilwat_31', ", meta$Version, " (", meta$Date, ").")
	print("Fixed soil temperature bug (strong dampening with depth): SWC - VWC confusion")
	print("NOTE: soil thawing/freezing turned off due to large number of errors - NEEDS FIX")
  }
  invisible()
}
