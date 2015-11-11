.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat31")
    packageStartupMessage("Package 'Rsoilwat_31', ", meta$Version, " (", meta$Date, ").")
	print("Fixed soil temperature under increased depth of constant soil temperature")
  }
  invisible()
}
