.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat")
    packageStartupMessage("Package 'Rsoilwat_31', ", meta$Version, " (", meta$Date, ").")
  }
  invisible()
}