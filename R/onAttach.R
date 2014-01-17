.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat")
    packageStartupMessage("Package 'Rsoilwat', ", meta$Version, " (", meta$Date, ").")
  }
  invisible()
}