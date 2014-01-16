.onAttach <- function(library, pkg)
{
  if(interactive())
  {
    meta <- packageDescription("Rsoilwat")
    packageStartupMessage("Package 'Rsoilwat_27', ", meta$Version, " (", meta$Date, ").")
  }
  invisible()
}