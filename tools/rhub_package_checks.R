path <- getwd()

if (grepl("Rsoilwat", path) && requireNamespace("rhub")) {
  temp <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  i <- which(temp == "Rsoilwat")
  path_rsoilwat <- paste(temp[seq_len(i)], collapse = .Platform$file.sep)

  setwd(path_rsoilwat)

  rhub::check_on_linux("Rsoilwat")
  rhub::check_on_windows("Rsoilwat")
  rhub::check_with_sanitizers("Rsoilwat")

  setwd(path)

} else {
  stop("The source package 'Rsoilwat' cannot be found in the current path")
}
