path <- getwd()

if (grepl("rSOILWAT2", path) && requireNamespace("rhub")) {
  temp <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  i <- which(temp == "rSOILWAT2")
  path_rsoilwat <- paste(temp[seq_len(i)], collapse = .Platform$file.sep)

  setwd(path_rsoilwat)

  rhub::check_on_linux("rSOILWAT2")
  rhub::check_on_windows("rSOILWAT2")
  rhub::check_with_sanitizers("rSOILWAT2")

  setwd(path)

} else {
  stop(
    "The source package 'rSOILWAT2' cannot be found in the current path",
    call. = FALSE
  )
}
