
convert_log_files <- function(source_directory, executable_directory, output_format = "csv.fathom") {

  vdat.exe.loc <- shQuote(file.path(executable_directory, "vdat"))

  log.files <- list.files(source_directory, pattern = "\\.(vrl|vdat)$", full.names = TRUE)

  log.list <- lapply(log.files, function(log.file) {
    if (file.exists(log.file)) {
      abs_path <- normalizePath(log.file, mustWork = FALSE)
      print(paste("Processing file:", abs_path))
      command <- paste(vdat.exe.loc, "convert --format=", output_format, "--timec=default", shQuote(abs_path))
      print(paste("Executing command:", command))
      result <- system(command, intern = TRUE)
      print(result)
      return(list(file = log.file, command = command, result = result))
    } else {
      warning(paste("File not found:", log.file))
      return(list(file = log.file, command = NULL, result = "File not found"))
    }
  })

  return(log.list)
}



