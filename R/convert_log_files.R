#' Convert Log Files
#' 
#' The function parses Innovasea receiver log files and returns a Fathom
#' formatted csv file for each receiver logfile.
#' 
#' 
#' @param source_directory Path to the folder on your local machine containing
#' the Innovasea receiver log files (.vdat, .vrl)
#' @param executable_directory Path to vdat.exe on your local machine
#' @param output_format String defining the output format of converted
#' logfiles. Valid values are c('csv.fathom', 'csv.fathom.split',
#' 'parquet.fathom')
#' @examples
#' 
#' \dontrun{
#'    # This example shows how to call the convert_log_files function
#'    # Replace "/path/to/your/data" with a valid directory path on your system
#'    convert_log_files('/path_to_receiver_logfiles', 'path_to_vdat_exe', output_format = 'csv.fathom')
#'    }
#' 
#' @export convert_log_files
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



