\name{convert_log_files}
\alias{convert_log_files}
\title{Convert Log Files}
\usage{
convert_log_files('path_to_log_files', 'path_to_vdat_exe')
}
\arguments{
  \item{source_directory}{ Path to the folder on your local machine containing the Innovasea receiver log files (.vdat, .vrl)}
  \item{executable_directory}{Path to vdat.exe on your local machine}
  \item{output_format}{String defining the output format of converted logfiles. Valid values are c('csv.fathom', 'csv.fathom.split', 'parquet.fathom')}
}

\description{
The function parses Innovasea receiver log files and returns a Fathom formatted csv file for each receiver logfile.
}
\Details{
The *Date Time (UTC)* time variable is referenced to UTC time zone, *Time*
}
\examples{
\dontrun{
#'   # This example shows how to call the convert_log_files function
#'   # Replace "/path/to/your/data" with a valid directory path on your system
#'   process_log_files("/path/to/your/data")
#' }

}
