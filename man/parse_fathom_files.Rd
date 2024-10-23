\name{parse_fathom_files}
\alias{parse_fathom_files}
\title{Parse Fathom Files}
\usage{
parse_fathom_files('path_to_log_files')
}
\arguments{
  \item{path}{ path to the folder on your local machine containing the Fathom exported log files (.csv)

  }
}

\description{
The function parses Innovasea's Fathom csv schema log files and returns a dataframe for record type.
}
\Details{
The *Date Time (UTC)* time variable is referenced to UTC time zone, *Time*
}
\examples{
\dontrun{
#'   # This example shows how to call the process_log_files function
#'   # Replace "/path/to/your/data" with a valid directory path on your system
#'   process_log_files("/path/to/your/data")
#' }

}
