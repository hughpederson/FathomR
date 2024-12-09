#' Parse Fathom Parquet Files
#'
#' The function parses Innovasea's Fathom parquet schema log files which have been
#' split into individual parquet files based on record type
#' Fathom receiver log files (.vrl, .vdat) can be
#' converted to user readable formats using FathomR::convert_log_files() or
#' using Innovasea's Fathom Connect desktop application or cloud based service
#' Fathom Central. Note: Export formats from Fathom Central are restricted to
#' interleaved .csv.
#'
#' @param path path to the parent folder containing subfolders for parsing
#'
#' @param record_type string describing the record type to parse
#'
#' @return Returns a dataframe for the selected record types
#'
#' @examples
#'
#' \dontrun{
#' #'   # This example shows how to call the process_log_files function
#' #'   # Replace "/path/to/your/data" with a valid directory path on your system
#' #'   parse_fathom_parquet_files('/path_to_fathom_parquet_files_parent_folder', record_type = 'DET')
#' #' }
#'
#' @export parse_fathom_parquet_files
parse_fathom_parquet_files <- function(parent_directory, record_type = NULL) {

  subfolders <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[grepl("parquet", basename(subfolders))]

  df_list_by_type <- list()

  for (subfolder in subfolders) {

    parquet_files <- list.files(subfolder, pattern = "\\.parquet$", full.names = TRUE)

    if (length(parquet_files) > 0) {
      for (file in parquet_files) {
        file_record_type <- sub("\\.parquet$", "", basename(file))

        if (!is.null(record_type) && file_record_type != record_type) {
          next
        }

        message(paste("Reading file:", file))

        df <- read_parquet(file)

        if (file_record_type %in% names(df_list_by_type)) {
          df_list_by_type[[file_record_type]] <- bind_rows(df_list_by_type[[file_record_type]], df)
        } else {
          df_list_by_type[[file_record_type]] <- df
        }
      }
    }
  }

  if (!is.null(record_type)) {
    return(df_list_by_type[[record_type]])
  }
  return(df_list_by_type)
}


