#' Parse Fathom split csv Files
#'
#' The function parses Innovasea's Fathom csv schema log files which have been
#' split into individual csv files based on record type
#' Fathom receiver log files (.vrl, .vdat) can be
#' converted to user readable formats using FathomR::convert_log_files() or
#' using Innovasea's Fathom Connect desktop application or cloud based service
#' Fathom Central. Note: Export formats from Fathom Central are restricted to
#' interleaved .csv.
#'
#' The *Date Time (UTC)* time variable is referenced to UTC time zone and is
#' not adjusted for clock drift. *Time* variable is equivalent to the *Date
#' Time (UTC)* following correction for clock drift using the *Time Correction
#' (s)* variable.
#'
#' @param path path to the parent folder containing subfolders for parsing
#'
#' @param record_type string describing the record type to be parsed
#'
#' @return Returns a dataframe for the selected record type
#'
#' @examples
#'
#' \dontrun{
#' #'   # This example shows how to call the process_log_files function
#' #'   # Replace "/path/to/your/data" with a valid directory path on your system
#' #'   parse_fathom_split_csv_files('/path_to_fathom_split_csv_files_parent_folder', record_type = 'DET')
#' #' }
#'
#' @export parse_fathom_split_csv_files

parse_fathom_split_csv_files <- function(parent_directory, record_type = NULL) {

  subfolders <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[grepl("csv-fathom-split", basename(subfolders))]

  df_list_by_type <- list()

  for (subfolder in subfolders) {

    csv_files <- list.files(subfolder, pattern = "\\.csv$", full.names = TRUE)

    if (length(csv_files) > 0) {

      for (file in csv_files) {
        file_record_type <- sub("\\.csv$", "", basename(file))
        if (!is.null(record_type) && file_record_type != record_type) {
          next
        }

        message(paste("Reading file:", file))

        df <- tryCatch({
          fread(file, header = TRUE, skip = 3, sep = ",", fill = TRUE, stringsAsFactors = FALSE, showProgress = FALSE)
        }, error = function(e) {
          message(paste("Error in reading file:", file))
          return(NULL)
        })

        if (is.null(df)) {
          next
        }

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
