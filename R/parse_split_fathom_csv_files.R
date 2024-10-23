parse_split_fathom_csv_files <- function(parent_directory, record_type = NULL) {

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
          fread(file, header = TRUE, skip = 2, sep = ",", fill = TRUE, stringsAsFactors = FALSE, showProgress = FALSE)
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
