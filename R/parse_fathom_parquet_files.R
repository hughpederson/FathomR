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


