#' Inspect Receiver Log Index
#'
#' Create an index of receiver log files by scanning a directory of exported
#' csv log files and extracting file-level metadata from each file header.
#'
#' @param directory_path Path to the folder on your local machine containing
#' receiver log csv files.
#' @param pattern Regular expression used to identify files to parse. Defaults
#' to `\\.csv$`.
#' @param assign_name Optional character string naming an object to assign in
#' `envir`.
#' @param envir Environment to receive the indexed data frame when
#' `assign_name` is supplied.
#'
#' @return Returns a data frame with one row per file and the columns
#' `file_name`, `file_path`, `model`, `serial_number`,
#' `vemco_data_log_value`, and `vdat_version`.
#'
#' @details The function reads the first record in each file to identify the
#' `VEMCO DATA LOG` metadata and then searches the remaining records for
#' fields describing receiver model and serial number. A warning is emitted if
#' more than one `vemco_data_log_value` is detected across files, which may
#' indicate that source logs should be converted with
#' `FathomR::convert_log_files()` or re-exported from Fathom Central or
#' Fathom Connect.
#'
#' @examples
#' \dontrun{
#' inspect_receiver_log_index("/path_to_receiver_log_csv_files")
#'
#' inspect_receiver_log_index(
#'   "/path_to_receiver_log_csv_files",
#'   assign_name = "receiver_log_index"
#' )
#' }
#'
#' @export
inspect_receiver_log_index <- function(directory_path,
                                    pattern = "\\.csv$",
                                    assign_name = NULL,
                                    envir = .GlobalEnv) {
  file_paths <- list.files(
    path = directory_path,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(file_paths) == 0) {
    stop("No CSV files found in the directory.")
  }

  parse_single_file <- function(file_path) {
    lines <- readLines(file_path, warn = FALSE)

    if (length(lines) == 0) {
      return(
        data.frame(
          file_name = basename(file_path),
          file_path = file_path,
          model = NA_character_,
          serial_number = NA_character_,
          vemco_data_log_value = NA_character_,
          vdat_version = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }

    first_record <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
    vemco_data_log_value <- if (length(first_record) >= 2 &&
      identical(first_record[1], "VEMCO DATA LOG")) {
      first_record[2]
    } else {
      NA_character_
    }
    vdat_version <- if (length(first_record) >= 3 &&
      identical(first_record[1], "VEMCO DATA LOG")) {
      first_record[3]
    } else {
      NA_character_
    }

    desc_map <- list()
    model <- NA_character_
    serial_number <- NA_character_

    if (length(lines) >= 2) {
      for (line in lines[-1]) {
        fields <- strsplit(line, ",", fixed = TRUE)[[1]]

        if (length(fields) == 0 || !nzchar(fields[1])) {
          next
        }

        record_type <- fields[1]

        if (identical(record_type, "RECORD TYPE")) {
          next
        }

        if (grepl("_DESC$", record_type)) {
          desc_map[[sub("_DESC$", "", record_type)]] <- fields[-1]
          next
        }

        record_fields <- desc_map[[record_type]]

        if (is.null(record_fields)) {
          next
        }

        value_fields <- fields[-1]
        matched_length <- min(length(value_fields), length(record_fields))

        if (matched_length == 0) {
          next
        }

        value_fields <- value_fields[seq_len(matched_length)]
        names(value_fields) <- record_fields[seq_len(matched_length)]

        if ("Model" %in% names(value_fields)) {
          model <- unname(value_fields[["Model"]])
        }

        if ("Serial Number" %in% names(value_fields)) {
          serial_number <- unname(value_fields[["Serial Number"]])
        }

        if (!is.na(model) || !is.na(serial_number)) {
          break
        }
      }
    }

    data.frame(
      file_name = basename(file_path),
      file_path = file_path,
      model = model,
      serial_number = serial_number,
      vemco_data_log_value = vemco_data_log_value,
      vdat_version = vdat_version,
      stringsAsFactors = FALSE
    )
  }

  receiver_log_index <- do.call(
    rbind,
    lapply(file_paths, parse_single_file)
  )

  rownames(receiver_log_index) <- NULL

  unique_vemco_data_log_values <- unique(
    receiver_log_index$vemco_data_log_value[
      !is.na(receiver_log_index$vemco_data_log_value)
    ]
  )

  if (length(unique_vemco_data_log_values) > 1) {
    warning(
      paste(
        "Conflict detected in vemco_data_log_value.",
        "Please convert .vrl or .vdat using",
        "FathomR::convert_logfiles() or export from",
        "Fathom Central or Connect prior to using parse_fathom_files()"
      ),
      call. = FALSE
    )
  }

  if (!is.null(assign_name)) {
    assign(assign_name, receiver_log_index, envir = envir)
  }

  receiver_log_index
}
