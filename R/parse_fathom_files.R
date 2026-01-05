#' Parse Fathom Files
#'
#' The function parses Innovasea's Fathom csv schema log files and returns a named list
#' of dataframes, one for each record type. Fathom receiver log files (.vrl, .vdat) can be
#' converted to user readable formats using FathomR::convert_log_files() or
#' using Innovasea's Fathom Connect desktop application or cloud based service
#' Fathom Central. Note: Export formats from Fathom Central are restricted
#' interleaved .csv.
#'
#' The *Date Time (UTC)* time variable is referenced to UTC time zone and is
#' not adjusted for clock drift. *Time* variable is equivalent to the *Date
#' Time (UTC)* following correction for clock drift using the *Time Correction
#' (s)* variable.
#'
#' @param path path to the folder on your local machine containing the Fathom
#' interleaved .csv files
#'
#' @param output defines the output type "list" or "env"
#'
#' @param envir defines the environment to which dataframes will be added
#' 
#' @param vars defines which Fathom variables to open. Options include: attitude, battery, cfg_channel, cfg_station,
#'  cfg_study, cfg_transmitter, clock_ref, data_source_file, depth, det, diag, event, event_init, event_offload, health_vr2ar, temp. All 
#' variables are loaded by default.   
#' 
#' @param overwrite will overwrite existing objects in the environment with the same name
#'
#' @return Returns a list of dataframes, one for each record type contained within the
#' interleaved Fathom csv files, appending records from each receiver to the
#' appropriate dataframes. Output option "env" will return individual dataframes for each record type direct to the environment
#' @examples
#'
#' \dontrun{
#' #'   # This example shows how to call the process_log_files function
#' #'   # Replace "/path/to/your/data" with a valid directory path on your system
#' #'   results <- parse_fathom_files('/path_to_fathom_csv_files')
#' #'   or
#' #'   parse_fathom_files('/path_to_fathom_csv_files', output ='env")
#' #' }
#'
#'
#' @export parse_fathom_files
# parse_fathom_files <- function(directory_path) {
#   Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)
#
#   file_path <- dir(directory_path, full.names = TRUE, pattern = "\\.csv$")
#
#   create_empty_dataframes <- function(file_path) {
#     lines <- read_lines(file_path)
#     desc_lines <- grep("_DESC", lines, value = TRUE)
#
#     data_frames <- list()
#
#     for (line in desc_lines) {
#       descriptor <- str_extract(line, "^[^,]+")
#       variables_str <- str_extract(line, "(?<=_DESC,).*$")
#       variable_names <- str_split(variables_str, ",", simplify = TRUE)
#
#       section_name <- tolower(gsub("_DESC", "", descriptor))
#       variable_names <- variable_names[variable_names != "" & !is.na(variable_names)]
#       variable_names <- make.unique(variable_names)
#
#       data_frames[[section_name]] <- tibble(!!!setNames(rep(list(character(0)), length(variable_names)), variable_names))
#     }
#
#     return(data_frames)
#   }
#
#   if (length(file_path) > 0) {
#     data_frames <- create_empty_dataframes(file_path[1])
#   } else {
#     stop("No CSV files found in the directory.")
#   }
#
#   fcsv.files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
#
#   pb <- progress_bar$new(
#     format = "[:bar] :percent Reading :current/:total files, eta: :eta",
#     total = length(fcsv.files),
#     clear = FALSE,
#     width = 60
#   )
#
#   fcsv.list <- lapply(fcsv.files, function(fcsv.files) {
#     pb$tick()
#
#     read.table(fcsv.files, sep = ",", col.names = paste("X", 1:100),
#                colClasses = rep("character", 100),
#                fill = TRUE, skip = 2)
#   })
#
#   all_data <- rbindlist(fcsv.list)
#
#   grouped_data <- all_data %>%
#     mutate(section_name = tolower(X.1)) %>%
#     select(-X.1) %>%
#     group_by(section_name)
#
#   unique_section_names <- group_keys(grouped_data)$section_name
#   data_by_section_with_names <- group_split(grouped_data)
#   names(data_by_section_with_names) <- unique_section_names
#
#   common_sections <- intersect(names(data_frames), names(data_by_section_with_names))
#
#   data_frames_updated <- map2(
#     data_frames[common_sections],
#     data_by_section_with_names[common_sections],
#     ~{
#       n_cols_x <- ncol(.x)
#       .y_adjusted <- .y %>%
#         select(seq_len(n_cols_x))
#       colnames(.y_adjusted) <- colnames(.x)
#       bind_rows(.y_adjusted, .x)
#     },
#     .id = "section_name"
#   )
#
#   data_frames_updated2 <- lapply(data_frames_updated, function(df) {
#     if ("Time" %in% names(df) && "Serial Number" %in% names(df)) {
#       df %>%
#         mutate(
#           Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
#           Serial = as.integer(`Serial Number`)
#         ) %>%
#         select(-`Serial Number`)
#     } else {
#       df
#     }
#   })
#
#   list2env(data_frames_updated2, envir = .GlobalEnv)
#   rm(data_by_section_with_names, data_frames, data_frames_updated, all_data, grouped_data, fcsv.list)
# }

parse_fathom_files <- function(directory_path,
                               output = c("list", "env"),
                               envir = .GlobalEnv,
                               vars = "All",
                               overwrite = TRUE) {
  output <- match.arg(output)
  Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

  # ---- helpers & checks ----
  stopifnot(dir.exists(directory_path))
  fcsv.files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
  if (!length(fcsv.files)) stop("No CSV files found in the directory.")

  create_empty_dataframes <- function(file_path) {
    lines <- readr::read_lines(file_path,
      progress = FALSE)
    desc_lines <- grep("_DESC", lines, value = TRUE)

    data_frames <- list()
    for (line in desc_lines) {
      descriptor <- stringr::str_extract(line, "^[^,]+")
      variables_str <- stringr::str_extract(line, "(?<=_DESC,).*$")
      variable_names <- stringr::str_split(variables_str, ",", simplify = TRUE)

      section_name <- tolower(gsub("_DESC", "", descriptor))
      variable_names <- variable_names[variable_names != "" & !is.na(variable_names)]
      variable_names <- make.unique(variable_names)

      # create a tibble with zero rows and the right columns
      data_frames[[section_name]] <- tibble::tibble(
        !!!stats::setNames(rep(list(character(0)), length(variable_names)), variable_names)
      )
    }
    data_frames
  }

  # seed empty frames using first csv
  data_frames <- create_empty_dataframes(file_path = fcsv.files[1])

  # ---- read all csvs (skip first 2 header lines) ----
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent Reading :current/:total files, eta: :eta",
    total = length(fcsv.files), clear = FALSE, width = 60
  )

  fcsv.list <- lapply(fcsv.files, function(fp) {
    pb$tick()
    utils::read.table(fp,
                      sep = ",",
                      col.names = paste("X", 1:100),  # gives "X 1", "X 2", ... -> becomes X.1, X.2
                      colClasses = rep("character", 100),
                      fill = TRUE, skip = 2, quote = "", comment.char = "")
  })

  all_data <- data.table::rbindlist(fcsv.list, fill = TRUE)

  # ---- split by section ----
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })

  grouped_data <- all_data %>%
    mutate(section_name = tolower(X.1)) %>%
    select(-.data$X.1) %>%
    group_by(.data$section_name)

  unique_section_names <- dplyr::group_keys(grouped_data)$section_name
  data_by_section_with_names <- dplyr::group_split(grouped_data)
  names(data_by_section_with_names) <- unique_section_names

  # keep only sections we know about (from _DESC)

  if (length(vars) == 1) {
    if (vars == "All") {
      common_sections <- intersect(names(data_frames), names(data_by_section_with_names))
    } else {
      common_sections <- vars  
    }
  } else {
    common_sections <- vars
  }

  data_frames_updated <- purrr::map2(
    data_frames[common_sections],
    data_by_section_with_names[common_sections],
    ~{
      n_cols_x <- ncol(.x)
      .y_adjusted <- .y %>% dplyr::select(seq_len(n_cols_x))
      colnames(.y_adjusted) <- colnames(.x)
      dplyr::bind_rows(.y_adjusted, .x)
    }
  )

  # ---- light typing / renames ----
  data_frames_updated2 <- lapply(data_frames_updated, function(df) {
    if ("Time" %in% names(df) && "Serial Number" %in% names(df)) {
      df %>%
        mutate(
          Time = as.POSIXct(.data$Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
          Serial = suppressWarnings(as.integer(`Serial Number`))
        ) %>%
        select(-`Serial Number`)
    } else df
  })

  # ---- output mode ----
  if (output == "env") {
    # check overwrites if requested
    if (!overwrite) {
      existing <- names(data_frames_updated2)[names(data_frames_updated2) %in% ls(envir = envir)]
      if (length(existing)) {
        stop("Objects already exist in target environment: ",
             paste(existing, collapse = ", "),
             ". Set overwrite = TRUE to replace.")
      }
    }
    list2env(data_frames_updated2, envir = envir)
    invisible(data_frames_updated2)  # still return (invisibly) for piping/tests
  } else {
    # default: return a named list of data frames
    data_frames_updated2
  }
}


