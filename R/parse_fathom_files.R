#' Parse Fathom Files
#' 
#' The function parses Innovasea's Fathom csv schema log files and returns a
#' dataframe for record type. Fathom receiver log files (.vrl, .vdat) can be
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
#' @return Returns a dataframe for each record type contained within the
#' interleaved Fathom csv files, appending records from each source to the
#' appropriate dataframes.
#' @examples
#' 
#' \dontrun{
#' #'   # This example shows how to call the process_log_files function
#' #'   # Replace "/path/to/your/data" with a valid directory path on your system
#' #'   parse_fathom_files('/path_to_fathom_csv_files')
#' #' }
#' 
#' 
#' @export parse_fathom_files
parse_fathom_files <- function(directory_path) {
  Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

  file_path <- dir(directory_path, full.names = TRUE, pattern = "\\.csv$")

  create_empty_dataframes <- function(file_path) {
    lines <- read_lines(file_path)
    desc_lines <- grep("_DESC", lines, value = TRUE)

    data_frames <- list()

    for (line in desc_lines) {
      descriptor <- str_extract(line, "^[^,]+")
      variables_str <- str_extract(line, "(?<=_DESC,).*$")
      variable_names <- str_split(variables_str, ",", simplify = TRUE)

      section_name <- tolower(gsub("_DESC", "", descriptor))
      variable_names <- variable_names[variable_names != "" & !is.na(variable_names)]
      variable_names <- make.unique(variable_names)

      data_frames[[section_name]] <- tibble(!!!setNames(rep(list(character(0)), length(variable_names)), variable_names))
    }

    return(data_frames)
  }

  if (length(file_path) > 0) {
    data_frames <- create_empty_dataframes(file_path[1])
  } else {
    stop("No CSV files found in the directory.")
  }

  fcsv.files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

  pb <- progress_bar$new(
    format = "[:bar] :percent Reading :current/:total files, eta: :eta",
    total = length(fcsv.files),
    clear = FALSE,
    width = 60
  )

  fcsv.list <- lapply(fcsv.files, function(fcsv.files) {
    pb$tick()

    read.table(fcsv.files, sep = ",", col.names = paste("X", 1:100),
               colClasses = rep("character", 100),
               fill = TRUE, skip = 2)
  })

  all_data <- rbindlist(fcsv.list)

  grouped_data <- all_data %>%
    mutate(section_name = tolower(X.1)) %>%
    select(-X.1) %>%
    group_by(section_name)

  unique_section_names <- group_keys(grouped_data)$section_name
  data_by_section_with_names <- group_split(grouped_data)
  names(data_by_section_with_names) <- unique_section_names

  common_sections <- intersect(names(data_frames), names(data_by_section_with_names))

  data_frames_updated <- map2(
    data_frames[common_sections],
    data_by_section_with_names[common_sections],
    ~{
      n_cols_x <- ncol(.x)
      .y_adjusted <- .y %>%
        select(seq_len(n_cols_x))
      colnames(.y_adjusted) <- colnames(.x)
      bind_rows(.y_adjusted, .x)
    },
    .id = "section_name"
  )

  data_frames_updated2 <- lapply(data_frames_updated, function(df) {
    if ("Time" %in% names(df) && "Serial Number" %in% names(df)) {
      df %>%
        mutate(
          Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
          Serial = as.integer(`Serial Number`)
        ) %>%
        select(-`Serial Number`)
    } else {
      df
    }
  })

  list2env(data_frames_updated2, envir = .GlobalEnv)
  rm(data_by_section_with_names, data_frames, data_frames_updated, all_data, grouped_data, fcsv.list)
}


