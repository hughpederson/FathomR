parse_fathom_files_vroom <- function(directory_path) {

  # Define paths to data files
  file_path <- dir(directory_path, pattern = "*.csv", full.names = TRUE)

  if (length(file_path) == 0) {
    stop("No CSV files found in the specified directory.")
  }

  # Function to create empty data frames based on DESC lines
  create_empty_dataframes <- function(file_path) {
    # Check if file exists
    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }

    lines <- readLines(file_path)
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

  data_frames <- create_empty_dataframes(file_path[1]) # Only needs the first file

  # Read all CSV files in the directory
  fcsv.files <- list.files(path = directory_path, pattern = ".csv", full.names = TRUE)

  # Check if there are CSV files to process
  if (length(fcsv.files) == 0) {
    stop("No CSV files found in the directory.")
  }

  # Create a progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent Reading :current/:total files, eta: :eta",
    total = length(fcsv.files),
    clear = FALSE,
    width = 60
  )

  # Use vroom to read CSV files efficiently
  fcsv.list <- lapply(fcsv.files, function(file) {
    if (!file.exists(file)) {
      stop("File does not exist: ", file)
    }

    # Update the progress bar
    pb$tick()

    # Print file name for debugging
    message("Reading file: ", file)

    vroom(file, delim = ",", col_types = cols(.default = "c"), skip = 2)
  })

  all_data <- bind_rows(fcsv.list)

  # Process the data based on the first column being a section name (update to actual column names if different)
  grouped_data <- all_data %>%
    mutate(section_name = tolower(all_data[[1]])) %>%  # Use actual column name instead of X1
    select(-1) %>%  # Remove the first column, which is now in section_name
    group_by(section_name)

  unique_section_names <- group_keys(grouped_data)$section_name
  data_by_section_with_names <- group_split(grouped_data)
  names(data_by_section_with_names) <- unique_section_names

  common_sections <- intersect(names(data_frames), names(data_by_section_with_names))

  # Ensure that we only select columns that exist in both data frames
  data_frames_updated <- map2(
    data_frames[common_sections],
    data_by_section_with_names[common_sections],
    ~{
      n_cols_x <- ncol(.x)
      n_cols_y <- ncol(.y)

      # Only select up to the minimum number of columns
      n_min_cols <- min(n_cols_x, n_cols_y)

      .y_adjusted <- .y %>%
        select(seq_len(n_min_cols))  # Select only up to the available columns
      colnames(.y_adjusted) <- colnames(.x)[seq_len(n_min_cols)]  # Adjust column names to match

      bind_rows(.y_adjusted, .x)  # Combine rows
    }
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

  # Save the data frames to the global environment
  list2env(data_frames_updated2, envir = .GlobalEnv)
  rm(data_by_section_with_names, data_frames, data_frames_updated, all_data, grouped_data, fcsv.list)
}
