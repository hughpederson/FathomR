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


