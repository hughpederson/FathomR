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
#' @param rec_type defines the record type(s) to be extracted from the csv files
#'
#' @param output defines the output type "list" or "env"
#'
#' @param envir defines the environment to which dataframes will be added
#'
#' @param overwrite will overwrite existing objects in the environment with the same name
#'
#' @param as_tibble outputs will be type tibble (default), otherwise type data.frame
#'
#' @return Returns a list of tibbles, one for each record type contained within the
#' interleaved Fathom csv files, appending records from each receiver to the
#' appropriate tibble. Output option "env" will return individual tibbles for each record type direct to the environment
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

parse_fathom_files <- function(directory_path,
                                  rec_type = NULL,
                                  output = c("list", "env"),
                                  envir = .GlobalEnv,
                                  overwrite = TRUE,
                                  as_tibble = TRUE) {

  output <- match.arg(output)

  stopifnot(dir.exists(directory_path))

  fcsv.files <- sort(list.files(
    path = directory_path,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  ))
  if (!length(fcsv.files)) stop("No CSV files found in the directory.")

  normalize_rec_type <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("_desc$", "", x)
    x[nzchar(x)]
  }
  rec_type_norm <- if (is.null(rec_type)) NULL else normalize_rec_type(rec_type)

  create_schema <- function(file_path, rec_type_norm = NULL) {
    lines <- readr::read_lines(file_path)
    desc_lines <- grep("_DESC", lines, value = TRUE)

    if (!is.null(rec_type_norm)) {
      keep <- vapply(desc_lines, function(line) {
        descriptor <- stringr::str_extract(line, "^[^,]+")
        section_name <- tolower(gsub("_DESC", "", descriptor))
        section_name %in% rec_type_norm
      }, logical(1))
      desc_lines <- desc_lines[keep]
      if (!length(desc_lines)) {
        stop("Requested rec_type not found in _DESC lines of: ", basename(file_path))
      }
    }

    schema <- list()
    for (line in desc_lines) {
      descriptor <- stringr::str_extract(line, "^[^,]+")
      variables_str <- stringr::str_extract(line, "(?<=_DESC,).*$")
      variable_names <- stringr::str_split(variables_str, ",", simplify = TRUE)

      section_name <- tolower(gsub("_DESC", "", descriptor))
      variable_names <- variable_names[variable_names != "" & !is.na(variable_names)]
      variable_names <- make.unique(variable_names)

      schema[[section_name]] <- list(
        colnames = variable_names,
        ncols = length(variable_names)
      )
    }
    schema
  }

  schema <- create_schema(fcsv.files[1], rec_type_norm = rec_type_norm)
  sections <- names(schema)

  fread_fathom <- function(fp, select_cols = NULL) {
    data.table::fread(
      fp,
      sep = ",",
      header = FALSE,
      fill = TRUE,
      skip = 2,
      colClasses = "character",
      select = select_cols,
      showProgress = FALSE
    )
  }

  acc <- setNames(vector("list", length(sections)), sections)
  for (s in sections) acc[[s]] <- list()

  pb <- progress::progress_bar$new(
    format = "[:bar] :percent Parsing :current/:total files, eta: :eta",
    total = length(fcsv.files), clear = FALSE, width = 60
  )

  if (!is.null(rec_type_norm) && length(rec_type_norm) == 1) {

    s <- rec_type_norm[[1]]
    if (!s %in% sections) stop("Requested rec_type '", s, "' not present in schema from _DESC.")

    ncols <- schema[[s]]$ncols
    select_cols <- 1:(ncols + 1)

    for (fp in fcsv.files) {
      pb$tick()

      dt <- fread_fathom(fp, select_cols = select_cols)
      if (!nrow(dt)) next

      dt <- dt[tolower(V1) == s]
      if (!nrow(dt)) next

      dt[, V1 := NULL]

      cur <- ncol(dt)
      if (cur < ncols) {
        for (j in (cur + 1):ncols) data.table::set(dt, j = j, value = NA_character_)
      } else if (cur > ncols) {
        dt <- dt[, seq_len(ncols), with = FALSE]
      }

      data.table::setnames(dt, schema[[s]]$colnames)
      acc[[s]][[length(acc[[s]]) + 1]] <- dt
    }

  } else {
    max_ncols <- max(vapply(schema, `[[`, numeric(1), "ncols"))
    select_cols <- 1:(max_ncols + 1)

    for (fp in fcsv.files) {
      pb$tick()

      dt <- fread_fathom(fp, select_cols = select_cols)
      if (!nrow(dt)) next

      dt[, section := tolower(V1)]
      if (!is.null(rec_type_norm)) {
        dt <- dt[section %in% rec_type_norm]
      }
      dt <- dt[section %in% sections]
      if (!nrow(dt)) next

      dt[, V1 := NULL]

      spl <- split(dt, by = "section", keep.by = FALSE)

      for (s in names(spl)) {
        piece <- spl[[s]]
        ncols <- schema[[s]]$ncols

        cur <- ncol(piece)
        if (cur < ncols) {
          for (j in (cur + 1):ncols) data.table::set(piece, j = j, value = NA_character_)
        } else if (cur > ncols) {
          piece <- piece[, seq_len(ncols), with = FALSE]
        }

        data.table::setnames(piece, schema[[s]]$colnames)
        acc[[s]][[length(acc[[s]]) + 1]] <- piece
      }
    }
  }

  out <- lapply(sections, function(s) {
    pieces <- acc[[s]]
    if (!length(pieces)) {
      data.table::as.data.table(setNames(vector("list", schema[[s]]$ncols), schema[[s]]$colnames))
    } else {
      data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    }
  })
  names(out) <- sections

  for (s in names(out)) {
    dt <- out[[s]]

    if ("Time" %in% names(dt)) {
      dt[, Time := fasttime::fastPOSIXct(Time, tz = "UTC")]
    }

    if ("Serial Number" %in% names(dt)) {
      dt[, Serial := suppressWarnings(as.integer(`Serial Number`))]
      dt[, `Serial Number` := NULL]
    }

    out[[s]] <- dt
  }

  if (isTRUE(as_tibble)) {
    out <- lapply(out, tibble::as_tibble)
  }

  if (output == "env") {
    if (!overwrite) {
      existing <- names(out)[names(out) %in% ls(envir = envir)]
      if (length(existing)) {
        stop("Objects already exist in target environment: ",
             paste(existing, collapse = ", "),
             ". Set overwrite = TRUE to replace.")
      }
    }
    list2env(out, envir = envir)
    invisible(out)
  } else {
    out
  }
}


