#' Peek Fathom Schema
#'
#' The function peeks into Innovasea's Fathom csv schema and returns a named list
#' containing example dataframes expected from parse_fathom_files().
#'
#'
#' @param directory_path path to the folder on your local machine containing the Fathom
#' interleaved .csv files
#'
#' @param output defines the output type "list" or "env"
#'
#' @param envir defines the environment to which dataframes will be added
#'
#' @param overwrite will overwrite existing objects in the environment with the same name
#'
#' @param n_max_header_lines integer defining the number of header lines to expect
#'
#' @param example_n integer defining the number of rows to sample from each record type to populate into $report
#'
#' @param example_max_files integer defining the max number of files to peek into
#'
#' @return Returns a list of containing two elements: $data and $report
#' $data contains blank example dataframes for each record type contained within the
#' interleaved Fathom csv files in the file path.
#' $report provides a list containing elements summarising the variables within each record type including example data defined by example_n
#' @examples
#'
#' \dontrun{
#' #'   # This example shows how to call the process_log_files function
#' #'   # Replace "/path/to/your/data" with a valid directory path on your system
#' #'   fathom_schema <- peek_fathom_schema('/path_to_fathom_csv_files', output = 'list")
#'
#' #'   fathom_schema <- peek_fathom_schema('/path_to_fathom_csv_files',
#' #'                                             output = 'list", example_n = 5)
#'
#' #'  names(res$data)
#' #'  str(res$data$det)
#' #'  res$data$det
#' #'  print(n=30, res$report$summary)
#'
#' #' }
#'
#'
#' @export peek_fathom_schema
#'

peek_fathom_schema <- function(directory_path,
                               output = c("list", "env"),
                               envir = .GlobalEnv,
                               overwrite = TRUE,
                               n_max_header_lines = 500,
                               example_n = 5,
                               example_max_files = 50,
                               verbose = TRUE) {

  output <- match.arg(output)
  stopifnot(dir.exists(directory_path))

  fcsv.files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
  if (!length(fcsv.files)) stop("No CSV files found in the directory.")

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

  # ---- helper: parse _DESC lines from one file (header only) ----
  parse_desc_from_file <- function(file_path, n_max_header_lines = 500) {
    lines <- readr::read_lines(file_path, n_max = n_max_header_lines)
    desc_lines <- grep("_DESC", lines, value = TRUE)
    if (!length(desc_lines)) return(list())

    out <- list()
    for (line in desc_lines) {
      descriptor <- stringr::str_extract(line, "^[^,]+")
      variables_str <- stringr::str_extract(line, "(?<=_DESC,).*$")
      variable_names <- stringr::str_split(variables_str %||% "", ",", simplify = TRUE)

      section_name <- tolower(gsub("_DESC", "", descriptor))
      variable_names <- variable_names[variable_names != "" & !is.na(variable_names)]
      out[[section_name]] <- unique(variable_names)
    }
    out
  }

  # ---- parse all files (headers only) ----
  schema_by_file <- lapply(fcsv.files, parse_desc_from_file, n_max_header_lines = n_max_header_lines)
  names(schema_by_file) <- basename(fcsv.files)

  all_sections <- sort(unique(unlist(lapply(schema_by_file, names))))
  if (!length(all_sections)) stop("No _DESC lines found in the CSV headers.")

  schema_union <- setNames(vector("list", length(all_sections)), all_sections)
  for (sec in all_sections) {
    cols <- unlist(lapply(schema_by_file, function(x) x[[sec]]), use.names = FALSE)
    cols <- cols[!is.na(cols) & cols != ""]
    schema_union[[sec]] <- make.unique(unique(cols))
  }

  infer_type <- function(x_chr) {
    x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
    if (!length(x_chr)) return("character")

    lx <- tolower(x_chr)
    if (all(lx %in% c("true", "false", "t", "f", "0", "1"))) return("logical")

    dt <- suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
    if (sum(!is.na(dt)) >= max(1, floor(0.8 * length(x_chr)))) return("POSIXct")

    suppressWarnings(n <- as.numeric(x_chr))
    if (all(!is.na(n))) {
      if (all(abs(n - round(n)) < .Machine$double.eps^0.5)) return("integer")
      return("double")
    }

    "character"
  }

  cast_vector <- function(x_chr, type) {
    if (type == "logical") {
      lx <- tolower(x_chr)
      out <- dplyr::case_when(
        is.na(x_chr) | x_chr == "" ~ NA,
        lx %in% c("true", "t", "1") ~ TRUE,
        lx %in% c("false", "f", "0") ~ FALSE,
        TRUE ~ NA
      )
      return(as.logical(out))
    }
    if (type == "POSIXct") {
      return(suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")))
    }
    if (type == "integer") {
      out <- suppressWarnings(as.integer(as.numeric(x_chr)))
      out[x_chr == ""] <- NA_integer_
      return(out)
    }
    if (type == "double") {
      out <- suppressWarnings(as.numeric(x_chr))
      out[x_chr == ""] <- NA_real_
      return(out)
    }
    x_chr[x_chr == ""] <- NA_character_
    as.character(x_chr)
  }

  if (!is.numeric(example_n) || example_n < 0) example_n <- 0

  sampled_rows <- setNames(vector("list", length(all_sections)), all_sections)
  n_col_width <- max(vapply(schema_union, length, integer(1)), 1)

  if (example_n > 0) {
    remaining <- setNames(rep(example_n, length(all_sections)), all_sections)
    files_to_scan <- head(fcsv.files, example_max_files)

    for (fp in files_to_scan) {
      if (all(remaining <= 0)) break

      dat <- utils::read.table(fp,
                               sep = ",",
                               col.names = paste("X", 1:max(100, n_col_width + 1)),
                               colClasses = rep("character", max(100, n_col_width + 1)),
                               fill = TRUE, skip = 2, quote = "", comment.char = "")

      if (!nrow(dat)) next

      dat$section_name <- tolower(dat$X.1)
      dat$X.1 <- NULL

      for (sec in all_sections) {
        if (remaining[[sec]] <= 0) next

        sec_rows <- dat[dat$section_name == sec, , drop = FALSE]
        if (!nrow(sec_rows)) next

        sec_rows$section_name <- NULL
        take <- min(nrow(sec_rows), remaining[[sec]])
        sec_rows <- sec_rows[seq_len(take), , drop = FALSE]

        sampled_rows[[sec]] <- dplyr::bind_rows(sampled_rows[[sec]], sec_rows)
        remaining[[sec]] <- remaining[[sec]] - take
      }
    }
  }

  data_frames <- lapply(all_sections, function(sec) {
    cols <- schema_union[[sec]]
    raw <- sampled_rows[[sec]]

    if (is.null(raw) || !nrow(raw)) {
      df_chr <- tibble::tibble(!!!stats::setNames(rep(list(character(0)), length(cols)), cols))
      attr(df_chr, "inferred_types") <- stats::setNames(rep("character", length(cols)), cols)
      return(df_chr)
    }

    raw <- raw[, seq_len(min(ncol(raw), length(cols))), drop = FALSE]
    names(raw) <- cols[seq_len(ncol(raw))]
    df_chr <- tibble::as_tibble(raw)

    inferred <- vapply(cols, function(v) {
      if (!v %in% names(df_chr)) return("character")
      infer_type(df_chr[[v]])
    }, character(1))
    inferred <- stats::setNames(inferred, cols)

    for (v in cols) if (!v %in% names(df_chr)) df_chr[[v]] <- NA_character_
    df_chr <- df_chr[, cols, drop = FALSE]

    df_typed <- df_chr
    for (v in cols) df_typed[[v]] <- cast_vector(df_chr[[v]], inferred[[v]])

    attr(df_typed, "inferred_types") <- inferred
    df_typed
  })
  names(data_frames) <- all_sections

  file_names <- names(schema_by_file)

  files_present_n <- vapply(all_sections, function(sec) {
    sum(vapply(schema_by_file, function(x) sec %in% names(x), logical(1)))
  }, integer(1))

  summary_tbl <- tibble::tibble(
    record_type = all_sections,
    n_files_present = files_present_n,
    n_files_total = length(file_names),
    n_columns = vapply(schema_union, length, integer(1)),
    n_example_rows = vapply(data_frames, nrow, integer(1))
  ) %>%
    dplyr::arrange(dplyr::desc(.data$n_files_present), .data$record_type)

  report <- list(
    directory_path = directory_path,
    n_files = length(file_names),
    n_record_types = length(all_sections),
    summary = summary_tbl
  )

  # define print format
  if (!exists("print.fathom_schema_report", mode = "function", inherits = TRUE)) {
    print.fathom_schema_report <- function(x, ...) {
      cat("\nFathom schema peek (high level)\n")
      cat("Directory:", x$directory_path, "\n")
      cat("Files:", x$n_files, "\n")
      cat("Record types:", x$n_record_types, "\n\n")
      print(x$summary)
      invisible(x)
    }
    environment(print.fathom_schema_report) <- .GlobalEnv
    assign("print.fathom_schema_report", print.fathom_schema_report, envir = .GlobalEnv)
  }

  class(report) <- c("fathom_schema_report", class(report))
  out <- list(data = data_frames, report = report)

  if (output == "env") {
    if (!overwrite) {
      existing <- names(data_frames)[names(data_frames) %in% ls(envir = envir)]
      if (length(existing)) {
        stop("Objects already exist in target environment: ",
             paste(existing, collapse = ", "),
             ". Set overwrite = TRUE to replace.")
      }
    }
    list2env(data_frames, envir = envir)
  }

  if (isTRUE(verbose)) print(report)

  if (output == "env") invisible(out) else out
}
