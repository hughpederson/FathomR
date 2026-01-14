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
                               example_n = 0,
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

  # ---- union schema (what empty dfs will look like) ----
  schema_union <- setNames(vector("list", length(all_sections)), all_sections)
  for (sec in all_sections) {
    cols <- unlist(lapply(schema_by_file, function(x) x[[sec]]), use.names = FALSE)
    cols <- cols[!is.na(cols) & cols != ""]
    schema_union[[sec]] <- make.unique(unique(cols))
  }

  empty_frames <- lapply(schema_union, function(cols) {
    tibble::tibble(!!!stats::setNames(rep(list(character(0)), length(cols)), cols))
  })

  # ---- report object (with pretty-print) ----
  file_names <- names(schema_by_file)

  section_summary <- lapply(all_sections, function(sec) {
    present_in <- file_names[vapply(schema_by_file, function(x) sec %in% names(x), logical(1))]
    missing_in <- setdiff(file_names, present_in)

    cols_by_file <- lapply(schema_by_file, function(x) x[[sec]] %||% character(0))

    # columns differing between files (ignoring order)
    col_sig <- vapply(cols_by_file, function(v) paste(sort(unique(v)), collapse = "\r"), character(1))
    differing <- length(unique(col_sig)) > 1

    # "new columns introduced" as we iterate through files in folder order
    seen <- character(0)
    new_cols_by_file <- setNames(vector("list", length(file_names)), file_names)
    for (fn in file_names) {
      this_cols <- cols_by_file[[fn]] %||% character(0)
      new_cols <- setdiff(this_cols, seen)
      new_cols_by_file[[fn]] <- new_cols
      seen <- unique(c(seen, this_cols))
    }

    list(
      section = sec,
      files_present = present_in,
      files_missing = missing_in,
      union_columns = schema_union[[sec]],
      columns_by_file = cols_by_file,
      new_columns_by_file = new_cols_by_file,
      columns_differ_between_files = differing
    )
  })
  names(section_summary) <- all_sections

  summary_tbl <- dplyr::bind_rows(lapply(section_summary, function(x) {
    tibble::tibble(
      section = x$section,
      files_present = length(x$files_present),
      files_total = length(file_names),
      n_union_columns = length(x$union_columns),
      n_missing_files = length(x$files_missing),
      columns_differ = isTRUE(x$columns_differ_between_files)
    )
  })) %>%
    dplyr::arrange(dplyr::desc(.data$files_present), .data$section)

  report <- list(
    files = file_names,
    sections = all_sections,
    summary = summary_tbl,
    by_section = section_summary,
    directory_path = directory_path
  )

  # ---- OPTIONAL: sample example rows per section ----
  if (is.numeric(example_n) && example_n > 0) {

    infer_type <- function(x_chr) {
      x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
      if (!length(x_chr)) return("character")

      # logical?
      lx <- tolower(x_chr)
      if (all(lx %in% c("true", "false", "t", "f", "0", "1"))) return("logical")

      # integer / double?
      suppressWarnings({
        n <- as.numeric(x_chr)
      })
      if (!all(is.na(n))) {
        # if all numeric parse succeeds:
        if (all(!is.na(n))) {
          # integer-ish?
          if (all(abs(n - round(n)) < .Machine$double.eps^0.5)) return("integer")
          return("double")
        }
      }

      # datetime (common Fathom format)
      dt <- suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
      if (sum(!is.na(dt)) >= max(1, floor(0.8 * length(x_chr)))) return("POSIXct")

      "character"
    }

    # read minimal body rows for each section until we have enough
    remaining <- setNames(rep(example_n, length(all_sections)), all_sections)
    sampled_rows <- setNames(vector("list", length(all_sections)), all_sections)

    files_to_scan <- head(fcsv.files, example_max_files)

    for (fp in files_to_scan) {
      if (all(remaining <= 0)) break

      # read body only, as character
      # (skip 2 header lines like your main parser)
      dat <- utils::read.table(fp,
                               sep = ",",
                               col.names = paste("X", 1:100),
                               colClasses = rep("character", 100),
                               fill = TRUE, skip = 2, quote = "", comment.char = "")

      if (!nrow(dat)) next

      # section label in X.1, rest are fields
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

    # apply schema column names & trim to schema width
    rows_by_section <- lapply(all_sections, function(sec) {
      cols <- schema_union[[sec]]
      df <- sampled_rows[[sec]]

      if (is.null(df) || !nrow(df)) {
        # empty but with correct column names
        return(tibble::tibble(!!!stats::setNames(rep(list(character(0)), length(cols)), cols)))
      }

      # df currently has X.2..X.N as columns; keep up to schema width and rename
      df <- df[, seq_len(min(ncol(df), length(cols))), drop = FALSE]
      names(df) <- cols[seq_len(ncol(df))]
      tibble::as_tibble(df)
    })
    names(rows_by_section) <- all_sections

    # variable-level summary: suggested class + example values
    vars_summary <- dplyr::bind_rows(lapply(all_sections, function(sec) {
      df <- rows_by_section[[sec]]
      cols <- names(df)

      dplyr::bind_rows(lapply(cols, function(v) {
        x <- df[[v]]
        ex <- x[!is.na(x) & x != ""]
        tibble::tibble(
          section = sec,
          variable = v,
          suggested_class = infer_type(x),
          example = if (length(ex)) ex[[1]] else NA_character_
        )
      }))
    }))

    report$examples <- list(
      example_n = example_n,
      example_max_files = example_max_files,
      rows_by_section = rows_by_section,
      vars = vars_summary
    )
  }

  class(report) <- c("fathom_schema_report", class(report))

  out <- list(data = empty_frames, report = report)

  # ---- output mode ----
  if (output == "env") {
    if (!overwrite) {
      existing <- names(empty_frames)[names(empty_frames) %in% ls(envir = envir)]
      if (length(existing)) {
        stop("Objects already exist in target environment: ",
             paste(existing, collapse = ", "),
             ". Set overwrite = TRUE to replace.")
      }
    }
    list2env(empty_frames, envir = envir)
  }

  if (isTRUE(verbose)) print(report)

  if (output == "env") invisible(out) else out
}
