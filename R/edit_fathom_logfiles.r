#' Count Detections by Full ID in Fathom Log Files
#'
#' Count `DET` records by `Full ID` across one or more Fathom csv log files,
#' with optional filtering by a time window.
#'
#' @param path Path to a Fathom csv file, a character vector of file paths, or
#'   a directory containing Fathom csv files.
#' @param minTime Optional lower bound for `DET` record times. Accepts
#'   `NULL`, `Date`, `POSIXt`, or a single character string in a common
#'   date-time format.
#' @param maxTime Optional upper bound for `DET` record times. Accepts
#'   `NULL`, `Date`, `POSIXt`, or a single character string in a common
#'   date-time format.
#' @param tz Time zone used when parsing `minTime`, `maxTime`, and file record
#'   times. Defaults to `"UTC"`.
#'
#' @return A named integer vector of detection counts, sorted in descending
#'   order by count. Returns `integer()` when no matching detections are found.
#'
#' @examples
#' \dontrun{
#' fathom_logfile_detection_count("/path_to_fathom_csv_files")
#'
#' fathom_logfile_detection_count(
#'   "/path_to_fathom_csv_files",
#'   minTime = "2024-01-01 00:00:00",
#'   maxTime = "2024-01-31 23:59:59"
#' )
#' }
#'
#' @export
fathom_logfile_detection_count <- function(path, minTime = NULL, maxTime = NULL, tz = "UTC") {
  paths <- .normalize_fathom_paths(path)
  min_time <- .parse_fathom_time_value(minTime, tz = tz)
  max_time <- .parse_fathom_time_value(maxTime, tz = tz)

  if (!is.null(min_time) && !is.null(max_time) && min_time > max_time) {
    stop("minTime must be earlier than or equal to maxTime.", call. = FALSE)
  }

  det_full_ids <- unlist(lapply(paths, function(one_path) {
    lines <- readLines(one_path, warn = FALSE, encoding = "UTF-8")
    lines <- sub("\r$", "", lines)
    desc_map <- .get_fathom_desc_map(lines)
    det_time_index <- .get_fathom_field_index(desc_map, "DET", "Time")
    det_full_id_index <- .get_fathom_field_index(desc_map, "DET", "Full ID")
    det_lines <- lines[startsWith(lines, "DET,")]

    if (!length(det_lines)) {
      return(character())
    }

    keep_time <- rep(TRUE, length(det_lines))

    if (!is.null(min_time) || !is.null(max_time)) {
      det_times <- vapply(
        det_lines,
        FUN.VALUE = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
        FUN = .extract_fathom_record_time,
        field_index = det_time_index,
        tz = tz
      )

      parsed_time <- !is.na(det_times)

      if (!is.null(min_time)) {
        keep_time[parsed_time] <- keep_time[parsed_time] & det_times[parsed_time] >= min_time
      }

      if (!is.null(max_time)) {
        keep_time[parsed_time] <- keep_time[parsed_time] & det_times[parsed_time] <= max_time
      }
    }

    vapply(
      det_lines[keep_time],
      FUN.VALUE = character(1),
      FUN = .extract_fathom_field,
      field_index = det_full_id_index
    )
  }), use.names = FALSE)

  det_full_ids <- det_full_ids[!is.na(det_full_ids) & nzchar(det_full_ids)]

  if (!length(det_full_ids)) {
    return(integer())
  }

  sort(table(det_full_ids), decreasing = TRUE)
}


#' Filter Fathom Log Files by Full ID
#'
#' Write filtered copies of one or more Fathom csv log files, keeping only
#' selected `DET` records and optionally restricting records by time.
#'
#' @param path Path to a Fathom csv file, a character vector of file paths, or
#'   a directory containing Fathom csv files.
#' @param keep_full_id Character vector of `Full ID` values to retain in `DET`
#'   records.
#' @param output_path Optional output destination. Supply `NULL` to write beside
#'   each input file using an auto-generated suffix, a single directory path to
#'   write all outputs into that directory, a single file path when filtering
#'   one input file, or one output path per input file.
#' @param minTime Optional lower bound for record times. Accepts `NULL`,
#'   `Date`, `POSIXt`, or a single character string in a common date-time
#'   format.
#' @param maxTime Optional upper bound for record times. Accepts `NULL`,
#'   `Date`, `POSIXt`, or a single character string in a common date-time
#'   format.
#' @param time_filter_scope Character string controlling which record types are
#'   time-filtered when `time_filter_record_types` is `NULL`. One of `"det"`,
#'   `"det_diag"`, or `"all"`.
#' @param time_filter_record_types Optional character vector of explicit record
#'   types to time-filter. When supplied, this overrides `time_filter_scope`.
#' @param tz Time zone used when parsing `minTime`, `maxTime`, and file record
#'   times. Defaults to `"UTC"`.
#' @param show_progress Logical; if `TRUE`, display progress while processing
#'   files.
#' @param parallel Logical; if `TRUE`, process multiple input files in parallel
#'   when more than one file is supplied.
#' @param n_cores Optional number of worker processes to use when
#'   `parallel = TRUE`. Defaults to one less than the number of detected
#'   physical CPU cores.
#'
#' @return Invisibly returns a list containing the selected `keep_full_id`
#'   values, parsed time filters, processing settings, and a per-file summary
#'   data frame with input and output paths plus line counts.
#'
#' @examples
#' \dontrun{
#' edit_fathom_files(
#'   path = "/path_to_fathom_csv_files",
#'   keep_full_id = c("A69-1601-12345", "A69-1602-54321")
#' )
#'
#' edit_fathom_files(
#'   path = "/path_to_fathom_csv_files",
#'   keep_full_id = "A69-1601-12345",
#'   minTime = "2024-01-01 00:00:00",
#'   maxTime = "2024-01-31 23:59:59",
#'   time_filter_scope = c("DET","DIAG"),
#'   parallel = TRUE,
#'   n_cores = 2
#' )
#' }
#'
#' @export
edit_fathom_files <- function(
  path,
  keep_full_id,
  output_path = NULL,
  minTime = NULL,
  maxTime = NULL,
  time_filter_scope = c("DET", "DIAG"),
  time_filter_record_types = NULL,
  tz = "UTC",
  show_progress = interactive(),
  parallel = FALSE,
  n_cores = NULL
) {
  paths <- .normalize_fathom_paths(path)
  stopifnot(length(keep_full_id) >= 1, all(nzchar(keep_full_id)))
  time_filter_scope <- match.arg(time_filter_scope)
  time_filter_record_types <- .normalize_record_types(time_filter_record_types)
  min_time <- .parse_fathom_time_value(minTime, tz = tz)
  max_time <- .parse_fathom_time_value(maxTime, tz = tz)

  if (!is.null(min_time) && !is.null(max_time) && min_time > max_time) {
    stop("minTime must be earlier than or equal to maxTime.", call. = FALSE)
  }

  output_paths <- .resolve_output_paths(paths, keep_full_id, output_path)

  if (is.null(n_cores)) {
    detected_cores <- parallel::detectCores(logical = FALSE)
    if (is.na(detected_cores) || detected_cores < 1L) {
      detected_cores <- 1L
    }
    n_cores <- max(1L, detected_cores - 1L)
  }

  use_parallel <- isTRUE(parallel) && length(paths) > 1L && n_cores > 1L

  worker_args <- list(
    keep_full_id = keep_full_id,
    min_time = min_time,
    max_time = max_time,
    time_filter_scope = time_filter_scope,
    time_filter_record_types = time_filter_record_types,
    tz = tz
  )

  if (use_parallel) {
    worker_fun <- function(i, paths, output_paths, worker_args) {
      .process_fathom_file(
        one_path = paths[[i]],
        one_output_path = output_paths[[i]],
        keep_full_id = worker_args$keep_full_id,
        min_time = worker_args$min_time,
        max_time = worker_args$max_time,
        time_filter_scope = worker_args$time_filter_scope,
        time_filter_record_types = worker_args$time_filter_record_types,
        tz = worker_args$tz
      )
    }

    if (isTRUE(show_progress)) {
      message(sprintf(
        "Filtering %s file(s) in parallel using %s worker(s)...",
        length(paths),
        n_cores
      ))
    }

    if (requireNamespace("pbapply", quietly = TRUE)) {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(
        cl,
        varlist = c(
          ".write_fathom_lines",
          ".split_fathom_csv_line",
          ".get_fathom_desc_map",
          ".get_fathom_record_fields",
          ".get_fathom_field_index",
          ".extract_fathom_field",
          ".parse_fathom_time_value",
          ".extract_fathom_record_time",
          ".process_fathom_file"
        ),
        envir = environment()
      )

      file_results <- pbapply::pblapply(
        X = seq_along(paths),
        FUN = worker_fun,
        cl = cl,
        paths = paths,
        output_paths = output_paths,
        worker_args = worker_args
      )
    } else {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(
        cl,
        varlist = c(
          ".write_fathom_lines",
          ".split_fathom_csv_line",
          ".get_fathom_desc_map",
          ".get_fathom_record_fields",
          ".get_fathom_field_index",
          ".extract_fathom_field",
          ".parse_fathom_time_value",
          ".extract_fathom_record_time",
          ".process_fathom_file"
        ),
        envir = environment()
      )

      file_results <- parallel::parLapplyLB(
        cl,
        X = seq_along(paths),
        fun = worker_fun,
        paths = paths,
        output_paths = output_paths,
        worker_args = worker_args
      )
    }

    if (isTRUE(show_progress)) {
      message("Parallel filtering complete.")
    }
  } else {
    if (isTRUE(show_progress)) {
      pb <- utils::txtProgressBar(min = 0, max = length(paths), style = 3)
      on.exit(close(pb), add = TRUE)
    }

    file_results <- vector("list", length(paths))

    for (i in seq_along(paths)) {
      file_results[[i]] <- .process_fathom_file(
        one_path = paths[[i]],
        one_output_path = output_paths[[i]],
        keep_full_id = keep_full_id,
        min_time = min_time,
        max_time = max_time,
        time_filter_scope = time_filter_scope,
        time_filter_record_types = time_filter_record_types,
        tz = tz
      )

      if (isTRUE(show_progress)) {
        utils::setTxtProgressBar(pb, i)
      }
    }
  }

  file_summary <- do.call(
    rbind,
    lapply(file_results, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  )

  invisible(list(
    keep_full_id = keep_full_id,
    minTime = min_time,
    maxTime = max_time,
    time_filter_scope = time_filter_scope,
    time_filter_record_types = time_filter_record_types,
    show_progress = show_progress,
    parallel = use_parallel,
    n_cores = if (use_parallel) n_cores else 1L,
    files = file_summary
  ))
}


.write_fathom_lines <- function(lines, output_path) {
  payload <- paste(lines, collapse = "\n")
  payload <- paste0(payload, "\n")
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  con <- file(output_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(c(bom, charToRaw(payload)), con)
}


.build_keep_full_id_label <- function(keep_full_id, max_chars = 80) {
  sanitized_ids <- gsub("[^A-Za-z0-9_-]", "_", keep_full_id)
  combined <- paste(sanitized_ids, collapse = "-")

  if (nchar(combined, type = "chars") <= max_chars) {
    return(combined)
  }

  paste0(length(keep_full_id), "-ids")
}


.normalize_fathom_paths <- function(path) {
  stopifnot(length(path) >= 1, all(nzchar(path)))

  paths <- unique(unlist(lapply(path, function(one_path) {
    if (dir.exists(one_path)) {
      dir_files <- list.files(
        one_path,
        pattern = "\\.csv$",
        full.names = TRUE
      )

      if (!length(dir_files)) {
        stop(sprintf("No CSV files were found in '%s'.", one_path), call. = FALSE)
      }

      return(sort(dir_files))
    }

    if (!file.exists(one_path)) {
      stop(sprintf("Path '%s' was not found.", one_path), call. = FALSE)
    }

    one_path
  }), use.names = FALSE))

  if (!length(paths)) {
    stop("No Fathom CSV files were supplied.", call. = FALSE)
  }

  paths
}


.normalize_record_types <- function(record_types) {
  if (is.null(record_types)) {
    return(NULL)
  }

  stopifnot(length(record_types) >= 1)

  record_types <- gsub(",", "", record_types, fixed = TRUE)
  record_types <- trimws(record_types)
  record_types <- toupper(record_types)
  record_types <- unique(record_types[nzchar(record_types)])

  if (!length(record_types)) {
    stop("time_filter_record_types must contain at least one non-empty record type.", call. = FALSE)
  }

  record_types
}


.split_fathom_csv_line <- function(line) {
  strsplit(line, ",", fixed = TRUE)[[1]]
}


.get_fathom_desc_map <- function(lines) {
  desc_lines <- lines[grepl("_DESC,", lines, fixed = TRUE)]

  if (!length(desc_lines)) {
    return(list())
  }

  desc_parts <- lapply(desc_lines, .split_fathom_csv_line)
  names(desc_parts) <- vapply(desc_parts, `[[`, character(1), 1)
  desc_parts
}


.get_fathom_record_fields <- function(desc_map, record_type) {
  desc_name <- paste0(record_type, "_DESC")
  fields <- desc_map[[desc_name]]

  if (is.null(fields)) {
    return(NULL)
  }

  fields[-1]
}


.get_fathom_field_index <- function(desc_map, record_type, field_name) {
  fields <- .get_fathom_record_fields(desc_map, record_type)

  if (is.null(fields)) {
    return(NA_integer_)
  }

  idx <- match(field_name, fields)

  if (is.na(idx)) {
    return(NA_integer_)
  }

  idx + 1L
}


.parse_fathom_time_value <- function(x, tz = "UTC") {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    stop("Time values must be NULL, Date, POSIXt, or a single character string.", call. = FALSE)
  }

  formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%OS",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M:%OS"
  )

  for (fmt in formats) {
    parsed <- as.POSIXct(x, format = fmt, tz = tz)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  stop(
    sprintf("Could not parse time value '%s'.", x),
    call. = FALSE
  )
}


.extract_fathom_field <- function(line, field_index) {
  if (is.na(field_index)) {
    return(NA_character_)
  }

  parts <- .split_fathom_csv_line(line)

  if (length(parts) < field_index) {
    return(NA_character_)
  }

  parts[[field_index]]
}


.extract_fathom_record_time <- function(line, field_index, tz = "UTC") {
  value <- trimws(.extract_fathom_field(line, field_index))

  if (is.na(value) || !nzchar(value)) {
    return(as.POSIXct(NA, origin = "1970-01-01", tz = tz))
  }

  parsed <- tryCatch(
    .parse_fathom_time_value(value, tz = tz),
    error = function(e) NULL
  )

  if (is.null(parsed) || is.na(parsed)) {
    return(as.POSIXct(NA, origin = "1970-01-01", tz = tz))
  }

  parsed
}


.build_default_output_path <- function(path, keep_full_id) {
  file_parts <- tools::file_path_sans_ext(path)
  ext <- tools::file_ext(path)
  id_label <- .build_keep_full_id_label(keep_full_id)
  suffix <- paste0("-DET-", id_label, "-only")

  if (nzchar(ext)) {
    paste0(file_parts, suffix, ".", ext)
  } else {
    paste0(path, suffix)
  }
}


.resolve_output_paths <- function(paths, keep_full_id, output_path = NULL) {
  if (is.null(output_path)) {
    return(vapply(
      paths,
      FUN.VALUE = character(1),
      FUN = .build_default_output_path,
      keep_full_id = keep_full_id
    ))
  }

  if (length(output_path) == length(paths)) {
    return(output_path)
  }

  if (length(output_path) == 1L && (length(paths) > 1L || dir.exists(output_path))) {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    }

    return(file.path(output_path, basename(paths)))
  }

  if (length(paths) == 1L && length(output_path) == 1L) {
    return(output_path)
  }

  stop(
    "output_path must be NULL, a single file path, a directory path, or one output path per input file.",
    call. = FALSE
  )
}


.process_fathom_file <- function(
  one_path,
  one_output_path,
  keep_full_id,
  min_time,
  max_time,
  time_filter_scope,
  time_filter_record_types,
  tz
) {
  lines <- readLines(one_path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\r$", "", lines)
  desc_map <- .get_fathom_desc_map(lines)
  record_type <- sub(",.*$", "", lines)
  is_det <- record_type == "DET"
  is_diag <- record_type == "DIAG"

  det_time_index <- .get_fathom_field_index(desc_map, "DET", "Time")
  det_full_id_index <- .get_fathom_field_index(desc_map, "DET", "Full ID")
  diag_time_index <- .get_fathom_field_index(desc_map, "DIAG", "Time")

  keep_line <- rep(TRUE, length(lines))
  det_indices <- which(is_det)

  if (length(det_indices)) {
    det_full_ids <- vapply(
      lines[det_indices],
      FUN.VALUE = character(1),
      FUN = .extract_fathom_field,
      field_index = det_full_id_index
    )

    keep_line[det_indices] <- det_full_ids %in% keep_full_id
  }

  time_filtered_lines <- 0L

  if (!is.null(min_time) || !is.null(max_time)) {
    scope_mask <- if (!is.null(time_filter_record_types)) {
      record_type %in% time_filter_record_types
    } else {
      switch(
        time_filter_scope,
        det = is_det,
        det_diag = is_det | is_diag,
        all = rep(TRUE, length(lines))
      )
    }

    scoped_indices <- which(scope_mask)

    if (length(scoped_indices)) {
      scoped_types <- record_type[scoped_indices]
      record_times <- vapply(
        seq_along(scoped_indices),
        FUN.VALUE = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
        FUN = function(i) {
          one_index <- scoped_indices[[i]]
          one_type <- scoped_types[[i]]

          time_index <- switch(
            one_type,
            DET = det_time_index,
            DIAG = diag_time_index,
            .get_fathom_field_index(desc_map, one_type, "Time")
          )

          .extract_fathom_record_time(lines[[one_index]], field_index = time_index, tz = tz)
        }
      )

      parsed_time <- !is.na(record_times)
      keep_time <- rep(TRUE, length(scoped_indices))

      if (!is.null(min_time)) {
        keep_time[parsed_time] <- keep_time[parsed_time] & record_times[parsed_time] >= min_time
      }

      if (!is.null(max_time)) {
        keep_time[parsed_time] <- keep_time[parsed_time] & record_times[parsed_time] <= max_time
      }

      keep_line[scoped_indices] <- keep_line[scoped_indices] & keep_time
      time_filtered_lines <- sum(parsed_time & !keep_time)
    }
  }

  filtered_lines <- lines[keep_line]
  .write_fathom_lines(filtered_lines, one_output_path)

  list(
    input_path = one_path,
    output_path = one_output_path,
    total_lines = length(lines),
    det_lines = sum(is_det),
    kept_det_lines = sum(is_det & keep_line),
    removed_det_lines = sum(is_det & !keep_line),
    diag_lines = sum(is_diag),
    kept_diag_lines = sum(is_diag & keep_line),
    removed_diag_lines = sum(is_diag & !keep_line),
    time_filtered_lines = time_filtered_lines
  )
}


.get_fathom_det_full_id_counts <- fathom_logfile_detection_count
.filter_fathom_det_by_full_id <- edit_fathom_files


if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 2 || length(args) > 3) {
    stop(
      paste(
        "Usage:",
        "Rscript filter_fathom_det.R <input_csv> <full_id_to_keep> [output_csv]"
      ),
      call. = FALSE
    )
  }

  result <- edit_fathom_files(
    path = args[[1]],
    keep_full_id = strsplit(args[[2]], ",", fixed = TRUE)[[1]],
    output_path = if (length(args) >= 3) args[[3]] else NULL
  )

  cat(sprintf(
    "Wrote filtered file(s): %s\n",
    paste(result$files$output_path, collapse = ", ")
  ))
  cat(sprintf(
    "Kept DET rows for Full ID(s) %s: %s\n",
    paste(result$keep_full_id, collapse = ", "),
    sum(result$files$kept_det_lines)
  ))
  cat(sprintf("Removed DET rows: %s\n", sum(result$files$removed_det_lines)))
}
