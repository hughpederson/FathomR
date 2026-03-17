#' Match Diagnostic Data to Detections
#'
#' Match each detection row to the nearest diagnostic record (by time) within
#' the same serial, and append selected diagnostic fields to the detections.
#'
#' @param dets A data frame of detections containing `time_var` and `serial_var`.
#' @param diag A data frame of diagnostic records, or a list of data frames.
#' @param diag_sources When `diag` is a list, either a character vector of list
#' names to include or a list of data frames to bind together.
#' @param time_var Name of the time column present in both `dets` and `diag`.
#' @param serial_var Name of the serial/receiver column present in both `dets`
#' and `diag`.
#' @param diag_vars Character vector of diagnostic columns to append. Defaults
#' to all columns in `diag` except `serial_var` and `time_var`.
#' @param keep_diag_time Logical; if `TRUE`, include the matched diagnostic time
#' in the output.
#' @param diag_time_col Name of the output column to store matched diagnostic
#' time when `keep_diag_time = TRUE`.
#' @param suffix Suffix appended to diagnostic column names if they collide with
#' columns in `dets`.
#' @param max_diff Optional numeric maximum allowed absolute time difference.
#' If `time_var` is POSIXct, this is interpreted as seconds.
#' @param tie Tie-breaking rule when distances are equal: `"earlier"` or
#' `"later"`.
#' @return A data frame with the same rows as `dets`, augmented with the matched
#' diagnostic columns (and optionally the matched diagnostic time).
#' @examples
#' \dontrun{
#' dets <- data.frame(
#'   Serial = c("A", "A", "B"),
#'   Time = as.POSIXct(
#'     c("2020-01-01 00:00:05", "2020-01-01 00:00:15", "2020-01-01 00:00:10"),
#'     tz = "UTC"
#'   )
#' )
#' diag <- data.frame(
#'   Serial = c("A", "A", "B"),
#'   Time = as.POSIXct(
#'     c("2020-01-01 00:00:00", "2020-01-01 00:00:20", "2020-01-01 00:00:12"),
#'     tz = "UTC"
#'   ),
#'   Temp = c(1, 2, 3)
#' )
#' match_diag_to_dets(dets, diag, diag_vars = "Temp")
#' }
#' @export match_diag_to_dets
match_diag_to_dets <- function(dets,
                               diag,
                               diag_sources = NULL,
                               time_var = "Time",
                               serial_var = "Serial",
                               diag_vars = NULL,
                               keep_diag_time = FALSE,
                               diag_time_col = "diag_time",
                               suffix = "_diag",
                               max_diff = NULL,
                               tie = c("earlier", "later")) {

  tie <- match.arg(tie)

  if (!time_var %in% names(dets)) stop("time_var not found in dets.")
  if (!serial_var %in% names(dets)) stop("serial_var not found in dets.")

  if (is.list(diag) && !is.data.frame(diag)) {
    if (is.null(diag_sources)) {
      stop("diag is a list; provide diag_sources as names or a list of data frames.")
    }

    if (is.character(diag_sources)) {
      missing_sources <- setdiff(diag_sources, names(diag))
      if (length(missing_sources) > 0) {
        stop("diag_sources not found in diag list: ", paste(missing_sources, collapse = ", "))
      }
      diag_list <- diag[diag_sources]
    } else if (is.list(diag_sources)) {
      diag_list <- diag_sources
    } else {
      stop("diag_sources must be a character vector or a list of data frames.")
    }

    keep_df <- vapply(diag_list, is.data.frame, logical(1))
    if (!any(keep_df)) stop("No data frames found in diag_sources.")
    diag <- dplyr::bind_rows(diag_list[keep_df])
  } else if (is.list(diag_sources)) {
    keep_df <- vapply(diag_sources, is.data.frame, logical(1))
    if (!any(keep_df)) stop("No data frames found in diag_sources.")
    diag <- dplyr::bind_rows(diag_sources[keep_df])
  }

  if (!time_var %in% names(diag)) stop("time_var not found in diag.")
  if (!serial_var %in% names(diag)) stop("serial_var not found in diag.")

  if (is.null(diag_vars)) {
    diag_vars <- setdiff(names(diag), c(serial_var, time_var))
  }

  missing_diag_vars <- setdiff(diag_vars, names(diag))
  if (length(missing_diag_vars) > 0) {
    stop("diag_vars not found in diag: ", paste(missing_diag_vars, collapse = ", "))
  }

  if (!is.null(max_diff) && !is.numeric(max_diff)) {
    stop("max_diff must be numeric (in seconds if time_var is POSIXct).")
  }

  if (!length(diag_vars) && !isTRUE(keep_diag_time)) {
    return(dets)
  }

  collisions <- intersect(diag_vars, names(dets))
  if (length(collisions) > 0) {
    rename_map <- stats::setNames(collisions, paste0(collisions, suffix))
    diag <- dplyr::rename(diag, !!!rename_map)
    diag_vars <- ifelse(diag_vars %in% collisions,
                        paste0(diag_vars, suffix),
                        diag_vars)
  }

  if (isTRUE(keep_diag_time)) {
    existing <- c(names(dets), diag_vars)
    if (diag_time_col %in% existing) {
      diag_time_col <- make.unique(c(existing, diag_time_col))[length(existing) + 1]
    }
  }

  to_num_time <- function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
      return(as.numeric(x))
    }
    if (is.numeric(x)) return(x)
    suppressWarnings(as.numeric(as.POSIXct(x, tz = "UTC")))
  }

  dets <- dplyr::mutate(dets, .row_id = dplyr::row_number())

  diag_proto <- diag[0, diag_vars, drop = FALSE]
  if (isTRUE(keep_diag_time)) {
    diag_proto[[diag_time_col]] <- diag[0, time_var, drop = FALSE][[time_var]]
  }
  diag_cols <- c(diag_vars, if (isTRUE(keep_diag_time)) diag_time_col)

  det_split <- split(dets, dets[[serial_var]], drop = TRUE)
  diag_split <- split(diag, diag[[serial_var]], drop = TRUE)

  out <- lapply(names(det_split), function(serial) {
    det_sub <- det_split[[serial]]
    diag_sub <- diag_split[[serial]]

    diag_match <- diag_proto[rep(1, nrow(det_sub)), , drop = FALSE]

    if (is.null(diag_sub) || !nrow(diag_sub)) {
      return(cbind(det_sub, diag_match))
    }

    diag_sub <- diag_sub[!is.na(diag_sub[[time_var]]), , drop = FALSE]
    if (!nrow(diag_sub)) {
      return(cbind(det_sub, diag_match))
    }

    diag_sub <- diag_sub[order(diag_sub[[time_var]]), , drop = FALSE]
    if (isTRUE(keep_diag_time)) {
      diag_sub[[diag_time_col]] <- diag_sub[[time_var]]
    }

    diag_times <- to_num_time(diag_sub[[time_var]])
    keep_diag <- !is.na(diag_times)
    diag_sub <- diag_sub[keep_diag, , drop = FALSE]
    diag_times <- diag_times[keep_diag]

    det_times <- to_num_time(det_sub[[time_var]])

    if (!length(diag_times)) {
      return(cbind(det_sub, diag_match))
    }

    idx_right <- findInterval(det_times, diag_times)
    n <- length(diag_times)

    idx_before <- pmax(idx_right, 1)
    idx_after <- pmin(idx_right + 1, n)

    before_diff <- abs(det_times - diag_times[idx_before])
    after_diff <- abs(det_times - diag_times[idx_after])

    use_after <- after_diff < before_diff
    if (tie == "later") {
      use_after <- use_after | (after_diff == before_diff)
    }

    idx <- ifelse(is.na(det_times),
                  NA_integer_,
                  ifelse(use_after, idx_after, idx_before))

    if (!is.null(max_diff)) {
      too_far <- !is.na(idx) & abs(det_times - diag_times[idx]) > max_diff
      idx[too_far] <- NA_integer_
    }

    if (any(!is.na(idx))) {
      diag_match[!is.na(idx), diag_cols] <- diag_sub[idx[!is.na(idx)], diag_cols, drop = FALSE]
    }

    cbind(det_sub, diag_match)
  })

  out <- dplyr::bind_rows(out)
  out <- out[order(out$.row_id), , drop = FALSE]
  out$.row_id <- NULL
  out
}
