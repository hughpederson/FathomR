#' Parse Fathom Parquet Files
#
#' @param x %% ~~Describe \code{x}
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#'
#' ## The function is currently defined as
#' function (x)
#' {
#'   }
#'
#' @export parse_fathom_parquet_files
parse_fathom_parquet_files <- function(parent_directory, record_type = NULL) {

  subfolders <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[grepl("parquet", basename(subfolders))]

  df_list_by_type <- list()

  for (subfolder in subfolders) {

    parquet_files <- list.files(subfolder, pattern = "\\.parquet$", full.names = TRUE)

    if (length(parquet_files) > 0) {
      for (file in parquet_files) {
        file_record_type <- sub("\\.parquet$", "", basename(file))

        if (!is.null(record_type) && file_record_type != record_type) {
          next
        }

        message(paste("Reading file:", file))

        df <- read_parquet(file)

        if (file_record_type %in% names(df_list_by_type)) {
          df_list_by_type[[file_record_type]] <- bind_rows(df_list_by_type[[file_record_type]], df)
        } else {
          df_list_by_type[[file_record_type]] <- df
        }
      }
    }
  }

  if (!is.null(record_type)) {
    return(df_list_by_type[[record_type]])
  }
  return(df_list_by_type)
}


