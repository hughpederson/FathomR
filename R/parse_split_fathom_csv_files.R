#' Parse Split Fathom csv Files %% ~~function to do ... ~~
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
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
#' @export parse_split_fathom_csv_files
parse_split_fathom_csv_files <- function(parent_directory, record_type = NULL) {

  subfolders <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[grepl("csv-fathom-split", basename(subfolders))]

  df_list_by_type <- list()

  for (subfolder in subfolders) {

    csv_files <- list.files(subfolder, pattern = "\\.csv$", full.names = TRUE)

    if (length(csv_files) > 0) {

      for (file in csv_files) {
        file_record_type <- sub("\\.csv$", "", basename(file))
        if (!is.null(record_type) && file_record_type != record_type) {
          next
        }

        message(paste("Reading file:", file))

        df <- tryCatch({
          fread(file, header = TRUE, skip = 2, sep = ",", fill = TRUE, stringsAsFactors = FALSE, showProgress = FALSE)
        }, error = function(e) {
          message(paste("Error in reading file:", file))
          return(NULL)
        })

        if (is.null(df)) {
          next
        }

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
