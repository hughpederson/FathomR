

#' Generate Automated Report with All Dataframes in Global Environment
#'
#' This function creates a semi-automated report using all dataframes in the global environment.
#'
#' @param output_file The file path where the report should be saved (e.g., "report.html").
#' @return Generates an HTML report.
#' @export
receiver_performance_report <- function(output_file = "Receiver_Performance_Report.html") {
  # Capture all dataframes in the global environment
  data_list <- mget(ls(envir = .GlobalEnv, pattern = "^[a-zA-Z][a-zA-Z0-9_.]*$"), envir = .GlobalEnv)

  # Filter only dataframes
  data_list <- Filter(is.data.frame, data_list)

  # Path to the template file within the package
  template_path <- system.file("rmarkdown", "Receiver Performance Report.Rmd", package = "FathomR")

  # Check if the template file exists
  if (template_path == "") {
    stop("Template file not found.")
  }

  # Render the RMarkdown file with all dataframes available in the environment
  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = list(data_list = data_list),
    envir = new.env(parent = .GlobalEnv)  # Use a new environment with access to global variables
  )
}
