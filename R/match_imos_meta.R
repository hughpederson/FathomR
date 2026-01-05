#' Match IMOS receiver metadata to parsed Fathom files
#'
#' This function uses IMOS receiver metadata to add IMOS metadata, including receiver_deployment_id, installation_name, station_name, and deployment locations (receiver_deployment_latitude and receiver_deployment_longitude)
#' to the parsed Fathom files. 
#'
#' @param input List of files opened with parse_fathom_files()
#' @param imos_meta Path to the IMOS receiver deployment metadata csv file
#' @return List of Fathom files, including IMOS metadata information. Please note that rows with no matching IMOS metadata will not be returned in the
#' matched outputs.
#' @export
#' 
match_imos_meta <- function(input, imos_meta) { 
  output <- input
  # Load IMOS data and sort columns
  imos <- read.csv(imos_meta)
  imos <- subset(imos, active == "NO")
  imos$Serial <- suppressWarnings(as.integer(stringr::str_split_fixed(imos$receiver_name, pattern = "-", n = 2)[,2]))
  imos <- imos[which(imos$Serial %in% unique(input[[1]]$Serial)),] # Select only receivers in the data
  if (nrow(imos) == 0)
    stop("No IMOS metadata found for the receivers present in the Fathom files.") 
  imos$receiver_deployment_datetime <- as.POSIXct(imos$receiver_deployment_datetime,
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  imos$receiver_recovery_datetime <- as.POSIXct(imos$receiver_recovery_datetime,
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  # List of files to be processed
  files.aux <- names(input)
  files.tot <- c("attitude", "battery", "cfg_channel",
    "clock_ref", "depth", "det", "diag", "event", "event_init",
    "event_offload", "health_vr2ar", "temp")
  files.aux <- files.tot[which(files.tot %in% files.aux)]

  # Processing starts
  message("Matching receiver deployment metadata")
  pb <- txtProgressBar(min = 0, max = length(files.aux), initial = 0, style = 3, width = 50)
  for (i in 1:length(files.aux)) {
    aux.file <- input[[files.aux[i]]]
    aux.matched <- data_match(df = aux.file, meta = imos)
    # Replace original file by matched file
    index <- which(names(output) == files.aux[i])
    output[[index]] <- aux.matched
    setTxtProgressBar(pb, i) 
  } 
  close(pb)
  return(output)
}

# Auxiliary function to combine datasets
data_match <- function(df, meta) {
  # Fixed timestamps (points)
  points <- data.table::data.table(df)
  # Time intervals with values to attach
  intervals <- data.table::data.table(meta)
  points_interval <- points
  points_interval$receiver_deployment_datetime <- points_interval$Time
  points_interval$receiver_recovery_datetime <- points_interval$Time
  # Key intervals
  data.table::setkey(intervals, Serial, receiver_deployment_datetime, receiver_recovery_datetime)
  data.table::setkey(points_interval, Serial, receiver_deployment_datetime, receiver_recovery_datetime)

  # Join
  result <- as.data.frame(data.table::foverlaps(
    points_interval,
    intervals,
    by.x = c("Serial", "receiver_deployment_datetime", "receiver_recovery_datetime"),
    by.y = c("Serial", "receiver_deployment_datetime", "receiver_recovery_datetime"),
    type = "within",
    nomatch = NULL
  ))
  # Tidy things up
  result <- tibble::as_tibble(result[,c(names(points), "receiver_deployment_id", "installation_name", "station_name", "receiver_deployment_latitude", "receiver_deployment_longitude")])
  return(result)
}
