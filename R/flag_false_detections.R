# flag_false_detections <- function(det, transmitter_id, time_var, transmission_delay = 60,
#                                   short_multiplier = 30, long_multiplier = 720) {
#   # Short and Long intervals based on transmission delay
#   short_interval <- short_multiplier * transmission_delay
#   long_interval <- long_multiplier * transmission_delay
#
#   # Add a new column to the det dataframe to store the false positive flag
#   det$false_positive_flag <- NA
#
#   # Get unique transmitter IDs from the det dataframe
#   transmitter_ids <- unique(det[[transmitter_id]])
#
#   # Loop through each unique Transmitter ID
#   for (i in 1:length(transmitter_ids)) {
#     # Select rows in det where the Transmitter ID matches
#     sel <- which(det[[transmitter_id]] == transmitter_ids[i])
#     sub <- det[sel,]
#
#     # Calculate time differences between detections (in seconds for more precision)
#     if (nrow(sub) == 1) {
#       # If the transmitter is only detected once, flag it as a false positive
#       det$false_positive_flag[sel] <- 2 # Flag as single detection
#     } else {
#       # Calculate time differences for multiple detections
#       time_diff <- as.numeric(difftime(sub[[time_var]][2:nrow(sub)],
#                                        sub[[time_var]][1:(nrow(sub)-1)],
#                                        units = 'secs'))
#
#       # Flag as false positive (2) if the number of long intervals exceeds short intervals
#       # Otherwise, flag as valid (1)
#       det$false_positive_flag[sel] <- ifelse(length(which(time_diff >= long_interval)) >
#                                                length(which(time_diff <= short_interval)), 2, 1)
#     }
#   }
#
#   # Create a summary dataframe with only false positives (flagged as 2)
#   false_positives_summary <- det[det$false_positive_flag == 2, ]
#
#   # Order the false positives summary by the transmitter ID (id_column)
#   false_positives_summary <- false_positives_summary[order(false_positives_summary[[transmitter_id]]), ]
#
#   # Return both the full dataframe and the summary of false positives as separate dataframes
#   return(list(full_data = det, false_positives_summary = false_positives_summary))
# }




#' Flag False Detections - False Detection Analysis (FDA)
#'
#'
#' False Detection Analysis flagging detections that are deemed to be potential
#' false positives. False positive detections are flagged in the detection
#' dataframe based on the criteria defined by Pincock (2012) based on the
#' presence of at least one short interval between successive detections and
#' there are more short intervals compared to long intervals. Single detections
#' of an ID are also flagged as potential false positives. %% ~~ A concise (1-5
#' lines) description of what the function does. ~~
#'
#'
#' @param det Dataframe containing detection records. If proceeded by
#' parse_log_files, use det dataframe.
#' @param transmitter_id variable describing the transmitter ID in det.
#' @param time_var Variable of type POSIXct describing time in the det
#' dataframe.
#' @param transmission_daley numeric value of type int describing the nominal
#' delay interval of the transmitter IDs in det.
#' @param short_multiplier numeric value of type int describing the lower
#' threshold of acceptable intervals between successive detections, default =
#' 30 x transmission_delay.
#' @param long_multiplier numeric value of type int describing the upper
#' threshold of acceptable intervals between successive detections, default =
#' 720 x transmission_delay.
#' @return Appends the false_positive_flag variable to the det dataframe to
#' identify detections as true positive (1) or false positive (2).
#' @note %% ~~further notes~~
#' @author H. Pederson (hugh.pederson@@innovasea.com)
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references For details on FDA see:
#'
#' Pincock, D. G. (2012): False Detections: What they are and how to remove
#' them from detection data. Document #: DOC-004691 Version 03, April 17, 2012
#'
#' https://support.fishtracking.innovasea.com/s/downloads?tabset-59625=1
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
#' @export flag_false_detections
flag_false_detections <- function(detection_df, transmitter_id, receiver_id, time_var, transmission_delay = 60,
                                  short_multiplier = 30, long_multiplier = 720) {

  # Short and Long intervals based on transmission delay
  short_interval <- short_multiplier * transmission_delay
  long_interval <- long_multiplier * transmission_delay

  # Add a new column to the detection_df dataframe to store the false positive flag
  detection_df$false_positive_flag <- NA

  # Initialize an empty summary dataframe
  summary_table <- data.frame(
    transmitter_id = character(),
    receiver_id = character(),
    total_detections = integer(),
    min_interval = numeric(),
    short_interval_count = integer(),
    long_interval_count = integer(),
    interval_ratio = numeric(),
    first_detection = as.POSIXct(character()),
    last_detection = as.POSIXct(character()),
    false_positive_flag = integer(),
    stringsAsFactors = FALSE
  )

  # Get unique combinations of transmitter_id and receiver_id from the detection_df dataframe
  unique_combinations <- unique(detection_df[, c(transmitter_id, receiver_id)])

  # Loop through each unique combination of Transmitter ID and Receiver ID
  for (i in 1:nrow(unique_combinations)) {
    # Select rows in detection_df where the Transmitter ID and Receiver ID match
    sel <- which(detection_df[[transmitter_id]] == unique_combinations[i, transmitter_id] &
                   detection_df[[receiver_id]] == unique_combinations[i, receiver_id])
    sub <- detection_df[sel,]

    # Sort by time within each Serial to ensure time differences are calculated correctly
    sub <- sub[order(sub[[time_var]]), ]

    # Handle case where there is only one detection
    if (nrow(sub) == 1) {
      detection_df$false_positive_flag[sel] <- 2 # Single detection is flagged as false positive

      # Fill in the summary table for this single detection
      summary_table <- rbind(summary_table, data.frame(
        transmitter_id = unique_combinations[i, transmitter_id],
        receiver_id = unique_combinations[i, receiver_id],
        total_detections = 1,
        min_interval = NA,
        short_interval_count = 0,
        long_interval_count = 0,
        interval_ratio = NA,
        first_detection = sub[[time_var]][1],
        last_detection = sub[[time_var]][1],
        false_positive_flag = 2
      ))
    } else if (nrow(sub) > 1) {  # Ensure there are at least two detections
      # Calculate time differences for multiple detections
      time_diff <- as.numeric(difftime(sub[[time_var]][2:nrow(sub)],
                                       sub[[time_var]][1:(nrow(sub)-1)],
                                       units = 'secs'))

      # Calculate the number of short and long intervals
      short_interval_count <- length(which(time_diff <= short_interval))
      long_interval_count <- length(which(time_diff >= long_interval))

      # Calculate interval ratio (for summary)
      interval_ratio <- ifelse(long_interval_count > 0, short_interval_count / long_interval_count, NA)

      # Flag all detections based on intervals
      flag <- ifelse(long_interval_count > short_interval_count, 2, 1)

      # Apply the same flag to all detections in this combination
      detection_df$false_positive_flag[sel] <- flag

      # Fill in the summary table for this combination
      summary_table <- rbind(summary_table, data.frame(
        transmitter_id = unique_combinations[i, transmitter_id],
        receiver_id = unique_combinations[i, receiver_id],
        total_detections = nrow(sub),
        min_interval = min(time_diff),
        short_interval_count = short_interval_count,
        long_interval_count = long_interval_count,
        interval_ratio = interval_ratio,
        first_detection = min(sub[[time_var]]),
        last_detection = max(sub[[time_var]]),
        false_positive_flag = flag
      ))
    }
  }

  # Return both the full dataframe and the summary dataframe
  return(list(full_data = detection_df, summary_table = summary_table,
              unique_combinations = unique_combinations))
}
