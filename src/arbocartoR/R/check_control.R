#' Check prev_control object
#'
#' @description Function to check the format and validity of the `prev_control` object.
#'
#' @param prev_control A `data.frame` or `data.table` describing preventive control measures implemented. Required columns: 'action', 'loc', 'start', 'end', 'p'.
#' @param sim_period A vector of dates specifying the simulation period. It should be in `Date` format.
#'
#' @details
#' The `prev_control` object should contain the following columns:
#' \itemize{
#'   \item 'action': A character vector with values 'K', 'L', or 'A'. 'K' denotes source reduction; 'L' denotes chemical larviciding; 'A' denotes fogging or area spraying.
#'   \item 'loc': A character vector with parcel IDs. These should match IDs in the relevant dataset.
#'   \item 'start': A `Date` vector indicating the start date of the measure.
#'   \item 'end': A `Date` vector indicating the end date of the measure.
#'   \item 'p': A numeric vector between 0 and 1. Represents the proportion of sites removed (for 'K'), additional mortality of adults (for 'A'), or additional mortality of larvae (for 'L').
#' }
#'
#' @importFrom data.table is.data.table copy setDT
#' @importFrom stats na.omit
#'
#' @noRd
#'
check_prev_control <- function(prev_control, sim_period) {

  # Ensure prev_control is a data.table
  if (!is.data.table(prev_control)) {
    if (is.data.frame(prev_control)) {
      prev_control <- setDT(copy(prev_control))
    } else {
      stop("prev_control must be a data.table or a data.frame")
    }
  }

  # Check required columns
  required_cols <- c("action", "loc", "start", "end", "p")
  missing_cols <- setdiff(required_cols, names(prev_control))
  if (length(missing_cols) > 0) {
    stop(paste("prev_control must contain the following columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check 'action' column
  if (!all(prev_control$action %in% c("K", "L", "A"))) {
    stop("'prev_control$action' must be one of 'K', 'L', or 'A'")
  }

  # Check 'start' and 'end' columns
  if (!inherits(prev_control$start, "Date") || !inherits(prev_control$end, "Date")) {
    stop("'prev_control$start' and 'prev_control$end' must be in Date format")
  }

  # Check that start and end dates are within the simulation period
  if (any(prev_control$start < min(sim_period) | prev_control$end > max(sim_period))) {
    stop("In prev_control, start and end dates must be within the simulation period.")
  }

  # Check 'p' column
  if (!is.numeric(prev_control$p) || any(prev_control$p < 0 | prev_control$p > 1)) {
    stop("'prev_control$p' must be numeric and between 0 and 1")
  }

  # Check that start dates precede end dates
  if (any(prev_control$start > prev_control$end)) {
    stop("Start date must precede end date in prev_control table.")
  }

  check_overlapping_actions <- function(prev_control) {
    # Helper function to check for overlaps within a subset of actions
    overlapping <- function(df) {
      df <- df[order(start, end)]
      overlaps <- FALSE
      for (i in 1:(nrow(df) - 1)) {
        if (df$end[i] >= df$start[i + 1]) {
          overlaps <- TRUE
          break
        }
      }
      return(overlaps)
    }

    # Identify overlapping actions
    overlaps_found <- FALSE
    # Extract unique actions and locations
    unique_actions <- unique(prev_control[, .(action, loc)])

    for (i in seq_len(nrow(unique_actions))) {
      action_loc <- unique_actions[i, ]
      # Subset control measures by action and location
      subset_control <- prev_control[action == action_loc$action & loc == action_loc$loc]

      if (nrow(subset_control) > 1) {
        # Check for overlapping periods if more than one action exists
        if (overlapping(subset_control)) {
          message <- paste("Overlapping actions detected in parcel:", action_loc$loc,
                           "for action:", action_loc$action, "\n",
                           capture.output(print(subset_control)), collapse = "\n")
          stop(message)
        }
      }
    }

    return(invisible(overlaps_found))
  }

  check_overlapping_actions(prev_control)

  # Example usage
  # prev_control should be a data.table with columns: action, loc, start, end
  # Replace with your actual data
  # prev_control <- data.table(action = ..., loc = ..., start = ..., end = ...)
  # check_overlapping_actions(prev_control)


  # Optional: Check for duplicates in the `prev_control` table
  # if (anyDuplicated(prev_control)) {
  #   stop("Duplicate rows found in prev_control.")
  # }

  return(TRUE)
}
