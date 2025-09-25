#' Check parcels object
#'
#' @description Function to check the format of the parcels object.
#'
#' @param parcels A `data.frame`, `data.table`, or `SpatVector` object describing the patches.
#'
#' @importFrom data.table is.data.table copy setDT
#'
#' @noRd
#'
check_parcels <- function(parcels) {

  # Check if parcels is one of the accepted types
  if (!inherits(parcels, c("data.table", "data.frame", "SpatVector"))) {
    stop("parcels must be a data.table, data.frame, or SpatVector")
  }

  # Convert SpatVector to data.frame if needed
  if (inherits(parcels, "SpatVector")) {
    parcels <- as.data.frame(parcels)
  }

  # Convert to data.table
  setDT(parcels)

  # Check for required columns
  required_cols <- c("ID", "POP", "Kfix", "Kvar")
  if (!all(required_cols %in% names(parcels))) {
    stop(paste('parcels must contain at least the following columns:', paste(required_cols, collapse = ", ")))
  }

  # Check if ID column is of character type
  if (!is.character(parcels$ID)) {
    stop('parcels$ID must be a character vector')
  }

  # Check if STATION column exists and is of character type
  if ("STATION" %in% names(parcels) && !is.character(parcels$STATION)) {
    stop('parcels$STATION must be a character vector if present')
  }

  # Optional: Check the types of other columns if needed
  if (!is.numeric(parcels$POP)) {
    stop('parcels$POP must be a numeric vector')
  }
  if (!is.numeric(parcels$Kfix)) {
    stop('parcels$Kfix must be a numeric vector')
  }
  if (!is.numeric(parcels$Kvar)) {
    stop('parcels$Kvar must be a numeric vector')
  }

  # If needed, you can add more checks here

  invisible(TRUE)  # Return TRUE invisibly if all checks pass
}
