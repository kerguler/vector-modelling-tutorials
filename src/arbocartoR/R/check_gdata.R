#' Check and build gdata
#'
#' @description Function to check and build the `gdata` object based on the provided vector and virus types.
#'
#' @param gdata A list of global parameters. If NULL, will be built using `build_gdata`.
#' @param vector A string indicating the mosquito vector. One of "Ae. albopictus", "Ae. albopictus (D)", or "Ae. aegypti". Default is "Ae. albopictus".
#' @param virus A string indicating the virus type. One of "DEN" (dengue), "ZIK" (zika), or "CHI" (chikungunya). Default is "DEN".
#' @param verbose Logical. If TRUE, will print additional information during the process. Default is TRUE.
#'
#' @return The validated `gdata` list.
#'
#' @noRd
#'
check_gdata <- function(gdata, vector = "Ae. albopictus", virus = "DEN", verbose = TRUE) {
  # Check if gdata is NULL
  if (is.null(gdata)) {
    if (is.null(vector)) {
      stop("If 'vector' is NULL, a set of global parameters ('gdata') must be provided.")
    }

    # Validate vector input
    if (!vector %in% c("Ae. albopictus", "Ae. albopictus (D)", "Ae. aegypti")) {
      stop(paste("Parameters for", vector, "haven't been implemented. Please provide a valid set of 'gdata' parameters or choose 'Ae. albopictus', 'Ae. albopictus (D)', or 'Ae. aegypti'."))
    }

    # Build gdata using the specified vector and virus
    gdata <- build_gdata(vector = vector, virus = virus, verbose = verbose)
  }

  # Retrieve the required parameter names
  required_params <- names(build_gdata(vector = vector, virus = virus, verbose = FALSE))

  # Check if gdata contains all required parameters
  missing_params <- setdiff(required_params, names(gdata))
  if (length(missing_params) > 0) {
    stop(paste("gdata must contain the following parameters:", paste(missing_params, collapse = ", "), ".\n",
               "You can use the function `build_gdata` to generate a properly formatted dataset."))
  }

  return(gdata)
}
