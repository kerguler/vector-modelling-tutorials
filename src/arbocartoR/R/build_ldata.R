#' Generate local parameters matrix
#'
#' @description
#' The function computes and formats for each patch the list of local parameters required by the model.
#'
#' @param parcels data.frame or data.table describing the patches. Required columns: 'ID', 'POP', 'Kfix', 'Kvar', 'STATION' (optional), 'DIFF_ALT' (optional).
#' @param meteo data.frame or data.table reporting daily meteorological data. Required columns: 'ID', 'DATE', 'RR', 'TP'.
#' @param gdata list of parameters, can be generated using `build_gdata` function.
#' @param vector string. Default is "Ae. albopictus (D)".
#' @param virus string. Default is "DEN" (dengue virus).
#' @param mMov list of two matrices for movement probabilities. Defaults to NULL, which means no movement.
#' @param prev_control data.frame or data.table describing preventive control measures. Required columns: 'action', 'loc', 'start', 'end', 'p'.
#' @param start_date date in 'Y-m-d' format. Defaults to the earliest date in the meteorological dataset.
#' @param end_date date in 'Y-m-d' format. Defaults to the most recent date in the meteorological dataset.
#' @param nYR_init numeric. Number of years used to initialize population dynamics. Default is 1.
#'
#' @return matrix of local parameters.
#'
#' @importFrom pbapply pbapply
#'
#' @export

build_ldata <- function(parcels,
                        meteo,
                        gdata = NULL,
                        vector = "Ae. albopictus (D)",
                        virus = 'DEN',
                        mMov = NULL,
                        prev_control = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        nYR_init = 1) {

  ####################
  ### General data ###
  ####################

  # Check parcels
  check_parcels(parcels)

  # Check meteo
  if (!(is.data.frame(meteo) || is.data.table(meteo)))
    stop("meteo must be a data.table or a data.frame")

  meteo <- copy(meteo)
  setDT(meteo)

  required_meteo_cols <- c("ID", "DATE", "RR", "TP")
  if (!all(required_meteo_cols %in% names(meteo)))
    stop("meteo must contain at least the following columns: 'ID', 'DATE', 'RR', 'TP'")

  if (!inherits(meteo$ID, c("factor", "character")))
    stop('meteo$ID must be a character vector')

  ############################
  ### Model initialization ###
  ############################

  # Set default start and end dates
  if (is.null(start_date))
    start_date <- min(meteo$DATE) %>% as.Date

  if (is.null(end_date))
    end_date <- max(meteo$DATE) %>% as.Date

  if (!inherits(start_date, "Date"))
    start_date <- as.Date(start_date)

  if (!inherits(end_date, "Date"))
    end_date <- as.Date(end_date)

  if (start_date < min(meteo$DATE) || end_date > max(meteo$DATE))
    stop("start_date and end_date must be within the range of meteorological records")

  message(paste("Simulation will run for", ifelse('STATION' %in% names(parcels),
                                                  sum(parcels$STATION %in% meteo$ID), sum(parcels$ID %in% meteo$ID)),
                "parcels from", start_date, "to", end_date))

  # Create simulation time series
  TS_sim <- sim_time_serie(start_date, end_date, nYR_init)

  # Check if all meteorological data are available
  test_meteo <- expand.grid(ID = unique(meteo$ID), DATE = seq(from = start_date, to = end_date, by = "days"))
  test_meteo$DATE <- as.Date(test_meteo$DATE)
  meteo$DATE <- as.Date(meteo$DATE)

  setDT(test_meteo)
  setDT(meteo)

  if (nrow(test_meteo[!meteo, on = .(ID, DATE)]) > 0)
    stop("Daily meteorological records are missing for the period")

  ###############################################################
  ## Define compartments, gdata, and stochastic transitions ##
  ###############################################################

  # Validate global parameters
  gdata <- check_gdata(gdata, vector, virus, verbose = FALSE)

  # Prepare global data
  gdata <- unlist(gdata)
  mode(gdata) <- "numeric"

  ##################
  ## Define ldata ##
  ##################

  if (is.null(mMov)) {
    mMov <- diag(nrow(parcels))
    rownames(mMov) <- colnames(mMov) <- parcels$ID
    diag(mMov) <- 1

    mMov <- list(
      mi_mixedpop = parcels$POP,
      proba_ij = mMov,
      proba_ji = mMov
    )

    names(mMov$mi_mixedpop) <- parcels$ID

    message("No human mobility considered (mMov = NULL)")
  }

  ldata <- create_ldata(
    parcels = parcels,
    meteo = meteo,
    gdata = gdata,
    mMov = mMov,
    TS_sim = TS_sim,
    prev_control = prev_control
  )

  return(ldata)
}
