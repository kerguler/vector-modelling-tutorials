#' Plot trajectories of the first simulation
#'
#' @description Function used to plot the trajectories of any compartments (median and 95% interval over all the selected parcels and simulation).
#'
#' @param traj output of a run_arbocartoR simulation
#' @param stage String vector. Epidemiological or biological stages to visualize (see colnames of traj).
#' @param parcels_ids String vector. Patch ids to visualize (must match with ID column in traj objects).
#' @param simulation Numerical vector. Considered simulation (must be <= to the length of traj).
#'
#' @importFrom dygraphs dygraph dySeries
#' @importFrom stats quantile
#' @importFrom data.table setnames
#'
#' @return dygraphs plot
#'
#' @keywords plot trajectories visualization
#'
#' @examples
#'
#' \dontrun{
#' data(parcels)
#' data(meteo)
#'
#' parcels <- parcels[startsWith(ID, "06"),]
#'
#' traj <- run_arbocartoR(parcels = parcels,
#'                        vector = "Ae. albopictus",
#'                        virus = "DEN",
#'                        meteo = meteo)
#'
#' plot_TS(traj, stage = c("Em", "Lm", "Pm"), parcels_ids = NULL, simulation = 1)
#' }
#'
#' @export

plot_TS <- function(traj,
                    stage,
                    parcels_ids = NULL,
                    simulation = 1) {

  # Input validation
  if (any(!stage %in% names(traj[[1]]))) {
    stop("Stages must be columns in the trajectories from the traj list")
  }

  if (is.null(parcels_ids)) {
    parcels_ids <- unique(traj[[1]][, ID])
  } else if (any(!parcels_ids %in% traj[[1]][, ID])) {
    stop("Selected parcels_ids must be present in the ID columns of trajectories")
  }

  if (length(simulation) > length(traj) || any(simulation > length(traj))) {
    stop(paste("There are only", length(traj), "simulations in your traj object"))
  }

  # Combine data from selected simulations
  if (length(simulation) > 1) {
    data_select <- do.call(rbind, traj[simulation])
  } else {
    data_select <- traj[[simulation]]
  }

  # Filter data by parcels_ids
  data_select <- data_select[ID %in% parcels_ids, ]

  data2plot <- NULL

  for (stage2plot in stage) {
    # Compute quantiles for each stage
    dataOFstage <- data_select[, .(
      `0.25%` = quantile(get(stage2plot), 0.025),
      `50%` = quantile(get(stage2plot), 0.5),
      `97.5%` = quantile(get(stage2plot), 0.975)
    ), by = DATE]

    # Rename columns
    setnames(dataOFstage, c("0.25%", "50%", "97.5%"),
             c(paste("0.25%", stage2plot), stage2plot, paste("97.5%", stage2plot)))

    # Merge data for plotting
    if (is.null(data2plot)) {
      data2plot <- dataOFstage
    } else {
      data2plot <- merge(data2plot, dataOFstage, by = "DATE")
    }
  }

  # Create dygraph plot
  p <- dygraph(data2plot)

  for (stage2plot in stage) {
    p <- dySeries(p, c(paste("0.25%", stage2plot), stage2plot, paste("97.5%", stage2plot)))
  }

  p
}
