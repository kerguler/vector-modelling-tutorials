#' Filter Meteorological Data
#'
#' @description Filters meteorological data to retain only the rows corresponding to parcel IDs present in `parcels` and dates present in `TS_sim$time_serie_date`.
#'
#' @usage filter_meteo(parcels, meteo, TS_sim)
#'
#' @param parcels data.table with parcel information, including a column `ID` or `STATION`.
#' @param meteo data.table with meteorological data, including columns `ID` and `DATE`.
#' @param TS_sim list containing a vector `time_serie_date` of dates to be retained.
#'
#' @details
#' This function ensures that the meteorological data contains only the IDs found in `parcels` and that there are records for all dates in `TS_sim$time_serie_date`.
#'
#' @return data.table with filtered meteorological data.
#'
#' @keywords filter meteo
#'
#' @examples
#' # Example usage:
#' parcels <- data.table(ID = 1:3, STATION = c('A', 'B', 'C'))
#' meteo <- data.table(ID = c('A', 'B', 'C'), DATE = as.Date('2023-01-01') + 0:2)
#' TS_sim <- list(time_serie_date = as.Date('2023-01-01') + 0:2)
#' filter_meteo(parcels, meteo, TS_sim)
#'
#' @importFrom data.table data.table
#'
#' @export
#'
filter_meteo <- function(parcels, meteo, TS_sim) {
  # Identifier la colonne contenant les identifiants
  if ("STATION" %in% names(parcels)) {
    parcels_ID <- parcels$STATION
  } else {
    parcels_ID <- parcels$ID
  }

  # Vérifier l'existence des identifiants dans meteo
  if (!all(parcels_ID %in% meteo$ID)) {
    stop("Certains identifiants dans 'parcels' n'existent pas dans 'meteo'")
  }

  # Créer une table avec toutes les combinaisons d'identifiants et de dates requises
  required_combinations <- data.table(
    ID = rep(parcels_ID, each = length(TS_sim$time_serie_date)),
    DATE = rep(TS_sim$time_serie_date, times = length(parcels_ID))
  )

  # Filtrer les lignes de 'meteo' pour garder uniquement les combinaisons requises
  meteo_filtered <- meteo[required_combinations, on = .(ID, DATE), nomatch = NULL]

  return(meteo_filtered)
}
