#' Parcelle data
#'
#' @name parcels
#' @docType data
#' @format data frame with columns:
#' @description The STATION, DIFF_ALT, and ALT are mandatory in case of using data from meteorological stations and should be removed if the IDs of the meteorological dataset reflect the parcels IDs.
#' \describe{
#'   \item{ID}{unique identifier for each parcel}
#'   \item{NOM_COM}{name of the city associated to the parcel (optional)}
#'   \item{SURF_HA}{surface in hectare (optional)}
#'   \item{STATION}{unique identifier of the meteorological station associated to the parcel (optional)}
#'   \item{DIFF_ALT}{difference between average altitude of the parcel and the altitude of the associated meteorological station (optional)}
#'   \item{ALT}{average altitude of the parcel (optional)}
#'   \item{Kfix}{fix carrying capacity (number of larvae in anthropic breeding sites)}
#'   \item{Kvar}{varying carrying capacity (number of larvae in natural breeding sites)}
#'   \item{POP}{human population size}
#'
#'   }
#' @source iris-ge, urban atlas, CLC
#' @keywords data

NULL

#' Meteorological data
#'
#' @name meteo
#' @docType data
#' @format data frame with columns:
#' \describe{
#'   \item{ID}{unique identifier. If the meteorological data are associated to meteorological stations the unique identifier is the one of the meteorological station (reflecting the 'STATION' column of the parcel dataset), otherwise the identifier is the one of the associated parcel.}
#'   \item{DATE}{date of the meteorological record}
#'   \item{RR}{daily precipitation/rainfall (in mm)}
#'   \item{TP}{daily average temperature (in degree)}
#'
#'
#' }
#' @source meteofrance
#' @keywords data

NULL


#' Carrying capacity estimates per land cover type
#'
#' @name configK
#' @docType data
#' @format data frame with columns:
#' \describe{
#'   \item{ID}{...}
#' }
#' @source mtd
#' @keywords data

NULL
