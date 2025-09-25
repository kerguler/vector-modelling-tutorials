#' Diapause periods
#'
#' @description Function to calculates days for diapause over all the simulated period
#'
#' @usage diapause(startFav, endFav, time_max, nYR_init)
#'
#' @param startFav Date. First day of the favourable period.
#' @param endFav Date. Last day of the favorable period.
#' @param TS_sim list. four vectors of simulated dates (dates) and simulated days (numeric) with and without the initialization period
#'
#' @return Numeric vector with days of start and end of the favorable period
#'
#' @keywords diapause
#'
#' @importFrom magrittr subtract
#' @importFrom data.table as.IDate
#'
#' @noRd

diapause <- function(startFav, endFav, TS_sim){

  # startFav <- which(format(TS_sim$time_serie_date, "%m-%d") == format(as.IDate(startFav), "%m-%d"))
  # endFav <- which(format(TS_sim$time_serie_date, "%m-%d") == format(as.IDate(endFav), "%m-%d"))

  x <- c(which(format(TS_sim$time_serie_date, "%m-%d") == format(as.IDate(startFav), "%m-%d")),
         which(format(TS_sim$time_serie_date, "%m-%d") == format(as.IDate(endFav), "%m-%d")))  %>%
    .[order(.)]

return(x)
}
