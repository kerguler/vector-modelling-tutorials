#' Time series of the simulated periods
#'
#' @description Function to calculate the dates and time points for simulations.
#'
#' @usage sim_time_serie(start_date, end_date, nYR_init)
#'
#' @param start_date Date. First day of output desired.
#' @param end_date Date. Last day of output desired.
#' @param nYR_init Numerical value. Number of years for initialization of the dynamics.
#'
#' @return list with dates and numeric vectors of simulated days (initialization always starts on January first).
#'
#' @keywords time series
#'
#' @importFrom data.table as.IDate
#'
#' @noRd

sim_time_serie <- function(start_date, end_date, nYR_init) {
  # Ensure dates are in Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Generate output date sequence
  time_serie_output_d <- seq.Date(start_date, end_date, by = "day")
  time_serie_output_t <- seq_along(time_serie_output_d)

  # Calculate the initialization start date
  year_start <- as.numeric(format(start_date, "%Y")) - nYR_init
  init_start_date <- as.IDate(paste0(year_start, "-01-01"))

  # Generate initialization date sequence
  time_serie_date <- seq.Date(init_start_date, end_date, by = "day")
  time_serie_num <- seq_along(time_serie_date) - 1

  return(list(
    time_serie_date = time_serie_date,
    time_serie_num = time_serie_num,
    time_serie_output_d = time_serie_output_d,
    time_serie_output_t = time_serie_output_t
  ))
}
