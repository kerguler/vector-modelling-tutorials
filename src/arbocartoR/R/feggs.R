#' Function for eggs development
#'
#' @description Function to write the temperature dependency for eggs development depending on the species
#'
#' @param vector string vector species
#' @param gdata list of global data
#'
#' @return string
#'
#' @noRd

feggs <- function(vector, gdata){
  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){
    options(scipen=999)
    feggs <- paste0('((temperature - ', gdata["TE"], ') / ', gdata["TDDE"], ')')
    options(scipen=0)
  }

  if(vector == "Ae. aegypti"){
    options(scipen=999)

    tempK = '(temperature + 273.15)'

    # feggfinal = ro * tempK * 24	 * exp((0.0033557 - (1 / tempK))) / (1 + exp(deltaHH * (T_12H - 1 / tempK)))
    # ( 3.57718120805369e-05  * (temperature + 273.15) * 24) * exp(0.0033557 - (1 / (temperature + 273.15))) / (1 + exp( 50327.1263210871  * ( 7.04994888787056e-05  - 1 / (temperature + 273.15))))
    feggs <- paste('(',gdata["q1E"],' * ', tempK,' * ', gdata["q2E"],' ) * exp(',gdata["q4E"],' - (1 / ',tempK,')) / (1 + exp(',gdata["q5E"],' * (',gdata["q6E"],' - 1 / ',tempK,')))')
    options(scipen=0)
  }
  return(feggs)
}
