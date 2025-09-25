#' Function for pupae development
#'
#' @description Function to write the temperature dependency for pupae development depending on the species
#'
#' @param vector string vector species
#' @param gdata list of global data
#'
#' @return string
#'
#' @noRd

fpupae <- function(vector, gdata){

  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){
    options(scipen=999)
    fpupae <- paste0('(', gdata["q1P"], ' * temperature * temperature + ', gdata["q2P"], ' * temperature + ', gdata["q3P"], ')')
    options(scipen=0)
    }

    if(vector == "Ae. aegypti"){
      options(scipen=999)
      tempK = '(temperature + 273.15)'
      fpupae <- paste('((',gdata["q1P"],'*', tempK, ' * ',gdata["q2P"],') * exp(',gdata["q3P"],' * (',gdata["q4P"],' - 1 / ',tempK,')) / (1 + exp(',gdata["q5P"],' * (',gdata["q6P"],' - 1 / ',tempK,'))))')
      options(scipen=0)
    }
  return(fpupae)
}
