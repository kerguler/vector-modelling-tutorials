#' Function for larvae development
#'
#' @description Function to write the temperature dependency for larvae development depending on the species
#'
#' @param vector string vector species
#' @param gdata list of global data
#'
#' @return string
#'
#' @noRd

flarvae <- function(vector, gdata){
  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){
    options(scipen=999)
    flarvae <- paste0('(', gdata["q1L"], ' * temperature * temperature + ', gdata["q2L"], ' * temperature + ', gdata["q3L"], ')')
    options(scipen=0)
  }

  if(vector == "Ae. aegypti"){
    options(scipen=999)
    tempK = '(temperature + 273.15)'
    flarvae <- paste('((',gdata["q1L"],'*', tempK, ' * ',gdata["q2L"],') * exp(',gdata["q3L"],' * (',gdata["q4L"],' - 1 / ',tempK,')) / (1 + exp(',gdata["q5L"],' * (',gdata["q6L"],' - 1 / ',tempK,'))))')
    options(scipen=0)
  }

  return(flarvae)
}
