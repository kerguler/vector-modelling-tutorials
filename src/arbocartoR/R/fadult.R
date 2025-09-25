#' Function for adult gorging
#'
#' @description Function to write the temperature dependency for adult gorging depending on the species
#'
#' @param vector string vector species
#' @param gdata list of global data
#'
#' @return string
#'
#' @noRd

fadult <- function(vector, gdata){
  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){
    options(scipen=999)
    fadult <- paste0('((temperature - ', gdata["TAG"], ')/', gdata["TDDAG"], ')')
    options(scipen=0)
  }

  if(vector == "Ae. aegypti"){
    options(scipen=999)

    tempK = '(temperature + 273.15)'
    fadult <- paste('((',gdata["q1Ag"],'*', tempK, ' * ',gdata["q2Ag"],') * exp(',gdata["q3Ag"],' * (',gdata["q4Ag"],' - 1 / ',tempK,')) / (1 + exp(',gdata["q5Ag"],' * (',gdata["q6Ag"],' - 1 / ',tempK,'))))')
    options(scipen=0)
  }

  return(fadult)
}
