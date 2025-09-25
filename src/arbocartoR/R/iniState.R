#' Write initial state of the meta-population
#'
#' @description initialize
#' @usage iniState(parcels, diapause = FALSE, initMosq = 100000)
#'
#' @param parcels data.frame or data.table
#' @param diapause logical
#' @param initMosq numerical value Initial number of eggs in each node.
#'
#' @return list with two data.frame: u0 and v0
#'
#' u0 is the initial population stage for each compartment in each parcel. Each row describe a patch and in columns:
#'  "Sh": susceptible humans ; "Eh": exposed humans ; "Ih": infectious humans ;"Rh": recovered humans ;
#'  "A1gmE", "A1omE", "A2hmE", "A2gmE", "A2omE": exposed stages for adult mosquitoes (see details of stages below)
#'  "A1gmI"  "A1omI"  "A2hmI"  "A2gmI"  "A2omI": infectious stages for adult mosquitoes (see details of stages below)
#'  "Neggs": number of eggs layed by infected mosquitoes
#'  "ninfhL": number of local human autochtonous infection
#'  "ninfhE": number of external human autochtonous infection
#'  "ninfm1": number of nulliparous mosquitoes autochtonous infection
#'  "ninfm2": number of parous mosquitoes autochtonous infection
#'
#' v0 is the initial population and time-dependant parameters state. Each columns describe a patch and in rows:
#' z: diapause (0 = dipause, 1 = favorable period); temperature; kL and kP: carrying capacities for larvae and pupae (rainfall dependent);
#' Em: number of eggs ; Lm: number of larvae ; Pm: number of pupae ; Aemm: number of emerging adults ;
#' A1hm: number of nulliparous adults seeking for host ; A1gm: number of gorged nulliparous adults ; A1om: number of nulliparous adults seeking for oviposition sites ;
#' A2hm: number of parous adults seeking for host ; A2gm: number of gorged parous adults ; A1om: number of parous adults seeking for oviposition sites ;
#' prevEggs, nIm1, nIm2, R0, betaHext and betaMext are continous variables calculated over time)
#' newEggs: daily number of new layed eggs
#'
#' @keywords demography
#'
#' @export


iniState <- function(parcels, diapause = FALSE, initMosq = 100000){

  nh <- parcels[, POP] %>% round
  nadm <- nrow(parcels)

u0 <- data.frame(
  # Human
  Sh = unlist(nh),
  Eh = rep(0, nadm),
  Ih = rep(0, nadm),
  Rh = rep(0, nadm),
  # # diagnosed
  # dEh = rep(0, nadm),
  # dIh = rep(0, nadm),
  # dRh = rep(0, nadm),

  # Infected mosquitoes (non infected mosquitoes will be simulated into v0) - exposed (E) and infectious (I)
  A1gmE = rep(0, nadm),
  A1omE = rep(0, nadm),
  A2hmE = rep(0, nadm),
  A2gmE = rep(0, nadm),
  A2omE = rep(0, nadm),

  A1gmI = rep(0, nadm),
  A1omI = rep(0, nadm),
  A2hmI = rep(0, nadm),
  A2gmI = rep(0, nadm),
  A2omI = rep(0, nadm),
  Neggs = rep(0, nadm), #new eggs from infected mosquitoes

  # Number of autochtonous human infection
  # ninfh = rep(0, nadm),
  ninfhL = rep(0, nadm),
  ninfhE = rep(0, nadm),
  ninfm1 = rep(0, nadm),
  ninfm2 = rep(0, nadm)
)

###############
## Create v0 ##
###############
## v0 is a continuous variable updated at each time step.
## v0 must contain an index for diapause (z: 0=unfavorable period; 1 = favorable period), the current
## temperature and carrying capacities (kL and kP) and non infected mosquito population dynamics.
## First, make sure the variables are defined by creating v0.
## Inititialize them to zero.

cont_var <- list_compartments()$v0_compartments

v0 <- matrix(0,
             nrow = length(cont_var),
             ncol = parcels[, ID] %>% length,
             dimnames=list(cont_var,
               unlist(parcels[, ID])
               ))

## Random initial mosquito population size (eggs)
v0["Em", ] <- initMosq # runif(nadm, 10000, 50000) %>% ceiling

if(!diapause) v0["z", ] <- 1

return(list(u0 = u0,
            v0 = v0))
}
