#' Write stochatic transitions
#'
#' @description Internal function to build the string vector describing stochastic transitions and counters. This vector is required by SimInf package.
#'
#' @usage build_transitions(gdata)
#'
#' @param gdata list of parameters
#'
#' @return String vector describing transitions
#'
#' @keywords internal
#'
#' @export

build_transitions <- function(gdata){
#### Stochastic transitions ####

## HUMAN ##
# IF THERE IS AT LEAST ONE INFECTED MOSQUITO
# Theorie: https://doi.org/10.1038/s41598-019-53127-z
# Evaluate the effect of temperature: https://doi.org/10.1038/s41598-022-10977-4
#
# Equation of infection: https://doi.org/10.1371/journal.pntd.0009153
# Parameter Value/Formula
# α(T)  || Biting rate (1/days)                        || 1/2 * 0.5 * (0.0043 * dT +0.0943)
# bVH   || Infection probability from vector to host   || 0.5
# bHV   || Infection probability from host to vector   || 0.31
# EIP(T)|| Extrinsic incubation period (days)          || 1.03 * (4 + exp(5.15–0. 123 + dT))
# δ     || Max. number of people infected by single mosquito (individuals/hectare)  || 4.5
# IIP = 1/νH || Intrinsic incubation period (days)     || 5
# r     || Recovery rate of humans (1/days)            || 1/5
#
# muH and rhoH are defined from: https://doi.org/10.1038/s41598-019-53127-z

stoch_transitions <- c(
  #### HUMANS INFECTION ####
   ## ninfh count the number of autochtonous transitions

  ### blood meal of infected mosquitoes in the patch

  # FIX ME adjust biting rate with arctan (a = arctan((gammaAh * Ah)/mi_mixedpop) * nmaxbite/(pi/2)

  # 1/ feed on susceptible resident human and infect him
  "A2hmI + Sh -> mi_mixedpop > 0 ? A2hmI * gammaAh * bMH * ((pii * Sh)/mi_mixedpop) * atan((gammaAh * (A1hm + A2hm + A2hmE + A2hmI))/mi_mixedpop) * maxbite/(M_PI/2) : 0 ->  A2gmI + Eh + ninfhL",

  # 2/ feed on human not resulting in a recorded infection
  "A2hmI -> mi_mixedpop > 0 ? A2hmI * gammaAh : 0 -> A2gmI",
  # "A2hmI -> mi_mixedpop > 0 ? A2hmI * gammaAh * (1 - ((1/mi_mixedpop) * pii * Sh * bMH)) : 0 -> A2gmI",


  ### Resident bitten in another patch

  # 3/ Infection of a resident human in an other patch
  "Sh -> mi_mixedpop > 0 ? betaMext * gammaAh * Sh * (1 - p2stay) * bMH * atan((gammaAh * (A1hm + A2hm + A2hmE + A2hmI))/mi_mixedpop) * maxbite/(M_PI/2) : 0 -> Eh + ninfhE",

  ### Epidemiological stages

  # 4/ Exposition (incubating)
  "Eh -> Eh * muH -> Ih",

  # 5/ Infectious period
  "Ih -> Ih * rhoH -> Rh",

  #### MOSQUITOS INFECTION ####

  # 6/ Infection during blood meal of nulliparous female
  # internal
  # "@ -> Ih > 0 ? (A1hm - (nIm1 - ninfm1)) * gammaAh * pii * (Ih/(Sh+Eh+Ih+Rh)) * bHM : 0 -> A1gmE + ninfm1",
  "@ ->  Ih > 0 ? (A1hm - (nIm1 - ninfm1)) * gammaAh * pii * Ih/(Sh+Eh+Ih+Rh) * bHM * (1/mi_mixedpop) * atan((gammaAh * (A1hm + A2hm + A2hmE + A2hmI))/mi_mixedpop) * maxbite/(M_PI/2) : 0 -> A1gmE + ninfm1",

  # external
  "@ -> (A1hm - (nIm1 - ninfm1)) * gammaAh * betaHext * bHM  -> A1gmE + ninfm1",

  # 7/ Infection during blood meal of parous female
  # internal
  "@ -> Ih > 0 ? (A2hm - (nIm2 - ninfm2)) * gammaAh * pii * Ih/(Sh+Eh+Ih+Rh) * bHM * (1/mi_mixedpop) * atan((gammaAh * (A1hm + A2hm + A2hmE + A2hmI))/mi_mixedpop) * maxbite/(M_PI/2) : 0 -> A2gmE + ninfm2",
  # external
  "@ -> (A2hm - (nIm2 - ninfm2)) * gammaAh * betaHext * bHM  -> A2gmE + ninfm2",

  # 8 - 12/ from exposed to infectious
  "A1gmE -> E2I > 0 ? A1gmE * 1/E2I : 0 -> A1gmI",
  "A1omE -> G2O > 0 && ((E2I - 1/G2O) > 0) ? A1omE *  1/(E2I - 1/G2O) : ((G2O > 0 && (E2I - 1/G2O)  <= 0) ? A1omE : 0 ) -> A1omI",
  "A2hmE -> G2O > 0 && ((E2I - 1/G2O - (1/gammaAo)) > 0) ? A2hmE *  1/(E2I - 1/G2O - (1/gammaAo)) : ((G2O > 0 && ((E2I - 1/G2O - (1/gammaAo))  <= 0)) ? A2hmE : 0 ) -> A2hmI",
  "A2gmE -> E2I > 0 ? A2gmE *  1/E2I : 0 -> A2gmI",
  "A2omE -> G2O > 0 && ((E2I - 1/G2O) > 0) ? A2omE *  1/(E2I - 1/G2O) : ((G2O > 0 && (E2I - 1/G2O)  <= 0) ? A2omE : 0 ) -> A2omI",

  #### INFECTED MOSQUITOS DEVELOPMENT ####

  # 13/ Egg maturing in nulliparous exposed female
  "A1gmE -> G2O > 0 ? A1gmE * G2O : 0 -> A1omE",
  # 14/ Oviposition of nulliparous exposed female
  paste0("A1omE -> A1omE * gammaAo -> A2hmE + ",gdata[["beta1"]]," * Neggs"),
  # 15/ Egg maturing in parous exposed female
  "A2gmE -> G2O > 0 ? A2gmE * G2O : 0 -> A2omE",
  # 16/ Oviposition of parous exposed female
  paste0("A2omE -> A2omE * gammaAo -> A2hmE + ",gdata[["beta2"]]," * Neggs"),


  # 17/ Egg maturing in nulliparous infectious female
  "A1gmI -> G2O > 0 ? A1gmI * G2O : 0 -> A1omI",
  # 18/ Oviposition of nulliparous infectious female
  paste0("A1omI -> A1omI * gammaAo -> A2hmI + ",gdata[["beta1"]]," * Neggs"),
  # 21/ Egg maturing in parous infectious female
  "A2gmI -> G2O > 0 ? A2gmI * G2O : 0 -> A2omI",
  # 22/ Oviposition of parous infectious female
  paste0("A2omI -> A2omI * gammaAo -> A2hmI + ",gdata[["beta2"]]," * Neggs"),

  # 23 - 32/ Mortality

  "A1gmE -> (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) > 1 ? A1gmE : A1gmE * (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) -> @",
  "A1omE -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A1omE : A1omE * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @",
  "A2hmE -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A2hmE : A2hmE * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @",
  "A2gmE -> (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) > 1 ? A2gmE : A2gmE * (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) -> @",
  "A2omE -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A2omE : A2omE * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @",

  "A1gmI -> (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) > 1 ? A1gmI : A1gmI * (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) -> @",
  "A1omI -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A1omI : A1omI * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @",
  "A2hmI -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A2hmI : A2hmI * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @",
  "A2gmI -> (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) > 1 ? A2gmI : A2gmI * (mu1A * exp((temperature-10) * mu2A) + mu3A + interv_Am) -> @",
  "A2omI -> (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) > 1 ? A2omI : A2omI * (mu1A * exp((temperature-10) * mu2A) + mu3A + muR + interv_Am) -> @"

)

return(stoch_transitions)
}
