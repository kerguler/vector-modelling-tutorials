#' List compartments used in the model
#'
#' @description This function lists the compartments used in the model, categorized into
#' human and infected mosquito compartments (u0) and environmental and uninfected mosquito
#' compartments (v0).
#'
#' @usage list_compartments()
#'
#' @return A list with two elements:
#' \describe{
#'   \item{u0_compartments}{A vector of compartment names related to human and infected mosquitoes.}
#'   \item{v0_compartments}{A vector of compartment names related to environmental variables and uninfected mosquitoes.}
#' }
#'
#' @noRd
#'
#' @examples
#' compartments <- list_compartments()
#' print(compartments$u0_compartments)
#' print(compartments$v0_compartments)
#'
list_compartments <- function() {

  u0_compartments <- c(
    # Human compartments
    "Sh",    # Susceptible humans
    "Eh",    # Exposed humans
    "Ih",    # Infectious humans
    "Rh",    # Recovered humans

    # Infected mosquitoes (non-infected mosquitoes will be simulated into v0)
    "A1gmE", # Nulliparous mosquitoes (gorged after blood meal)
    "A1omE", # Nulliparous mosquitoes (seeking oviposition site)
    "A2hmE", # Parous mosquitoes (seeking host)
    "A2gmE", # Parous mosquitoes (gorged after blood meal)
    "A2omE", # Parous mosquitoes (seeking oviposition site)
    "A1gmI", # Nulliparous mosquitoes (gorged after blood meal, infected)
    "A1omI", # Nulliparous mosquitoes (seeking oviposition site, infected)
    "A2hmI", # Parous mosquitoes (seeking host, infected)
    "A2gmI", # Parous mosquitoes (gorged after blood meal, infected)
    "A2omI", # Parous mosquitoes (seeking oviposition site, infected)
    "Neggs", # New eggs from infected mosquitoes

    # Number of autochthonous human infections
    "ninfhL", # Number of infections in humans (latent)
    "ninfhE", # Number of infections in humans (exposed)
    "ninfm1", # Number of infections in mosquitoes (1st stage)
    "ninfm2"  # Number of infections in mosquitoes (2nd stage)
  )

  v0_compartments <- c(
    "z",         # Diapause (1 = Diapause, 0 = Favourable period for egg hatching)
    "temperature",# Temperature variable
    "RR_day",    # Daily rainfall
    "RR_7days",  # Rainfall over the past 7 days
    "kL",        # Carrying capacity for larvae
    "kP",        # Carrying capacity for pupae
    "Em",        # Eggs
    "Lm",        # Larvae
    "Pm",        # Pupae
    "Aemm",      # Emerging adult mosquitoes
    "A1hm",      # Nulliparous mosquitoes seeking host
    "A1gm",      # Nulliparous mosquitoes gorged after blood meal
    "A1om",      # Nulliparous mosquitoes seeking oviposition site
    "A2hm",      # Parous mosquitoes seeking host
    "A2gm",      # Parous mosquitoes gorged after blood meal
    "A2om",      # Parous mosquitoes seeking oviposition site
    "prevEggs",  # Previous eggs count
    "newEggs",   # Daily new layed eggs
    "nIm1",      # Number of infections in mosquitoes (1st stage)
    "nIm2",      # Number of infections in mosquitoes (2nd stage)
    "R0",        # Basic reproduction number
    "betaHext",  # External transmission rate for humans
    "betaMext",  # External transmission rate for mosquitoes
    "interv_Am", # Intervention parameters for adult mosquitoes
    "E2I",       # Transition from eggs to infectious mosquitoes
    "G2O"        # Growth transition to oviposition
  )

  return(list(u0_compartments = u0_compartments,
              v0_compartments = v0_compartments))
}
