#' Generate global parameters list
#'
#' @description
#' The functions formats all global parameters required for the model.
#'
#' @param vector string. "Ae. albopictus", "Ae. albopictus (D)" or "Ae. aegypti". Default is "Ae. albopictus (D)".
#' @param virus string.  "DEN" (dengue), "ZIK" (zika) or "CHI" (chikungunya). Default is "DEN" (dengue) virus.
#'
#' @param bHM numeric. Probability of infection from host to vector when an infected host is bitten by an susceptible vector
#' @param bMH numeric. Probability of infection from vector to host when a human is bitten by an infected mosquito.
#' @param muH  numeric. Daily transition rate from exposed (E) to infected (I) for humans (1/days)
#' @param rhoH  numeric. Daily recovery rate of humans (1/days)
#'
#' @param muE  numeric. Daily mortality rate of eggs (1/days)
#'
#' @param TE numeric. Minimal temperature needed for albopictus eggs development (°C)  (see details of albopictus functions)
#' @param TDDE numeric. Total number of degree-day necessary for albopictus eggs development (°C) (see details of albopictus functions)
#'
#' @param q1E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#' @param q2E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#' @param q3E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#' @param q4E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#' @param q5E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#' @param q6E numeric. Parameter for the aegypti egg development function (see details of aegypti functions)
#'
#' @param mu1L  numeric. Parameter for the larvae mortality function (see details)
#' @param mu2L  numeric. Parameter for the larvae mortality function (see details)
#' @param mu3L  numeric. Parameter for the larvae mortality function (see details)
#'
#' @param q1L numeric. Parameter for the function of transition from larva to pupa (see details)
#' @param q2L numeric. Parameter for the function of transition from larva to pupa (see details)
#' @param q3L numeric. Parameter for the function of transition from larva to pupa (see details)
#' @param q4L numeric. Parameter for the function of transition from larva to pupa (see details of aegypti functions)
#' @param q5L numeric. Parameter for the function of transition from larva to pupa (see details of aegypti functions)
#' @param q6L numeric. Parameter for the function of transition from larva to pupa (see details of aegypti functions)
#'
#' @param mu1P  numeric. Parameter for the pupae mortality function (see details)
#' @param mu2P  numeric. Parameter for the pupae mortality function (see details)
#' @param mu3P  numeric. Parameter for the pupae mortality function (see details)
#'
#' @param muEM  numeric. Daily mortality rate of emerging adults during the emergence (1/days)
#'
#' @param q1P numeric. Parameter for the function of transition from pupae to emerging adult (see details)
#' @param q2P numeric. Parameter for the function of transition from pupae to emerging adult (see details)
#' @param q3P numeric. Parameter for the function of transition from pupae to emerging adult (see details)
#' @param q4P numeric. Parameter for the function of transition from pupae to emerging adult (see details of aegypti functions)
#' @param q5P numeric. Parameter for the function of transition from pupae to emerging adult (see details of aegypti functions)
#' @param q6P numeric. Parameter for the function of transition from pupae to emerging adult (see details of aegypti functions)
#'
#' @param mu1A  numeric. Parameter for the adult mortality function: \eqn{mu1A \times e^{(temperature_{t}-10) \times  mu2A}  + mu3A}
#' @param mu2A  numeric. Parameter for the adult mortality function: \eqn{mu1A \times e^{(temperature_{t}-10) \times  mu2A}  + mu3A}
#' @param mu3A  numeric. Parameter for the adult mortality function: \eqn{mu1A \times e^{(temperature_{t}-10) \times  mu2A}  + mu3A}
#'
#' @param gammaAem numeric. Daily development rate of emerging adults (1/days)
#' @param sigma numeric. Sex-ratio at the emergence
#'
#' @param gammaAh numeric. Daily transition rate from host-seeking to engorged adults (1/days)
#' @param muR  numeric. Daily mortality rate related to seeking behavior (1/days)
#'
#' @param TAG numeric. Minimal temperature needed for egg maturation (°C) (see details of albopictus functions)
#' @param TDDAG numeric. Total number of degree-days necessary for egg maturation (°C) (see details of albopictus functions)
#'
#' @param q1Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#' @param q2Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#' @param q3Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#' @param q4Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#' @param q5Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#' @param q6Ag numeric. Parameter for the function driving eggs maturation after blood meal (see details of aegypti functions)
#'
#' @param gammaAo numeric. Daily transition rate from oviposition site-seeking to host-seeking adults (1/days)
#'
#' @param beta1 numeric.  Number of eggs laid by ovipositing nulliparous females (per female)
#' @param beta2 numeric.  Number of eggs laid by ovipositing parous females (per female)
#'
#' @param maxbite numeric.  Maximal number of bite per human.
#'
#' @param startFav date. First day of the favorable period for diapausing mosquitos.
#' @param endFav date. Last day of the favorable period for diapausing mosquitos.
#'
#' @param muErain numeric. Additional mortality of eggs due to heavy rain (> 80mm)
#' @param muLrain numeric. Additional mortality of larvae due to heavy rain (> 80mm)
#' @param muPrain numeric. Additional mortality of pupae due to heavy rain (> 80mm)
#'
#' @param verbose logical. Provide additional information on parameters during generation
#'
#' @details
#' Aegypti (with \eqn{temperature\_K_{t}} the daily temperature in kalvin):
#'
#' egg development function: \eqn{(q1E \times temperature\_K_{t} \times q2E \times e^{(q4E - (1 / temperature\_K_{t}))}) / (1 +  e^{q5E \times (q6E - 1 / temperature\_K_{t})})}
#'
#' transition function from larva to pupa: \eqn{(q1L \times temperature\_K_{t} \times q2L) \times e^{q3L \times (q4L - 1 / temperature\_K_{t})} / (1 + e^{q5L \times (q6L - 1 / temperature\_K_{t})})}
#'
#' transition function from pupa to emerging adult: \eqn{(q1P \times temperature\_K_{t} \times q2P) \times e^{q3P \times (q4P - 1 / temperature\_K_{t})} / (1 + e^{q5P \times (q6P- 1 / temperature\_K_{t})})}
#'
#' function for eggs maturation after blood meal: \eqn{(q1Ag \times temperature\_K_{t} \times q2Ag ) \times e^{q3Ag \times (q4Ag - 1 / temperature\_K_{t})} / (1 + e^{q5Ag \times (q6Ag - 1 / temperature\_K_{t})})}
#'
#' Albopictus (with \eqn{temperature\_C_{t}} the daily temperature in celsius degree):
#'
#' egg development function: \eqn{(temperature\_C_{t} - TE) / TDDE}
#'
#' larvae mortality function: \eqn{ mu1L \times e^{(temperature\_C_{t}-10) \times mu2L}  + mu3L}
#'
#' transition function from larva to pupa: \eqn{q1L \times temperature\_C_{t}^{2}  + q2L \times temperature\_C_{t}+ q3L}
#'
#' transition function from pupae to emerging adult: \eqn{q1P \times temperature\_C_{t}^{2}  + q2P \times temperature\_C_{t} + q3P}
#'
#' function for eggs maturation after blood meal: \eqn{(temperature\_C_{t} - TAG )/ TDDAG}
#'
#' Both:
#'
#' Larvae mortality function: \eqn{(mu1L \times e^{(temperature\_C_{t} - 10)\times mu2L} + mu3L)\times (1 + Lm/kL)}, with Lm the number of larvae and kL the carrying capacity (density dependance)
#'
#' Pupae mortality function: \eqn{mu1P \times e^{(temperature\_C_{t}-10) \times  mu2P}  + mu3P}
#'
#' @return Named list of parameters
#'
#' @examples build_gdata()
#'
#' @export

build_gdata <- function(
    vector = "Ae. albopictus (D)",
    virus = "DEN",
    ##### Infection
    # mosquitoes infection from host to vector
    bHM = NULL,
    # human infection probability from vector to host
    bMH = NULL,
    #
    muH = NULL, # transition rate from exposed (E) to infected (I) for humans (1/days)  (doi: 10.1038/s41598-019-53127-z : 1/2)
    rhoH = NULL, # Recovery rate of humans (1/days) (doi: 10.1038/s41598-019-53127-z : 1/4)
    # eggs
    muE = NULL,
    ### albo specific
    TE = NULL,
    TDDE = NULL,
    ### aegypti specific
    q1E = NULL,
    q2E = NULL,
    q3E = NULL,
    q4E = NULL,
    q5E = NULL,
    q6E = NULL,
    # larva
    mu1L = NULL,
    mu2L = NULL,
    mu3L = NULL,
    q1L = NULL,
    q2L = NULL,
    q3L = NULL,
    q4L = NULL,
    q5L = NULL,
    q6L = NULL,
    muEM = NULL,
    mu1P = NULL,
    mu2P = NULL,
    mu3P = NULL,
    q1P = NULL,
    q2P = NULL,
    q3P = NULL,
    q4P = NULL,
    q5P = NULL,
    q6P = NULL,
    # emerging adults
    mu1A = NULL,
    mu2A = NULL,
    mu3A = NULL,
    gammaAem = NULL,
    sigma = NULL,
    # host-seeking adults
    gammaAh = NULL,
    muR = NULL,
    # engorged adults
    ### albo specific
    TAG = NULL,
    TDDAG = NULL,
    ### aegypti specific
    q1Ag = NULL,
    q2Ag = NULL,
    q3Ag = NULL,
    q4Ag = NULL,
    q5Ag = NULL,
    q6Ag = NULL,
    # oviposition-site-seeking adults
    gammaAo = NULL,
    beta1 = NULL,
    beta2 = NULL,
    maxbite = NULL,
    startFav = NULL,
    endFav   = NULL,
    muErain = NULL,
    muLrain = NULL,
    muPrain = NULL,
    verbose = T) {

  ### CHECKS

  if(! vector %in% c("Ae. albopictus", "Ae. albopictus (D)", "Ae. aegypti"))
    stop(
      "Only Ae. albopictus in both temperate ('Ae. albopictus (D)') and tropical environments ('Ae. albopictus') and
      Ae. aegypti have been implemented yet, nevertheless all parameters can be modified."
    )

  if(! virus %in% c("DEN", "CHI", "ZIK"))
    stop(
      "Only dengue ('DEN'), chikungunya ('CHI') and zika ('ZIK') virus are implemented yet, nevertheless all parameters can be modified."
    )

  ### Defaults parameters

  ### Intrinsic incubation

  # zika    >> 3 - 14 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5403043/
  #   6.8 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9803243/#pone.0270127

  #   chik    >> 2 - 10 https://www.pasteur.fr/fr/centre-medical/fiches-maladies/chikungunya
  # 3 - 7 https://www.ncid.sg/Health-Professionals/Diseases-and-Conditions/Pages/Chikungunya.aspx

  # dengue  >> 2 - 7 (Pasteur) ;
  # 3 - 10 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3511440/
  #   4–10  World Health Organization and Special Programme for Research and Training in Tropical Diseases (2009) Dengue: Guideline for diagnosis, treatment, prevention and control. France: World Health Organization.
  # 3–14  Tomashek KM (2011) Dengue fever. CDC Health Information for International Travel 2012: The Yellow Book. USA: Oxford University Press. 156–161.
  # 3-9 https://pubmed.ncbi.nlm.nih.gov/17330791/
  #   3 - 10 https://www.cdc.gov/dengue/training/cme/ccm/page45915.html
  # 5.9 https://www.sciencedirect.com/science/article/pii/S1755436517300907

  ### Infectious period

  # zika    >> 10.8 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6137117/
  #   10 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9803243/#pone.0270127
  #   2 - 7 https://www.cdc.gov/zika/pdfs/clinicianppt.pdf
  # 2 - 7 https://www.ecdc.europa.eu/en/zika-virus-infection/facts/factsheet

  # chik    >> 2 - 5 https://wwwnc.cdc.gov/travel/yellowbook/2024/infections-diseases/chikungunya
  # 2 - 6 https://www.ncid.sg/Health-Professionals/Diseases-and-Conditions/Pages/Chikungunya.aspx

  # dengue  >> 7 https://www.cdc.gov/dengue/training/cme/ccm/page45915.html
  # 4 - 7 https://wwwnc.cdc.gov/travel/yellowbook/2024/infections-diseases/dengue
  # 4 - 10
  # 5 https://academic.oup.com/jid/article/181/1/2/892842


  ##### Infection human
  if(virus == "DEN"){
    # https://www.cdc.gov/dengue/training/cme/ccm/page45915.html
    if(is.null(muH)) muH = 1/mean(c(3,10)) # Intrinsic Incubation Period (3-14 days) - Once bitten, DENV replicates in the human for 3–14 days. After incubation, the human can become ill.
    if(is.null(rhoH)) rhoH = 1/7 # Infectious Period (about 7 days) - Both symptomatic and asymptomatic persons are viremic and can transmit DENV to mosquitoes that bite them during this approximately 7-day period. This viremic period is known as the "period of infectivity". In sick persons, viremia typically coincides with the presence of fever.
  }

  if(virus == "ZIK"){
    # https://www.who.int/health-topics/zika-virus-disease#tab=tab_1
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9803243/#pone.0270127.ref055
    if(is.null(muH)) muH = 1/mean(c(3,14)) # Intrinsic Incubation Period (3-14 days)
    if(is.null(rhoH)) rhoH = 1/7 # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9803243/#pone.0270127.ref055 & https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5403043/
  }

  if(virus == "CHI"){
    # https://www.cdc.gov/dengue/training/cme/ccm/page45915.html
    if(is.null(muH)) muH = 1/mean(c(3, 7)) # Intrinsic Incubation Period (3-7 days)
    if(is.null(rhoH)) rhoH = 1/mean(c(2, 6))  # Infectious Period: From up to 2 days before illness onset to 5 days after illness onset.
  }

  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){

    ##### Infection
    if(virus == "DEN"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4109365/
      if(is.null(bMH)) bMH = 0.31
      if(is.null(bHM)) bHM = 0.31
    }
    if(virus == "CHI"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4109365/
      if(is.null(bMH)) bMH = 0.33
      if(is.null(bHM)) bHM = 0.33
    }
    if(virus == "ZIK"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5963318/
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4911567/ - data from dengue paper below
      # https://pubmed.ncbi.nlm.nih.gov/23139836/ (Dengue)
      if(is.null(bMH)) bMH = 0.5
      if(is.null(bHM)) bHM = 0.41
    }

    ## Mosquitoes life cycle
    if(is.null(muE)) muE = 0.05
    if(is.null(TE)) TE = 10
    if(is.null(TDDE)) TDDE = 110
    if(is.null(mu1L)) mu1L = 0.0007
    if(is.null(mu2L)) mu2L = 0.1838
    if(is.null(mu3L)) mu3L = 0.02
    if(is.null(q1L)) q1L = -0.0007
    if(is.null(q2L)) q2L = 0.0392
    if(is.null(q3L)) q3L = -0.3911
    if(is.null(muEM)) muEM = 0.1
    if(is.null(mu1P)) mu1P = 0.0003
    if(is.null(mu2P)) mu2P = 0.2228
    if(is.null(mu3P)) mu3P = 0.02
    if(is.null(q1P)) q1P = 0.0008
    if(is.null(q2P)) q2P = -0.0051
    if(is.null(q3P)) q3P = 0.0319
    if(is.null(mu1A)) mu1A = 0.0003
    if(is.null(mu2A)) mu2A = 0.1745
    if(is.null(mu3A)) mu3A = 0.025
    if(is.null(gammaAem)) gammaAem = 0.4
    if(is.null(sigma)) sigma = 0.5
    if(is.null(gammaAh)) gammaAh = 0.3
    if(is.null(muR)) muR = 0.08
    if(is.null(TAG)) TAG = 10
    if(is.null(TDDAG)) TDDAG = 77
    if(is.null(gammaAo)) gammaAo = 0.28

    if(is.null(muErain)) muErain = 0.1
    if(is.null(muLrain)) muLrain = 0.5
    if(is.null(muPrain)) muPrain = 0.5
  }

  if(vector == "Ae. albopictus (D)")
  {
    if(is.null(beta1)) beta1 = 95
    if(is.null(beta2)) beta2 = 75
    if(is.null(startFav)) startFav = as.Date("10/03/2020", format = "%d/%m/%Y")
    if(is.null(endFav)) endFav = as.Date("30/09/2020", format = "%d/%m/%Y")
  }

  if(vector == "Ae. albopictus"){
    if(is.null(beta1)) beta1 = 60
    if(is.null(beta2)) beta2 = 80
  }

  if(vector == "Ae. aegypti"){

    ##### Infection
    if(virus == "DEN"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4109365/
      if(is.null(bMH)) bMH = 0.33
      if(is.null(bHM)) bHM = 0.33
    }
    if(virus == "CHI"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4109365/
      if(is.null(bMH)) bMH = 0.24
      if(is.null(bHM)) bHM = 0.24
    }
    if(virus == "ZIK"){
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5963318/
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4911567/ - data from dengue paper below
      # https://pubmed.ncbi.nlm.nih.gov/23139836/ (Dengue)
      if(is.null(bMH)) bMH = 0.5
      if(is.null(bHM)) bHM = 0.41
    }

    ## Mosquitoes life cycle
    if(is.null(muE)) muE = 0.01
    if(is.null(mu1L)) mu1L = 0.0007
    if(is.null(mu2L)) mu2L = 0.1838
    if(is.null(mu3L)) mu3L = 0.02
    if(is.null(muEM)) muEM = 0.1
    if(is.null(mu1P)) mu1P = 0.0003
    if(is.null(mu2P)) mu2P = 0.2228
    if(is.null(mu3P)) mu3P = 0.02
    if(is.null(mu1A)) mu1A = 0.0003
    if(is.null(mu2A)) mu2A = 0.1745
    if(is.null(mu3A)) mu3A = 0.025
    if(is.null(gammaAem)) gammaAem = 0.4
    if(is.null(sigma)) sigma = 0.5
    if(is.null(gammaAh)) gammaAh = 1.0
    if(is.null(muR)) muR = 0.08
    if(is.null(gammaAo)) gammaAo = 1.0
    if(is.null(beta1)) beta1 = 60
    if(is.null(beta2)) beta2 = 60

    if(is.null(q1E)) q1E = 0.01066 / 298 #ro
    if(is.null(q2E)) q2E = 24
    if(is.null(q3E)) q3E = 1
    if(is.null(q4E)) q4E = 0.0033557
    if(is.null(q5E)) q5E = 100000.0 / 1.987 # deltaHH
    if(is.null(q6E)) q6E = 1.0 / 14184.5 # T_12H

    if(is.null(q1P)) q1P = 0.0161 / 298
    if(is.null(q2P)) q2P = 24
    if(is.null(q3P)) q3P = 14931.94 / 1.987
    if(is.null(q4P)) q4P = 0.0033557
    if(is.null(q5P)) q5P = -472379 / 1.987
    if(is.null(q6P)) q6P = 1 / 148.45

    if(is.null(q1L)) q1L = 0.00873 / 298
    if(is.null(q2L)) q2L = 24
    if(is.null(q3L)) q3L = 26018.51 / 1.987 # deltaHA
    if(is.null(q4L)) q4L = 0.0033557
    if(is.null(q5L)) q5L = 55990.75 / 1.987
    if(is.null(q6L)) q6L = 1 / 304.58

    if(is.null(q1Ag)) q1Ag = 0.00898 / 298
    if(is.null(q2Ag)) q2Ag = 24
    if(is.null(q3Ag)) q3Ag = 15725.23 / 1.987
    if(is.null(q4Ag)) q4Ag = 0.0033557
    if(is.null(q5Ag)) q5Ag = 1756481.07 / 1.987
    if(is.null(q6Ag)) q6Ag = 1 / 447.17

    if(is.null(muErain)) muErain = 0.1
    if(is.null(muLrain)) muLrain = 0.2
    if(is.null(muPrain)) muPrain = 0.2

  }


  if(is.null(maxbite)) maxbite = 3 ## FIX ME

  ### Checks

  sapply(c(
    bMH,
    muH,
    muE,
    muEM,
    gammaAem,
    sigma,
    gammaAh,
    muR,
    gammaAo,
    bHM,
    muErain,
    muLrain,
    muPrain
  ), function(x){
    if(!is.null(x) & !inherits(x, 'function')){
      if(x < 0 | x > 1)
        stop(
          paste(x, " must fall between 0 and 1.")
        )}
  })

  if(verbose){
    if(vector == "Ae. albopictus"){
      message("Parameters were based on Ae. albopictus ecology in tropical environment")
    } else
      if(vector == "Ae. aegypti"){
        message("Parameters were based on Ae. aegypti ecology")}
  }

  if(vector == "Ae. albopictus (D)"){
    if(verbose)
      message("Parameters are based on Ae. albopictus ecology in temperate environment")

    if(!is.null(startFav) & !is.null(startFav)){
      if(inherits(startFav,"Date"))
        startFav %<>% as.Date

      if(inherits(endFav, "Date"))
        endFav %<>% as.Date

      if(verbose)
        message("Diapause period is from ",  format(endFav, "%m-%d"), " to ", format(startFav, "%m-%d"))
    }
  }

  sapply(c(beta1, beta2), function(x)
    if (x < 0)
      stop(
        "Number of eggs layed beta1 and beta2 must be positive numbers."
      ))


  if(vector %in% c("Ae. albopictus", "Ae. albopictus (D)")){
    gdata = list(
      # humans
      bMH = bMH, # Infection probability from vector to host - 0.5
      # delta = delta,
      muH = muH, # transition rate from exposed (E) to infected (I) for humans (1/days)  (doi: 10.1038/s41598-019-53127-z : 1/2)
      rhoH = rhoH, # Recovery rate of humans (1/days) (doi: 10.1038/s41598-019-53127-z : 1/4)
      # eggs
      muE = muE,
      TE = TE, # 10.4 in Tran 2013
      TDDE = TDDE,
      # larva
      mu1L = mu1L,
      mu2L = mu2L,
      mu3L = mu3L,
      q1L = q1L,
      q2L = q2L,
      q3L = q3L,
      # pupa
      muEM = muEM,
      mu1P = mu1P,
      mu2P = mu2P,
      mu3P = mu3P,
      q1P = q1P,
      q2P = q2P,
      q3P = q3P,
      # emerging adults
      mu1A = mu1A,
      mu2A = mu2A,
      mu3A = mu3A,
      gammaAem = gammaAem,
      sigma = sigma, # proportion of females
      # host-seeking adults
      gammaAh = gammaAh, # 0.2 dans Tran 2013
      muR = muR,
      # engorged adults
      TAG = TAG,
      TDDAG = TDDAG,
      # oviposition-site-seeking adults
      gammaAo = gammaAo,
      beta1 = beta1,
      beta2 = beta2,
      maxbite = maxbite,
      # mosquitoes infection
      bHM = bHM,
      muErain = muErain,
      muLrain = muLrain,
      muPrain = muPrain
    )

    if(vector == "Ae. albopictus (D)"){
      gdata %<>% append(., list(
        startFav = startFav,
        endFav   = endFav
      ))
    }

  }


  if(vector == "Ae. aegypti"){
    gdata = list(
      # humans
      bMH = bMH, # Infection probability from vector to host - 0.5
      # delta = delta,
      muH = muH, # transition rate from exposed (E) to infected (I) for humans (1/days)  (doi: 10.1038/s41598-019-53127-z : 1/2)
      rhoH = rhoH, # Recovery rate of humans (1/days) (doi: 10.1038/s41598-019-53127-z : 1/4)
      # mosquitoes infection
      bHM = bHM,
      ## Mosquitoes life cycle
      muE = muE,
      mu1L = mu1L,
      mu2L = mu2L,
      mu3L = mu3L,
      muEM = muEM,
      mu1P = mu1P,
      mu2P = mu2P,
      mu3P = mu3P,
      mu1A = mu1A,
      mu2A = mu2A,
      mu3A = mu3A,
      gammaAem = gammaAem,
      sigma = sigma,
      gammaAh = gammaAh,
      muR = muR,
      gammaAo = gammaAo,
      beta1 = beta1,
      beta2 = beta2,
      maxbite = maxbite,

      q1E = q1E,
      q2E = q2E,
      q3E = q3E,
      q4E = q4E,
      q5E = q5E,
      q6E = q6E,

      q1P = q1P,
      q2P = q2P,
      q3P = q3P,
      q4P = q4P,
      q5P = q5P,
      q6P = q6P,

      q1L = q1L,
      q2L = q2L,
      q3L = q3L,
      q4L = q4L,
      q5L = q5L,
      q6L = q6L,

      q1Ag = q1Ag,
      q2Ag = q2Ag,
      q3Ag = q3Ag,
      q4Ag = q4Ag,
      q5Ag = q5Ag,
      q6Ag = q6Ag,

      muErain = muErain,
      muLrain = muLrain,
      muPrain = muPrain
    )
  }

  return(gdata)

}
