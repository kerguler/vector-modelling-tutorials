#' Build matrix of contact probabilities
#'
#' @description This function estimate probability matrices of movements/trips between all the polygons of a SpatVector or sf object and return them along with other mobility metrics.
#'
#' @param SpatVec SpatVector or sf. Spatial vector of polygons representing patches. Polygons must include 'POP' and 'ID' attributes.
#' @param law string. Law used to calculate probabilities. Default is "NGravExp". See documentation of `TDLM::run_law` function for law options details.
#' @param param numeric. Parameter used to adjust the importance of distance or opportunity associated with the chosen law.
#' A single value or a vector of several parameter values can be used.
#' Not necessary for the original radiation law or the uniform law. (see TDLM package for more details)
#' Default is NULL. If NULL but required the parameter will be estimated by TDLM package.
#' @param p2move numeric. Optional. Daily probability to move from the residential patch or daily proportion of residents moving from the residential patch. p2move is required if outflow is not provided. If the outflow is provided, the p2move probability is not used.
#' @param outflow numeric. Optional. Average number of person moving out of each patch daily.
#' @param inflow numeric. Optional. Average number of person moving in each patch daily.
#' @param verbose logical. Display more information during calculation
#'
#' @importFrom TDLM extract_spatial_information check_format_names run_law extract_opportunities calib_param run_model
#' @importFrom sf st_as_sf
#' @importFrom magrittr multiply_by
#'
#' @return  List of mobility metrics:
#'
#' `mi_mixedpop` is the estimated daily number of persons in each administrative unit (staying residents + visitors).
#'
#' `proba_ij` is a normalized matrix of probabilities for movements of residents from patch i (rows) in patch j in j (columns) (proba_ij). The referent population is the resident population.
#'
#' `proba_ji` is a normalized matrix of probabilities for origin i (columns) of agents in patch j (rows) (proba_ji). The referent population is the total population during the day including staying residents and visitors.
#'
#' @export

build_mMov <- function(SpatVec,
                       law = "NGravExp",
                       param = NULL,
                       p2move = NULL,
                       outflow = NULL,
                       inflow = NULL,
                       verbose = F){

  # CHECKS
  message("Checks")

  ## Check SpatVec format

  if(!inherits(SpatVec, c("SpatVector", "sf")))
    stop("SpatVec must be either a SpatVector or a sf object")

  ### turn to sf is SpatVector
  if(inherits(SpatVec, c("SpatVector"))){
    if(verbose)
      message("Turn to sf")
    SpatVec %<>% st_as_sf
  }

  ## Check SpatVec attributes

  if(!("ID" %in% names(SpatVec)) | !("POP" %in% names(SpatVec)))
    stop("SpatVec must include 'POP' and 'ID' as polygons attributes.")

  ## Check param according to law

  if(is.null(param) & law %in% c("GravExp", "GravPow"))
    stop("For GravExp and GravPow laws, a parameter must be provided.")


  ## Check inflow and outflow format

  if(!is.null(outflow) & !is.null(p2move)){
    warning("Only the outflow provided will be used. To use the p2move probability, set outflow at NULL")
  }

  if(is.null(outflow) & is.null(p2move))
    stop("Either outflow or p2move should be provided.")

  if(!is.null(outflow) & length(outflow) != nrow(SpatVec))
    stop("For GravExp and GravPow laws, a parameter must be provided.")


  if(is.null(p2move)){
    p2move <- outflow / mi
  p2move[is.na(p2move)] <- 0
  }

  if(verbose)
    message("Build mi, mj, Oi")

  # Extract population
  mass <- data.frame(Population = round(SpatVec$POP))
  row.names(mass) <- SpatVec$ID

  mi <- as.numeric(mass[,"Population"])
  names(mi) <- rownames(mass)
  mj <- mi

  if(law %in% c("GravExp", "NGravExp","GravPow","NGravPow", "Schneider","RadExt", "Rad")){
    if(verbose)
      message("Compute distance between patches")
    spi <- extract_spatial_information(SpatVec,
                                       id = "ID",
                                       show_progress = verbose)

    distance <- spi$distance

    check_format_names(vectors = list(mi = mi,
                                      mj = mj),
                       matrices = list(distance = distance),
                       check = "format_and_names")


    if(is.null(param) & law %in% c("NGravExp", "NGravPow", "Schneider","RadExt"))
      param <- spi$surface %>% mean %>% calib_param(av_surf = ., law = law)

  } else distance = NULL


  if(law %in% c("Schneider", "Rad", "RadExt")){

    if(verbose)
      message("Compute opportunity vector")

    sij <- extract_opportunities(
      opportunity = mi,
      distance = distance,
      check_names = TRUE
    )

  } else sij = NULL

  if(verbose)
    message("Compute travelling distributions")
  res <- run_law(
    law = law,
    mass_origin = mi,
    mass_destination = mj,
    distance = distance,
    opportunity = sij,
    param = param,
    check_names = TRUE
  )

  if(verbose)
    message("Normalize probabilities")

  proba_ij <- res$proba
  proba_ij <- proba_ij / apply(proba_ij,1,sum)
  proba_ij[is.na(proba_ij)] <- 0
  proba_ij %<>% multiply_by(p2move)
  diag(proba_ij) <- 1 - p2move

  ###### Estimate the daily total population and proportion of resident in each patch using Production constrained model

  if(is.null(outflow))
    outflow <- p2move * mi

  if(is.null(inflow)){
    simulated_trips <- run_model(
      res$proba,
      model = "PCM",
      nb_trips = NULL,
      out_trips = outflow,
      average = T,
      nbrep = 1,
      maxiter = 50,
      mindiff = 0.01,
      check_names = FALSE
    )
    inflow <- colSums(simulated_trips$replication_1)
  }

  mi_mixedpop <- mi - outflow + inflow
  pii <- (mi - outflow)/mi_mixedpop
  pii[is.na(pii)] <- 0
  names(mi_mixedpop) <- colnames(proba_ij)

  proba_ji <- res$proba %>% t
  proba_ji <- proba_ji / apply(proba_ji,1,sum)
  proba_ji[is.na(proba_ji)] <- 0
  proba_ji %<>% multiply_by(1-pii)
  diag(proba_ji) <- pii

  colnames(proba_ji) <-
    colnames(proba_ij) <-
    rownames(proba_ji) <-
    rownames(proba_ij) <- row.names(mass)

  mob_features <- list(
    mi_mixedpop = mi_mixedpop,
    proba_ij = proba_ij,
    proba_ji = proba_ji)

  return(mob_features)
}
