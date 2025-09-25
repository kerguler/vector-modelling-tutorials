#' @importFrom data.table data.table

create_ldata <- function(parcels, meteo, gdata, mMov, TS_sim, prev_control){

  parcels %<>% copy
  parcels %>% setDT

  meteo %<>% unique
  meteo %<>% copy
  setDT(meteo)

  ## CHECKS
  meteorological_records <- ifelse("STATION" %in% names(parcels), TRUE , FALSE)

  PARCELS_ID <- if(meteorological_records) "STATION" else "ID"

  if(NA %in% match(parcels[,PARCELS_ID, with = FALSE] %>% unlist, meteo$ID))
    stop("Some parcels do not have any records in the meteorological dataset")

  if(meteo[ID %in% (parcels[,PARCELS_ID, with = FALSE] %>% unlist), .N, by = ID] %>% .[,N] %>% min < 365)
    stop("At least one year of meteorological data is required for initialization")

  if(FALSE %in%
     (as.IDate(TS_sim$time_serie_output_d) %in% meteo[ID %in% (parcels[,PARCELS_ID, with = FALSE] %>% unlist), DATE]))
    stop("Meteorological data are required for each day of the simulated period")

  ## CALCULATION

  message("## Generating parameters for all patches can take time, please be patient. ##")

  t_sim <- length(TS_sim$time_serie_date)

  # Initialization period
  meteo_baseline <- meteo

  meteo_grid <- expand.grid(DATE = TS_sim$time_serie_date,
                       ID = meteo$ID %>% unique)

  meteo <- merge(meteo_grid, meteo %>% unique, by = c("ID", 'DATE'), all.x = TRUE)
  setDT(meteo)

  ldata <- pbapply(parcels, 1, function(x){

    # Replace NA values for initialization period if missing from meteorological data
    while(TRUE %in% meteo[ID == x[PARCELS_ID], is.na(RR)]){

      NA_start <- meteo[ID == x[PARCELS_ID] & is.na(RR), DATE %>% as.IDate] %>% min %>% format(., "%m-%d")
      NA_length <- meteo[ID == x[PARCELS_ID] & is.na(RR), DATE] %>% length

      NA_start <- meteo[ID == x[PARCELS_ID] & !is.na(RR), which(DATE %>% as.IDate %>% format(., "%m-%d") %>% equals(NA_start))[1]]

      meteo[ID == x[PARCELS_ID] & is.na(RR), c("RR", "TP")] <-
        meteo_baseline[ID == x[PARCELS_ID] & !is.na(RR), c("RR", "TP")][NA_start:(NA_start + (NA_length-1))]
    }

    # Adjust temperature for altitude
    if(meteorological_records){
      temperature <- meteo[ID == x[PARCELS_ID], TP - as.numeric(x["DIFF_ALT"]) * (4.2 / 1000)]
    } else {
      temperature <- meteo[ID == x[PARCELS_ID], TP]
    }

    # Collect rainfall data
    rain <- meteo[ID == x[PARCELS_ID], RR]

    # Simplify carrying capacities
    kfix <- as.numeric(x["Kfix"])
    kvar <- as.numeric(x["Kvar"])

    c(t_sim,
      kfix, kvar,
      temperature,
      rain
    ) %>% as.numeric
  })

  colnames(ldata) <- parcels$ID

  # Add two rows
  ldata %<>% rbind(matrix(0, nrow = 3, ncol = ncol(ldata)), .)
  ldata[1,] <- ldata[4,]
  ldata[2,] <- diag(mMov$proba_ij[parcels$ID, parcels$ID])
  ldata[3,] <- diag(mMov$proba_ji[parcels$ID, parcels$ID])
  ldata[4,] <- mMov$mi_mixedpop[parcels$ID]

  proba_ij <- mMov$proba_ij[parcels$ID, parcels$ID] %>% t
  proba_ji <- mMov$proba_ji[parcels$ID, parcels$ID] %>% t

  diag(proba_ij) <- diag(proba_ji) <- 0

  ldata <- rbind(ldata, proba_ij, proba_ji)

  # Add prev_control measures
  if(!is.null(prev_control)){
    CSS <- data.table(matrix(0, nrow = prev_control[, .N], ncol = ncol(ldata)))
    setnames(CSS, colnames(ldata))

    for(x in seq_len(nrow(prev_control))){
      CSS[x, which(names(CSS) == prev_control[x, loc])] <- prev_control[x, p]
    }

    ldata <- rbind(ldata, as.matrix(CSS))
  }

  rownames(ldata) <- c("n_days",
                       "p2stay","pii", "mi_mixedpop",
                       "kfix", "kvar",
                       paste0("Tp_", seq(t_sim)),
                       paste0("RR_", seq(t_sim)),
                       paste0("Oij_", parcels$ID),
                       paste0("Dij_", parcels$ID),
                       if(!is.null(prev_control)) paste0("CSS_", seq_len(nrow(prev_control)))
  )

  return(ldata)
}
