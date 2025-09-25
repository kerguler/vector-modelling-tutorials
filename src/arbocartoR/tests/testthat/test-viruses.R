

test_that("ZIKA", {

  f <- system.file("shape/SpatVec.shp", package = "arbocartoR")
  SpatVec <- terra::vect(f)

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  SpatVec <- SpatVec[SpatVec$ID %in% parcels$ID, ]

  introduction_pts <- build_E_random(
    period_start <- "2022/08/10" %>% as.Date,
    period_end  <- "2022/08/30" %>% as.Date,
    n_ind = NULL,
    n_events = 5,
    stage = "Eh",
    loc = parcels$ID[1])

  mMov <- build_mMov(SpatVec,
                     law = "GravExp",
                     param = 0.00001,
                     p2move = 0.9)

  ldata <- build_ldata(parcels,
                       meteo,
                       vector = "Ae. albopictus (D)",
                       virus = 'ZIK',
                       mMov = mMov)

  traj <- run_arbocartoR(parcels = parcels,
                      ldata = ldata,
                      vector = "Ae. albopictus (D)",
                      virus = 'ZIK',
                      n_sim = 2,
                      start_date = min(meteo$DATE) %>% as.Date,
                      end_date =  max(meteo$DATE) %>% as.Date,
                      introduction_pts = introduction_pts)

  expect_type(traj, "list")
  expect_s3_class(traj[[1]], "data.frame")
  expect_length(traj, 2)
})

test_that("DENGUE", {

  f <- system.file("shape/SpatVec.shp", package = "arbocartoR")
  SpatVec <- terra::vect(f)

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  SpatVec <- SpatVec[SpatVec$ID %in% parcels$ID, ]

  introduction_pts <- build_E_random(
    period_start <- "2022/08/10" %>% as.Date,
    period_end  <- "2022/08/30" %>% as.Date,
    n_ind = NULL,
    n_events = 5,
    stage = "Eh",
    loc = parcels$ID[1])

  mMov <- build_mMov(SpatVec,
                     law = "GravExp",
                     param = 0.00001,
                     p2move = 0.9)

  ldata <- build_ldata(parcels,
                       meteo,
                       vector = "Ae. albopictus (D)",
                       virus = 'DEN',
                       mMov = mMov)

  traj <- run_arbocartoR(parcels = parcels,
                      ldata = ldata,
                      vector = "Ae. albopictus (D)",
                      virus = 'DEN',
                      n_sim = 2,
                      start_date = min(meteo$DATE) %>% as.Date,
                      end_date =  max(meteo$DATE) %>% as.Date,
                      introduction_pts = introduction_pts)

  expect_type(traj, "list")
  expect_s3_class(traj[[1]], "data.frame")
  expect_length(traj, 2)
})


test_that("CHIKUNGUNYA", {

  f <- system.file("shape/SpatVec.shp", package = "arbocartoR")
  SpatVec <- terra::vect(f)

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  SpatVec <- SpatVec[SpatVec$ID %in% parcels$ID, ]

  introduction_pts <- build_E_random(
    period_start <- "2022/08/10" %>% as.Date,
    period_end  <- "2022/08/30" %>% as.Date,
    n_ind = NULL,
    n_events = 5,
    stage = "Eh",
    loc = parcels$ID[1])

  mMov <- build_mMov(SpatVec,
                     law = "GravExp",
                     param = 0.00001,
                     p2move = 0.9)

  ldata <- build_ldata(parcels,
                       meteo,
                       vector = "Ae. albopictus (D)",
                       virus = 'CHI',
                       mMov = mMov)

  traj <- run_arbocartoR(parcels = parcels,
                      ldata = ldata,
                      vector = "Ae. albopictus (D)",
                      virus = 'CHI',
                      n_sim = 2,
                      start_date = min(meteo$DATE) %>% as.Date,
                      end_date =  max(meteo$DATE) %>% as.Date,
                      introduction_pts = introduction_pts)

  expect_type(traj, "list")
  expect_s3_class(traj[[1]], "data.frame")
  expect_length(traj, 2)
})
