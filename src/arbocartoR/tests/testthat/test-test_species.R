test_that("simulations with aegypti", {

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  traj <- run_arbocartoR(parcels = parcels,
                      vector = "Ae. aegypti",
                      meteo = meteo,
                      n_sim = 2,
                      verbose = F)

  expect_type(traj, "list")
  expect_s3_class(traj[[1]], "data.frame")
  expect_length(traj, 2)

})


test_that("simulation works with introduction, human mobility and preventive control", {

  f <- system.file("shape/SpatVec.shp", package = "arbocartoR")
  SpatVec <- terra::vect(f)

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  SpatVec <- SpatVec[SpatVec$ID %in% parcels$ID, ]

  prev_control <- data.table(
    action = character(),
    loc = factor(),
    start = structure(numeric(0), class = "Date"),
    end = structure(numeric(0), class = "Date"),
    p = numeric()
  )

  # action : 'K': destruction of breeding sites, 'L': larviciding, "A": adulticiding

  prev_control %<>% list(prev_control, data.table(
    action = "K",
    loc = "060640000",
    start = as.Date("2022/07/01"),
    end = as.Date("2022/07/07"),
    p = 0.5)) %>% data.table::rbindlist(fill = TRUE)

  ## add prev_control action: increase the larvae mortality (larviciding)
  prev_control %<>% list(prev_control, data.table(
    action = "L",
    loc = "060640000",
    start = as.Date("2022/07/01"),
    end = as.Date("2022/07/07"),
    p = 0.5)) %>% data.table::rbindlist(fill = TRUE)

  prev_control %<>% list(prev_control, data.table(
    action = "L",
    loc = "061220000",
    start = as.Date("2022/07/07"),
    end = as.Date("2022/07/15"),
    p = 0.5)) %>% data.table::rbindlist(fill = TRUE)

  ## add prev_control action: increase the mosquito adult mortality (fumigation)
  prev_control %<>% list(prev_control, data.table(
    action = "A",
    loc = "060640000",
    start = as.Date("2022/08/01"),
    end = as.Date("2022/08/07"),
    p = 0.5)) %>% data.table::rbindlist(fill = TRUE)

  prev_control %<>% unique

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
                       vector = "Ae. aegypti",
                       mMov = mMov,
                       prev_control = prev_control)

  traj <- run_arbocartoR(parcels = parcels,
                      ldata = ldata,
                      vector = "Ae. aegypti",
                      n_sim = 2,
                      start_date = min(meteo$DATE) %>% as.Date,
                      end_date =  max(meteo$DATE) %>% as.Date,
                      introduction_pts = introduction_pts,
                      prev_control = prev_control,
                      verbose = F)

  expect_type(traj, "list")
  expect_s3_class(traj[[1]], "data.frame")
  expect_length(traj, 2)

})
