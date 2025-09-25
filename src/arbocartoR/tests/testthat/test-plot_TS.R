test_that("multiplication works", {
  data(parcels)
  data(meteo)

  parcels %<>% .[startsWith(ID, "06"),]
  parcels %<>% .[ID %in% unique(parcels$ID)[1:4],]

  traj <- run_arbocartoR(parcels = parcels,
                         vector = "Ae. albopictus (D)",
                         virus = "DEN",
                         meteo = meteo,
                         n_sim = 2)

  p <- plot_TS(traj,
               stage = c("Em", "Lm", "Pm"),
               parcels_ids = NULL,
               simulation = 1)

  expect_s3_class(p, "dygraphs")

  p <- plot_TS(traj,
               stage = c("Em", "Lm", "Pm"),
               parcels_ids = parcels$ID[1],
               simulation = 1)

  expect_s3_class(p, "dygraphs")


  p <- plot_TS(traj,
               stage = c("Em", "Lm", "Pm"),
               parcels_ids = parcels$ID[1],
               simulation = 1:2)

  expect_s3_class(p, "dygraphs")

})
