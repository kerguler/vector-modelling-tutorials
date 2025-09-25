test_that("build_ldata", {

  parcels %<>% .[startsWith(ID, "06") & (grepl("Gatt", NOM_COM) | grepl("Saint-Jeannet", NOM_COM) | grepl("La Gaude", NOM_COM)),]

  ldata <- build_ldata(parcels,
                          meteo,
                          gdata = NULL,
                          vector = "Ae. aegypti",
                          virus = 'ZIK',
                          mMov = NULL,
                          prev_control = NULL,
                          start_date = NULL,
                          end_date = NULL,
                          nYR_init = 1)

  expect_equal(inherits(ldata, 'matrix'),T)
  expect_equal(ncol(ldata), nrow(parcels))

})
