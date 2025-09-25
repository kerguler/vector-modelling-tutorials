test_that("estim_K", {

  ## LOAD STUDY AREA SPATIAL DIVISION

  PARCELS_SHAPE <- system.file("shape/SpatVec.shp", package = "arbocartoR") %>% terra::vect(.)

  ## LOAD URBAN ATLAS

  PARCELS_SHAPE_estimated <- estim_K(PARCELS_SHAPE)

  expect_true(inherits(PARCELS_SHAPE_estimated, "SpatVector"))
  expect_contains(names(PARCELS_SHAPE_estimated), c("Kfix", "Kvar"))

})
