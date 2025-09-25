test_that("build movement matrix", {

  f <- system.file("shape/SpatVec.shp", package="arbocartoR")
  SpatVec <- terra::vect(f) %>% .[1:4,]

  for(law in c("GravExp", "NGravExp","GravPow","NGravPow", "Schneider","Rad", "RadExt", "Unif")){

    if(!law %in% c("GravExp", "GravPow"))
      param = NULL else param = 0.01

  mMov <- build_mMov(SpatVec,
                         law = law,
                         param = param,
                         p2move = 0.7,
                     outflow = NULL,
                     inflow = NULL,
                         verbose = F)

    expect_true(inherits(mMov, 'list'))
    expect_true(inherits(mMov$proba_ij, 'matrix'))
    expect_true(inherits(mMov$proba_ji, 'matrix'))
    expect_equal(dim(mMov$proba_ij), dim(mMov$proba_ji))

    expect_false(TRUE %in% is.na(mMov$proba_ij))
    expect_false(TRUE %in% is.na(mMov$proba_ji))

    expect_true(
      round(sum(mMov$proba_ij * SpatVec$POP)) == round(sum(mMov$proba_ji * mMov$mi_mixedpop))
    )
    }

})
