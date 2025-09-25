test_that("albopictus", {

  gdata <- build_gdata(
    vector = "Ae. albopictus (D)")

    expect_type(gdata, "list")
    expect_length(gdata, 38)

    gdata <- build_gdata(
      vector = "Ae. albopictus")

    expect_true(inherits(gdata, "list"))
    expect_length(gdata, 36)


    gdata <- build_gdata(
      vector = "Ae. albopictus",
      virus = 'CHI')

    expect_true(inherits(gdata, "list"))
    expect_length(gdata, 36)

    gdata <- build_gdata(
      vector = "Ae. albopictus",
      virus = 'ZIK')

    expect_true(inherits(gdata, "list"))
    expect_length(gdata, 36)
  })

test_that("aegypti", {

  gdata <- build_gdata(
    vector = "Ae. aegypti")

  expect_true(inherits(gdata, "list"))
  expect_length(gdata, 50)

  gdata <- build_gdata(
    vector = "Ae. aegypti",
    virus = 'CHI')

  expect_true(inherits(gdata, "list"))
  expect_length(gdata, 50)

  gdata <- build_gdata(
    vector = "Ae. aegypti",
    virus = 'ZIK')

  expect_true(inherits(gdata, "list"))
  expect_length(gdata, 50)

})
