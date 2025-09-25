test_that("introduction", {

  E <- build_E_random(
    period_start = as.Date("2020/03/10"),
    period_end = as.Date("2020/09/30"),
    n_ind = NULL,
    n_events = 10,
    stage = "Eh",
    loc = LETTERS[1:3])

  E %>%
    expect_type("list") %>%
    expect_s3_class("data.frame") %>%
    expect_length(4)

    expect_contains(names(E),
                    c("time","dest","n","select"))

    expect_equal(nrow(E), 10)

    expect_s3_class(E$time, "Date")
    expect_s3_class(E$dest, "factor")
    expect_true(inherits(E$n, c("numeric", 'integer')))
    expect_s3_class(E$select, "factor")

    ##

    E <- build_E_random(
      period_start = as.Date("2020/03/10"),
      period_end = as.Date("2020/09/30"),
      n_ind = 50,
      n_events = NULL,
      stage = "Eh",
      loc = LETTERS[1:3])

    E %>%
      expect_type("list") %>%
      expect_s3_class("data.frame") %>%
      expect_length(4)

    expect_contains(names(E),
                    c("time","dest","n","select"))


    expect_equal(sum(E$n), 50)

    expect_s3_class(E$time, "Date")
    expect_s3_class(E$dest, "factor")
    expect_true(inherits(E$n, c("numeric", 'integer')))
    expect_s3_class(E$select, "factor")

    ###
    E <- build_E_random(
      period_start = as.Date("2020/03/10"),
      period_end = as.Date("2020/09/30"),
      n_ind = 50,
      n_events = 12,
      stage = "Eh",
      loc = LETTERS[1:3])

    E %>%
      expect_type("list") %>%
      expect_s3_class("data.frame") %>%
      expect_length(4)

    expect_contains(names(E),
                    c("time","dest","n","select"))


    expect_equal(nrow(E), 12)
    expect_equal(sum(E$n), 50)

    expect_s3_class(E$time, "Date")
    expect_s3_class(E$dest, "factor")
    expect_true(inherits(E$n, c("numeric", 'integer')))
    expect_s3_class(E$select, "factor")


    introduction_pts <- build_E_random(
      period_start <- "2020/03/10" %>% as.Date,
      period_end  <- "2020/09/30" %>% as.Date,
      n_ind = NULL,
      n_events = 5,
      stage = "Eh",
      loc = letters[1:5])

    expect_equal(dim(introduction_pts), c(5,4))
    expect_equal(names(introduction_pts), c("time", "dest", "n", "select"))

})
