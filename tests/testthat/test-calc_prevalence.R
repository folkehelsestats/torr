# tests/testthat/test-calc_prevalence.R

testthat::test_that("annual weighted prevalence matches weighted mean", {
  dat <- data.table::data.table(
    year = rep(2025, 10),
    ltp_any = c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    anypop = c(rep(1, 9),0),
    kjonn = c(1, 1, 2, 2, 1, 2, 1, 1, 2, 2),
    agecat = factor(
      c("25-34", "35-64", "18-24", "35-64", "35-64",
        "35-64", "25-34", "25-34", "25-34", "35-64")
    ),
    vekt2 = c(
      646.9867, 792.1924, 910.7136, 517.3095, 898.7392,
      578.6778, 766.8745, 916.0997, 691.0792, 754.5794
    )
  )

  result <- torr::calc_prevalence(
    data = dat,
    denominator = "anypop",
    year_var = "year",
    outcome_var = "ltp_any",
    weight_var = "vekt2",
    rolling_year = 1
  )

  expected <- round(with(dat, sum(ltp_any * vekt2) / sum(anypop * vekt2))*100, 1)

  testthat::expect_equal(result$rolling_period, "2025-2025")
  testthat::expect_equal(result$ltp_any, expected)
  testthat::expect_true(all(c("ci_l", "ci_u") %in% names(result)))
  testthat::expect_true(result$ci_l <= result$ltp_any)
  testthat::expect_true(result$ci_u >= result$ltp_any)
})


testthat::test_that("the supplied 2025 data give the expected weighted prevalence", {
  # This is the full 50-row dataset supplied in the issue.
  dat <- data.table::data.table(
    year = rep(2025, 50),
    ltp_any = c(
      0,1,1,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,0,
      1,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,1,0,0,1,
      1,0,0,1,0,0,0,0,0,0
    ),
    anypop = c(rep(1, 48), rep(0,2)),
    kjonn = c(
      1,1,2,2,1,2,1,1,2,2,1,2,2,2,1,1,1,2,2,2,
      2,1,1,1,1,2,2,1,2,2,2,1,1,2,2,1,1,2,1,1,
      1,1,2,1,2,2,1,2,1,2
    ),
    agecat = factor(c(
      "25-34","35-64","18-24","35-64","35-64","35-64","25-34","25-34",
      "25-34","35-64","35-64","16-17","16-17","35-64","16-17","35-64",
      "25-34","25-34","25-34","18-24","18-24","25-34","35-64","35-64",
      "25-34","35-64","18-24","16-17","25-34","35-64","18-24","25-34",
      "35-64","25-34","35-64","16-17","35-64","25-34","18-24","35-64",
      "35-64","35-64","35-64","25-34","35-64","35-64","25-34","18-24",
      "25-34","18-24"
    )),
    vekt2 = c(
      646.9867,792.1924,910.7136,517.3095,898.7392,578.6778,766.8745,
      916.0997,691.0792,754.5794,478.7647,542.6031,542.6031,1138.3498,
      3361.8713,747.8778,766.8745,691.0792,691.0792,791.6530,639.3345,
      728.4380,875.9613,3239.6847,728.4380,3196.7781,639.3345,510.3996,
      609.6278,986.0312,557.8831,766.8745,560.2161,691.0792,654.6557,
      591.8509,1063.5715,833.8989,451.7261,778.8513,560.2161,1085.3505,
      654.6557,728.4380,626.0200,3305.4887,999.8171,1151.6642,1088.4492,
      710.2017
    )
  )

  result <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", "vekt2", rolling_year = 1
  )

  # Weighted prevalence calculated directly from the supplied rows.
  expected <- round(with(dat, sum(ltp_any * vekt2) / sum(anypop * vekt2))*100,1)

  testthat::expect_equal(result$ltp_any, expected)
  testthat::expect_equal(result$ltp_any, 26.2)
})


testthat::test_that("by supports gender, age group, and both together", {
  dat <- data.table::data.table(
    year = rep(2025, 8),
    ltp_any = c(0, 1, 1, 0, 0, 1, 1, 0),
    anypop = rep(1, 8),
    kjonn = c(1, 1, 2, 2, 1, 1, 2, 2),
    agecat = factor(c(
      "18-24", "18-24", "18-24", "18-24",
      "25-34", "25-34", "25-34", "25-34"
    )),
    vekt2 = c(1, 2, 1, 2, 1, 2, 1, 2)
  )

  by_gender <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", "vekt2",
    by = "kjonn", rolling_year = 1
  )

  by_age <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", "vekt2",
    by = "agecat", rolling_year = 1
  )

  by_both <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", "vekt2",
    by = c("kjonn", "agecat"), rolling_year = 1
  )

  testthat::expect_equal(nrow(by_gender), 2)
  testthat::expect_equal(nrow(by_age), 2)
  testthat::expect_equal(nrow(by_both), 4)

  # Check one domain against a direct weighted mean.
  expected_male <- round(with(dat[kjonn == 1, ], sum(ltp_any * vekt2) / sum(vekt2))*100,1)
  observed_male <- by_gender$ltp_any[by_gender$kjonn == 1]

  testthat::expect_equal(observed_male, expected_male)
})


testthat::test_that("denominator excludes ineligible observations before estimation", {
  dat <- data.table::data.table(
    year = rep(2025, 4),
    ltp_any = c(0, 1, 1, 1),
    anypop = c(1, 1, 0, 0),
    vekt2 = c(1, 1, 100, 100)
  )

  result <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", "vekt2", rolling_year = 1
  )

  testthat::expect_equal(result$ltp_any, 50)
})


testthat::test_that("NULL weight_var produces an unweighted prevalence", {
  dat <- data.table::data.table(
    year = rep(2025, 4),
    ltp_any = c(0, 1, 0, 1),
    anypop = rep(1, 4)
  )

  result <- torr::calc_prevalence(
    dat, "anypop", "year", "ltp_any", weight_var = NULL,
    rolling_year = 1
  )

  testthat::expect_equal(result$ltp_any, 50)
})


testthat::test_that("three-year rolling periods use consecutive calendar years", {
  dat <- data.table::data.table(
    year = c(2023, 2023, 2024, 2024, 2025, 2025),
    outcome = c(0, 1, 1, 0, 1, 1),
    eligible = rep(1, 6),
    weight = c(1, 3, 2, 2, 4, 1)
  )

  result <- torr::calc_prevalence(
    dat, "eligible", "year", "outcome", "weight",
    rolling_year = 3
  )

  expected <- with(dat, sum(outcome * weight) / sum(weight))

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$rolling_period, "2023-2025")
  testthat::expect_equal(result$outcome, round(expected * 100, 1))
  testthat::expect_equal(result$outcome, 76.9)
})


testthat::test_that("rolling periods overlap correctly", {
  dat <- data.table::data.table(
    year = rep(2020:2024, each = 2),
    outcome = c(0,1, 1,0, 0,0, 1,1, 1,0),
    eligible = rep(1, 10),
    weight = rep(1, 10)
  )

  result <- torr::calc_prevalence(
    dat, "eligible", "year", "outcome", "weight",
    rolling_year = 3
  )

  testthat::expect_equal(
    result$rolling_period,
    c("2020-2022", "2021-2023", "2022-2024")
  )
  testthat::expect_equal(nrow(result), 3)
})


testthat::test_that("missing calendar years are not silently bridged", {
  dat <- data.table::data.table(
    year = c(2020, 2020, 2022, 2022, 2023, 2023),
    outcome = c(0, 1, 1, 0, 1, 1),
    eligible = rep(1, 6),
    weight = rep(1, 6)
  )

  result <- torr::calc_prevalence(
    dat, "eligible", "year", "outcome", "weight",
    rolling_year = 2
  )

  # 2020-2021 is incomplete; 2021-2022 is incomplete;
  # only 2022-2023 is a complete two-calendar-year period.
  testthat::expect_equal(result$rolling_period, "2022-2023")
})


testthat::test_that("invalid arguments are rejected", {
  dat <- data.table::data.table(
    year = 2025,
    outcome = 0,
    eligible = 1,
    weight = 1
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome",
                          "weight", rolling_year = 0),
    "positive integer"
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome",
                          "weight", ci_level = 1),
    "between 0 and 1"
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome",
                          "weight", rolling_year = 2),
    "unique year"
  )
})


testthat::test_that("invalid denominator and outcome values are rejected", {
  dat <- data.table::data.table(
    year = c(2025, 2025),
    outcome = c(0, 2),
    eligible = c(1, 2),
    weight = c(1, 1)
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome", "weight"),
    "denominator.*0/1"
  )

  dat$eligible <- 1

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome", "weight"),
    "outcome.*0/1"
  )
})


testthat::test_that("missing denominator values are rejected", {
  dat <- data.table::data.table(
    year = c(2025, 2025),
    outcome = c(0, 1),
    eligible = c(1, NA),
    weight = c(1, 1)
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome", "weight"),
    "denominator.*missing"
  )
})


testthat::test_that("invalid weights are rejected", {
  dat <- data.table::data.table(
    year = c(2025, 2025),
    outcome = c(0, 1),
    eligible = c(1, 1),
    weight = c(1, 0)
  )

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome", "weight"),
    "positive"
  )

  dat$weight <- c(1, NA)

  testthat::expect_error(
    torr::calc_prevalence(dat, "eligible", "year", "outcome", "weight"),
    "missing"
  )
})


testthat::test_that("missing outcome values are removed after denominator filtering", {
  dat <- data.table::data.table(
    year = c(2025, 2025, 2025),
    outcome = c(0, 1, NA),
    eligible = c(1, 1, 0),
    weight = c(1, 1, 100)
  )

  result <- torr::calc_prevalence(
    dat, "eligible", "year", "outcome", "weight",
    rolling_year = 1
  )

#   testthat::expect_equal(result$outcome, 0.5, tolerance = 1e-10)
  testthat::expect_equal(result$outcome, 50)
})


testthat::test_that("PSU and strata can be supplied", {
  dat <- data.table::data.table(
    year = rep(2025, 8),
    outcome = c(0, 1, 1, 0, 0, 1, 1, 0),
    eligible = rep(1, 8),
    weight = rep(1, 8),
    psu = c(1, 1, 2, 2, 3, 3, 4, 4),
    strata = c(1, 1, 1, 1, 2, 2, 2, 2)
  )

  result <- torr::calc_prevalence(
    dat, "eligible", "year", "outcome", "weight",
    psu = "psu", strata = "strata", rolling_year = 1
  )

  testthat::expect_s3_class(result, c("data.table", "data.frame"))
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true(is.finite(result$outcome))
})
