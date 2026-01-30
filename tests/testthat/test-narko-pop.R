library(testthat)
library(data.table)

test_that("create_narko_pop creates correct LTP population indicator", {
  # Create test data
  dt <- data.table(
    id = 1:5,
    ltp_var = c(1, 2, NA, 3, NA),
    ans1 = c(1, 1, 2, 1, 2)
  )

  result <- create_narko_pop(
    d = dt,
    types = "ltp",
    vars = "ltp_var",
    val = "test"
  )

  # Check that new column was created
  expect_true("ltpPop_test" %in% names(result))

  # Check values
  expect_equal(result$ltpPop_test, c(1L, 1L, 1L, 0L, 1L))
})

test_that("create_narko_pop creates correct LYP population indicator", {
  # Create test data
  dt <- data.table(
    id = 1:5,
    ltp_var = c(1, 2, NA, 3, NA),
    lyp_var = c(1, NA, 2, NA, NA),
    ans1 = c(1, 1, 2, 1, 2)
  )

  result <- create_narko_pop(
    d = dt,
    types = c("ltp", "lyp"),
    vars = c("ltp_var", "lyp_var"),
    val = "test"
  )

  # Check that new columns were created
  expect_true("lypPop_test" %in% names(result))
  expect_true("ltpPop_test" %in% names(result))

  # Check LYP values
  expect_equal(result$lypPop_test, c(1L, 1L, 1L, 0L, 1L))
})

test_that("create_narko_pop works with both ltp and lyp", {
  dt <- data.table(
    ltp_var = c(1, 2, NA, 3),
    lyp_var = c(1, NA, 2, NA),
    ans1 = c(1, 1, 2, 1)
  )

  result <- create_narko_pop(
    d = dt,
    types = c("ltp", "lyp"),
    vars = c("ltp_var", "lyp_var"),
    val = "2024"
  )

  expect_true(all(c("ltpPop_2024", "lypPop_2024") %in% names(result)))
})

test_that("create_narko_pop doesn't modify original data.table", {
  dt <- data.table(
    ltp_var = c(1, 2, NA),
    ans1 = c(1, 1, 2)
  )

  original_names <- names(dt)

  result <- create_narko_pop(
    d = dt,
    types = "ltp",
    vars = "ltp_var",
    val = "test"
  )

  # Original should be unchanged
  expect_equal(names(dt), original_names)
  # Result should have new column
  expect_true("ltpPop_test" %in% names(result))
})

test_that("create_narko_pop works with custom ans column", {
  dt <- data.table(
    ltp_var = c(1, NA, 3),
    custom_ans = c(1, 2, 1)
  )

  result <- create_narko_pop(
    d = dt,
    types = "ltp",
    vars = "ltp_var",
    val = "test",
    ans = "custom_ans"
  )

  expect_equal(result$ltpPop_test, c(1L, 1L, 0L))
})

test_that("create_narko_pop handles edge cases correctly", {
  dt <- data.table(
    ltp_var = c(1, 2, 0, NA, NA, 3),
    lyp_var = c(NA, 1, 2, NA, NA, 0),
    ans1 = c(1, 1, 1, 2, 1, 2)
  )

  result <- create_narko_pop(
    d = dt,
    types = c("ltp", "lyp"),
    vars = c("ltp_var", "lyp_var"),
    val = "edge"
  )

  # Row 1: ltp=1, lyp=NA, ltp=1 -> lyp should be 0 (because ltp != 2)
  expect_equal(result$lypPop_edge[1], 0L)

  # Row 4: ltp=NA, lyp=NA, ans1=2 -> both should be 1
  expect_equal(result$ltpPop_edge[4], 1L)
  expect_equal(result$lypPop_edge[4], 1L)
})

test_that("create_narko_pop validates input correctly", {
  dt <- data.table(ltp_var = 1:3, ans1 = 1:3)

  # Invalid types
  expect_error(
    create_narko_pop(dt, types = "invalid", vars = "ltp_var", val = "test"),
    "must contain only 'ltp' and/or 'lyp'"
  )

  # Mismatched vars and types length
  expect_error(
    create_narko_pop(dt, types = c("ltp", "lyp"), vars = "ltp_var", val = "test"),
    "Length of 'vars' must equal length of 'types'"
  )

  # Missing ans column
  expect_error(
    create_narko_pop(dt, types = "ltp", vars = "ltp_var", val = "test", ans = "missing"),
    "Column 'missing' not found"
  )

  # Missing variable column
  expect_error(
    create_narko_pop(dt, types = "ltp", vars = "missing_var", val = "test"),
    "Column 'missing_var' not found"
  )

  # Invalid input type
  expect_error(
    create_narko_pop(list(a = 1), types = "ltp", vars = "ltp_var", val = "test"),
    "must be a data.table or data.frame"
  )
})

test_that("create_narko_pop works with data.frame input", {
  df <- data.frame(
    ltp_var = c(1, 2, NA),
    ans1 = c(1, 1, 2)
  )

  result <- create_narko_pop(
    d = df,
    types = "ltp",
    vars = "ltp_var",
    val = "test"
  )

  expect_s3_class(result, "data.table")
  expect_true("ltpPop_test" %in% names(result))
})
