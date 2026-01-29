library(testthat)
library(data.table)

# Helper function to create test data
create_test_data <- function() {
  data.table(
    ans1 = c(1, 2, 3, 1, 2, 1, NA, 2),
    ans2_cannabis = c(1, 2, 8, NA, 1, 2, 9, NA),
    ans2_cocaine = c(2, NA, 9, 1, NA, 1, 2, 8),
    ans3_cannabis = c(1, 2, NA, 1, 2, NA, 1, 2),
    ans3_cocaine = c(NA, 1, 2, NA, 1, 2, NA, NA)
  )
}

test_that("create_narko_pop returns correct structure", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Check that result is a data.table
  expect_true(is.data.table(result))

  # Check that new columns are created
  expect_true("ltpPop_cannabis" %in% names(result))
  expect_true("ltpPop_cocaine" %in% names(result))
  expect_true("lypPop_cannabis" %in% names(result))
  expect_true("lypPop_cocaine" %in% names(result))

  # Check that original columns are preserved
  expect_true("ans1" %in% names(result))
  expect_true("ans2_cannabis" %in% names(result))

  # Check that result has correct number of rows
  expect_equal(nrow(result), nrow(dt))
})

test_that("create_narko_pop correctly identifies LTP population", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Row 1: ans1=1, ans2_cannabis=1 -> ltpPop=1 (answered 1)
  expect_equal(result$ltpPop_cannabis[1], 1)

  # Row 2: ans1=2, ans2_cannabis=2 -> ltpPop=1 (answered 2)
  expect_equal(result$ltpPop_cannabis[2], 1)

  # Row 3: ans1=3, ans2_cannabis=8 -> ltpPop=0 (not 1 or 2, ans1 not 2)
  expect_equal(result$ltpPop_cannabis[3], 0)

  # Row 4: ans1=1, ans2_cannabis=NA -> ltpPop=0 (NA but ans1 is not 2)
  expect_equal(result$ltpPop_cannabis[4], 0)

  # Row 5: ans1=2, ans2_cannabis=1 -> ltpPop=1 (answered 1)
  expect_equal(result$ltpPop_cannabis[5], 1)

  # Row 8: ans1=2, ans2_cannabis=NA -> ltpPop=1 (NA and ans1=2)
  expect_equal(result$ltpPop_cannabis[8], 1)
})

test_that("create_narko_pop correctly identifies LYP population", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Row 1: ans1=1, ans3_cannabis=1 -> lypPop=1 (answered 1)
  expect_equal(result$lypPop_cannabis[1], 1)

  # Row 2: ans1=2, ans3_cannabis=2 -> lypPop=1 (answered 2)
  expect_equal(result$lypPop_cannabis[2], 1)

  # Row 3: ans1=3, ans3_cannabis=NA -> lypPop=0 (NA but ans1 is not 2)
  expect_equal(result$lypPop_cannabis[3], 0)

  # Row 6: ans1=1, ans3_cannabis=NA -> lypPop=0 (NA but ans1 is not 2)
  expect_equal(result$lypPop_cannabis[6], 0)

  # Row 8: ans1=2, ans3_cannabis=2 -> lypPop=1 (answered 2)
  expect_equal(result$lypPop_cannabis[8], 1)
})

test_that("create_narko_pop handles cocaine columns correctly", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Row 1: ans1=1, ans2_cocaine=2 -> ltpPop=1
  expect_equal(result$ltpPop_cocaine[1], 1)

  # Row 2: ans1=2, ans2_cocaine=NA -> ltpPop=1 (NA and ans1=2)
  expect_equal(result$ltpPop_cocaine[2], 1)

  # Row 2: ans1=2, ans3_cocaine=1 -> lypPop=1
  expect_equal(result$lypPop_cocaine[2], 1)

  # Row 8: ans1=2, ans3_cocaine=NA -> lypPop=1 (NA and ans1=2)
  expect_equal(result$lypPop_cocaine[8], 1)
})

test_that("create_narko_pop does not modify original data", {
  dt <- create_test_data()
  original_ncol <- ncol(dt)
  original_names <- names(dt)

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Check original data is unchanged
  expect_equal(ncol(dt), original_ncol)
  expect_equal(names(dt), original_names)

  # Check that result is different from original
  expect_true(ncol(result) > ncol(dt))
})

test_that("create_narko_pop throws error for non-data.table input", {
  df <- data.frame(
    ans1 = c(1, 2),
    ans2_cannabis = c(1, 2),
    ans3_cannabis = c(1, 2)
  )

  expect_error(
    create_narko_pop(
      data = df,
      ltp = "ans2_cannabis",
      lyp = "ans3_cannabis",
      narkvars = "cannabis"
    ),
    "'data' must be a data.table"
  )
})

test_that("create_narko_pop throws error when ans1 column is missing", {
  dt <- data.table(
    ans2_cannabis = c(1, 2),
    ans3_cannabis = c(1, 2)
  )

  expect_error(
    create_narko_pop(
      data = dt,
      ltp = "ans2_cannabis",
      lyp = "ans3_cannabis",
      narkvars = "cannabis"
    ),
    "'data' must contain 'ans1' column"
  )
})

test_that("create_narko_pop throws error for mismatched vector lengths", {
  dt <- create_test_data()

  # More ltp variables than narkvars
  expect_error(
    create_narko_pop(
      data = dt,
      ltp = c("ans2_cannabis", "ans2_cocaine"),
      lyp = c("ans3_cannabis"),
      narkvars = c("cannabis", "cocaine")
    ),
    "'ltp', 'lyp', and 'narkvars' must have the same length"
  )

  # More narkvars than ltp variables
  expect_error(
    create_narko_pop(
      data = dt,
      ltp = c("ans2_cannabis"),
      lyp = c("ans3_cannabis"),
      narkvars = c("cannabis", "cocaine")
    ),
    "'ltp', 'lyp', and 'narkvars' must have the same length"
  )
})

test_that("create_narko_pop throws error for missing columns", {
  dt <- create_test_data()

  # Missing LTP column
  expect_error(
    create_narko_pop(
      data = dt,
      ltp = c("ans2_cannabis", "ans2_heroin"),
      lyp = c("ans3_cannabis", "ans3_cocaine"),
      narkvars = c("cannabis", "heroin")
    ),
    "LTP columns not found in data: ans2_heroin"
  )

  # Missing LYP column
  expect_error(
    create_narko_pop(
      data = dt,
      ltp = c("ans2_cannabis", "ans2_cocaine"),
      lyp = c("ans3_cannabis", "ans3_heroin"),
      narkvars = c("cannabis", "heroin")
    ),
    "LYP columns not found in data: ans3_heroin"
  )
})

test_that("create_narko_pop works with single substance", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = "ans2_cannabis",
    lyp = "ans3_cannabis",
    narkvars = "cannabis"
  )

  expect_true("ltpPop_cannabis" %in% names(result))
  expect_true("lypPop_cannabis" %in% names(result))
  expect_false("ltpPop_cocaine" %in% names(result))
  expect_false("lypPop_cocaine" %in% names(result))
})

test_that("create_narko_pop produces binary outputs only", {
  dt <- create_test_data()

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  # Check all population columns are 0 or 1
  expect_true(all(result$ltpPop_cannabis %in% c(0, 1)))
  expect_true(all(result$ltpPop_cocaine %in% c(0, 1)))
  expect_true(all(result$lypPop_cannabis %in% c(0, 1)))
  expect_true(all(result$lypPop_cocaine %in% c(0, 1)))
})

test_that("create_narko_pop handles all NA values correctly", {
  dt <- data.table(
    ans1 = c(1, 2, 8),
    ans2_cannabis = c(NA, NA, NA),
    ans3_cannabis = c(NA, NA, NA)
  )

  result <- create_narko_pop(
    data = dt,
    ltp = "ans2_cannabis",
    lyp = "ans3_cannabis",
    narkvars = "cannabis"
  )

  # Row 1: ans1=1, ans2=NA -> ltpPop=0
  expect_equal(result$ltpPop_cannabis[1], 0)

  # Row 2: ans1=2, ans2=NA -> ltpPop=1
  expect_equal(result$ltpPop_cannabis[2], 1)

  # Row 3: ans1=3, ans2=NA -> ltpPop=0
  expect_equal(result$ltpPop_cannabis[3], 0)
})

test_that("create_narko_pop preserves row order", {
  dt <- create_test_data()
  dt$id <- 1:nrow(dt)

  result <- create_narko_pop(
    data = dt,
    ltp = c("ans2_cannabis", "ans2_cocaine"),
    lyp = c("ans3_cannabis", "ans3_cocaine"),
    narkvars = c("cannabis", "cocaine")
  )

  expect_equal(result$id, dt$id)
  expect_equal(result$ans1, dt$ans1)
})

test_that("create_narko_pop handles NA in ans2 and ans3 correctly", {
  dt <- data.table(
    ans1 = c(1, 2, 1, 8),
    ans2_cannabis = c(1, 2, NA, NA),
    ans3_cannabis = c(1, NA, 2, NA)
  )

  result <- create_narko_pop(
    data = dt,
    ltp = "ans2_cannabis",
    lyp = "ans3_cannabis",
    narkvars = "cannabis"
  )

  # When ans1 is NA, should be treated as not in population (0)
  # Row 3: ans1=NA, ans2_cannabis=2 -> ltpPop=0 (ans1 is not 2)
  expect_equal(result$ltpPop_cannabis[3], 0)

  # Row 4: ans1=NA, ans2_cannabis=NA -> ltpPop=0 (ans1 is not 2)
  expect_equal(result$ltpPop_cannabis[4], 0)

  expect_equal(result$lypPop_cannabis[3], 1)
  expect_equal(result$lypPop_cannabis[4], 0)

  # All values should be 0 or 1, no NAs
  expect_true(all(result$ltpPop_cannabis %in% c(0, 1)))
  expect_true(all(result$lypPop_cannabis %in% c(0, 1)))
})
