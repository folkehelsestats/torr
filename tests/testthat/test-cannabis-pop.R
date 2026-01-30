library(testthat)
library(data.table)

# ==============================================================================
# Basic functionality tests
# ==============================================================================

test_that("create_cann_pop creates all three columns with default types", {
  dt <- data.table(
    id = 1:3,
    lifetime = c(1, 2, 3),
    lastyear = c(1, 2, 3),
    lastmonth = c(1, 2, 3)
  )

  result <- create_cann_pop(dt, vars = c("lifetime", "lastyear", "lastmonth"))

  expect_true("ltpPop_cannabis" %in% names(result))
  expect_true("lypPop_cannabis" %in% names(result))
  expect_true("lmpPop_cannabis" %in% names(result))
})

test_that("create_cann_pop creates only ltp when types = 'ltp'", {
  dt <- data.table(
    lifetime = c(1, 2, 3)
  )

  result <- create_cann_pop(dt, types = "ltp", vars = "lifetime")

  expect_true("ltpPop_cannabis" %in% names(result))
  expect_false("lypPop_cannabis" %in% names(result))
  expect_false("lmpPop_cannabis" %in% names(result))
})

test_that("create_cann_pop creates ltp and lyp when types = c('ltp', 'lyp')", {
  dt <- data.table(
    lifetime = c(1, 2, 3),
    lastyear = c(1, 2, 3)
  )

  result <- create_cann_pop(dt, types = c("ltp", "lyp"), vars = c("lifetime", "lastyear"))

  expect_true("ltpPop_cannabis" %in% names(result))
  expect_true("lypPop_cannabis" %in% names(result))
  expect_false("lmpPop_cannabis" %in% names(result))
})

test_that("create_cann_pop works with custom value parameter", {
  dt <- data.table(
    lifetime = c(1, 2, 3)
  )

  result <- create_cann_pop(dt, types = "ltp", vars = "lifetime", value = "alcohol")

  expect_true("ltpPop_alcohol" %in% names(result))
  expect_false("ltpPop_cannabis" %in% names(result))
})

# ==============================================================================
# Calculation logic tests
# ==============================================================================

test_that("create_cann_pop calculates ltp correctly", {
  dt <- data.table(
    lifetime = c(1, 2, 3, 4, NA)
  )

  result <- create_cann_pop(dt, types = "ltp", vars = "lifetime")

  expect_equal(result$ltpPop_cannabis, c(1, 1, 0, 0, 0))
})

test_that("create_cann_pop calculates lyp correctly with cascading", {
  dt <- data.table(
    lifetime = c(1, 2, 3, 2, 1),
    lastyear = c(1, 2, 3, 4, 3)
  )

  result <- create_cann_pop(dt, types = c("ltp", "lyp"), vars = c("lifetime", "lastyear"))

  # lyp should be 1 if: lastyear in 1:2 OR lifetime == 2
  expect_equal(result$lypPop_cannabis, c(1, 1, 0, 1, 0))
})

test_that("create_cann_pop calculates lmp correctly with cascading", {
  dt <- data.table(
    lifetime = c(1, 2, 3, 2, 1, 3),
    lastyear = c(1, 1, 2, 3, 3, 3),
    lastmonth = c(1, 2, 3, 4, 3, 3)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  # lmp should be 1 if: lastmonth in 1:2 OR lastyear == 2 OR lifetime == 2
  expect_equal(result$lmpPop_cannabis, c(1, 1, 1, 1, 0, 0))
})

test_that("create_cann_pop handles NA values correctly", {
  dt <- data.table(
    lifetime = c(1, NA, 2, NA),
    lastyear = c(NA, 1, NA, 2),
    lastmonth = c(1, NA, 2, NA)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(result$ltpPop_cannabis, c(1, 0, 1, 0))
  expect_equal(result$lypPop_cannabis, c(0, 1, 1, 1))
  expect_equal(result$lmpPop_cannabis, c(1, 0, 1, 1))
})

# ==============================================================================
# Input validation tests
# ==============================================================================

test_that("create_cann_pop validates dt is data.table or data.frame", {
  expect_error(
    create_cann_pop(list(a = 1), types = "ltp", vars = "a"),
    "dt must be a data.table or data.frame"
  )

  expect_error(
    create_cann_pop("not a dataframe", types = "ltp", vars = "a"),
    "dt must be a data.table or data.frame"
  )
})

test_that("create_cann_pop validates types is non-empty character vector", {
  dt <- data.table(a = 1)

  expect_error(
    create_cann_pop(dt, types = character(0), vars = "a"),
    "types must be a non-empty character vector"
  )

  expect_error(
    create_cann_pop(dt, types = 123, vars = "a"),
    "types must be a non-empty character vector"
  )
})

test_that("create_cann_pop validates types contains only valid values", {
  dt <- data.table(a = 1)

  expect_error(
    create_cann_pop(dt, types = "invalid", vars = "a"),
    "Invalid types: invalid"
  )

  expect_error(
    create_cann_pop(dt, types = c("ltp", "xyz", "abc"), vars = c("a", "b", "c")),
    "Invalid types: xyz, abc"
  )
})

test_that("create_cann_pop rejects duplicate types", {
  dt <- data.table(a = 1, b = 2)

  expect_error(
    create_cann_pop(dt, types = c("ltp", "ltp"), vars = c("a", "b")),
    "types cannot contain duplicate values"
  )
})

test_that("create_cann_pop validates types and vars have same length", {
  dt <- data.table(a = 1, b = 2, c = 3)

  expect_error(
    create_cann_pop(dt, types = c("ltp", "lyp"), vars = "a"),
    "Length of types \\(2\\) must match length of vars \\(1\\)"
  )

  expect_error(
    create_cann_pop(dt, types = "ltp", vars = c("a", "b")),
    "Length of types \\(1\\) must match length of vars \\(2\\)"
  )
})

test_that("create_cann_pop validates cascading requirements for lyp", {
  dt <- data.table(a = 1, b = 2)

  expect_error(
    create_cann_pop(dt, types = "lyp", vars = "b"),
    "To create 'lyp' indicator, 'ltp' must also be in types"
  )
})

test_that("create_cann_pop validates cascading requirements for lmp", {
  dt <- data.table(a = 1, b = 2, c = 3)

  expect_error(
    create_cann_pop(dt, types = "lmp", vars = "c"),
    "To create 'lmp' indicator, both 'ltp' and 'lyp' must be in types"
  )

  expect_error(
    create_cann_pop(dt, types = c("ltp", "lmp"), vars = c("a", "c")),
    "To create 'lmp' indicator, both 'ltp' and 'lyp' must be in types"
  )
})

test_that("create_cann_pop validates types are in correct order", {
  dt <- data.table(a = 1, b = 2, c = 3)

  expect_error(
    create_cann_pop(dt, types = c("lyp", "ltp"), vars = c("b", "a")),
    "types must be in order: 'ltp', 'lyp', 'lmp'"
  )

  expect_error(
    create_cann_pop(dt, types = c("lmp", "lyp", "ltp"), vars = c("c", "b", "a")),
    "types must be in order: 'ltp', 'lyp', 'lmp'"
  )

  expect_error(
    create_cann_pop(dt, types = c("ltp", "lmp", "lyp"), vars = c("a", "c", "b")),
    "types must be in order: 'ltp', 'lyp', 'lmp'"
  )
})

test_that("create_cann_pop validates variables exist in dt", {
  dt <- data.table(a = 1, b = 2)

  expect_error(
    create_cann_pop(dt, types = "ltp", vars = "c"),
    "Variables not found in dt: c"
  )

  expect_error(
    create_cann_pop(dt, types = c("ltp", "lyp"), vars = c("a", "z")),
    "Variables not found in dt: z"
  )

  expect_error(
    create_cann_pop(dt, types = c("ltp", "lyp"), vars = c("x", "y")),
    "Variables not found in dt: x, y"
  )
})

test_that("create_cann_pop validates value parameter", {
  dt <- data.table(a = 1)

  expect_error(
    create_cann_pop(dt, types = "ltp", vars = "a", value = ""),
    "value must be a non-empty character string of length 1"
  )

  expect_error(
    create_cann_pop(dt, types = "ltp", vars = "a", value = c("a", "b")),
    "value must be a non-empty character string of length 1"
  )

  expect_error(
    create_cann_pop(dt, types = "ltp", vars = "a", value = 123),
    "value must be a non-empty character string of length 1"
  )
})

# ==============================================================================
# Data integrity tests
# ==============================================================================

test_that("create_cann_pop doesn't modify original data.table", {
  dt <- data.table(
    lifetime = c(1, 2, 3),
    lastyear = c(1, 2, 3),
    lastmonth = c(1, 2, 3)
  )

  original_cols <- names(dt)
  original_rows <- nrow(dt)

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(names(dt), original_cols)
  expect_equal(nrow(dt), original_rows)
  expect_true(length(names(result)) > length(names(dt)))
})

test_that("create_cann_pop works with data.frame input", {
  df <- data.frame(
    lifetime = c(1, 2, 3),
    lastyear = c(1, 2, 3),
    lastmonth = c(1, 2, 3)
  )

  result <- create_cann_pop(df,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_s3_class(result, "data.table")
  expect_true(all(c("ltpPop_cannabis", "lypPop_cannabis", "lmpPop_cannabis") %in% names(result)))
})

test_that("create_cann_pop preserves all original columns", {
  dt <- data.table(
    id = 1:3,
    lifetime = c(1, 2, 3),
    lastyear = c(1, 2, 3),
    lastmonth = c(1, 2, 3),
    extra_col = c("a", "b", "c")
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_true(all(c("id", "lifetime", "lastyear", "lastmonth", "extra_col") %in% names(result)))
})

test_that("create_cann_pop returns correct number of rows", {
  dt <- data.table(
    lifetime = 1:100,
    lastyear = 1:100,
    lastmonth = 1:100
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(nrow(result), nrow(dt))
})

# ==============================================================================
# Edge cases and special scenarios
# ==============================================================================

test_that("create_cann_pop handles all zeros", {
  dt <- data.table(
    lifetime = c(3, 4, 5),
    lastyear = c(3, 4, 5),
    lastmonth = c(3, 4, 5)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(result$ltpPop_cannabis, c(0, 0, 0))
  expect_equal(result$lypPop_cannabis, c(0, 0, 0))
  expect_equal(result$lmpPop_cannabis, c(0, 0, 0))
})

test_that("create_cann_pop handles all ones", {
  dt <- data.table(
    lifetime = c(1, 1, 1),
    lastyear = c(1, 1, 1),
    lastmonth = c(1, 1, 1)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(result$ltpPop_cannabis, c(1, 1, 1))
  expect_equal(result$lypPop_cannabis, c(1, 1, 1))
  expect_equal(result$lmpPop_cannabis, c(1, 1, 1))
})

test_that("create_cann_pop handles boundary values (1 and 2)", {
  dt <- data.table(
    lifetime = c(1, 2, 1, 2),
    lastyear = c(1, 2, 2, 1),
    lastmonth = c(1, 2, 2, 1)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("lifetime", "lastyear", "lastmonth"))

  expect_equal(result$ltpPop_cannabis, c(1, 1, 1, 1))
  expect_equal(result$lypPop_cannabis, c(1, 1, 1, 1))
  expect_equal(result$lmpPop_cannabis, c(1, 1, 1, 1))
})

test_that("create_cann_pop works with different variable names", {
  dt <- data.table(
    var_lifetime_cannabis = c(1, 2, 3),
    var_year_cannabis = c(1, 2, 3),
    var_month_cannabis = c(1, 2, 3)
  )

  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp", "lmp"),
                           vars = c("var_lifetime_cannabis", "var_year_cannabis", "var_month_cannabis"))

  expect_true(all(c("ltpPop_cannabis", "lypPop_cannabis", "lmpPop_cannabis") %in% names(result)))
})

test_that("create_cann_pop creates integer outputs for efficiency", {
  dt <- data.table(
    lifetime = c(1, 2, 3)
  )

  result <- create_cann_pop(dt, types = "ltp", vars = "lifetime")

  expect_type(result$ltpPop_cannabis, "integer")
})

# ==============================================================================
# Multiple substance scenarios
# ==============================================================================

test_that("create_cann_pop can be called multiple times for different substances", {
  dt <- data.table(
    cannabis_ltp = c(1, 2, 3),
    cannabis_lyp = c(1, 2, 3),
    alcohol_ltp = c(2, 3, 1),
    alcohol_lyp = c(2, 3, 1)
  )

  # Add cannabis indicators
  result <- create_cann_pop(dt,
                           types = c("ltp", "lyp"),
                           vars = c("cannabis_ltp", "cannabis_lyp"),
                           value = "cannabis")

  # Add alcohol indicators
  result <- create_cann_pop(result,
                           types = c("ltp", "lyp"),
                           vars = c("alcohol_ltp", "alcohol_lyp"),
                           value = "alcohol")

  expect_true(all(c("ltpPop_cannabis", "lypPop_cannabis",
                    "ltpPop_alcohol", "lypPop_alcohol") %in% names(result)))

  expect_equal(result$ltpPop_cannabis, c(1, 1, 0))
  expect_equal(result$ltpPop_alcohol, c(1, 0, 1))
})
