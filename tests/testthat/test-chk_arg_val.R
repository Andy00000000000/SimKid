test_that("correct usage 1", {
  expect_no_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = NA))
})

test_that("correct usage 2", {
  expect_no_error(chk_arg_val(age0isbirth = TRUE, overlay_percentile = 0.90))
})

test_that("age0isbirth null", {
  expect_error(chk_arg_val(age0isbirth = NULL, overlay_percentile = NA))
})

test_that("age0isbirth na", {
  expect_error(chk_arg_val(age0isbirth = NA, overlay_percentile = NA))
})

test_that("age0isbirth character", {
  expect_error(chk_arg_val(age0isbirth = "TRUE", overlay_percentile = NA))
})

test_that("age0isbirth length 2", {
  expect_error(chk_arg_val(age0isbirth = c(TRUE, FALSE), overlay_percentile = NA))
})

test_that("overlay_percentile length 2", {
  expect_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = c(0.1,0.9)))
})

test_that("overlay_percentile NULL", {
  expect_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = NULL))
})

test_that("overlay_percentile not numeric", {
  expect_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = "A"))
})

test_that("overlay_percentile <= 0", {
  expect_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = 0))
})

test_that("overlay_percentile >= 1", {
  expect_error(chk_arg_val(age0isbirth = FALSE, overlay_percentile = 1))
})
