# correct usage

test_that("correct usage", {
  expect_no_error(chk_arg_bodysize(data = data.frame(WTKG = 1, HTCM = 1)))
})

# errors

test_that("data not data frame", {
  expect_error(chk_arg_bodysize(data = NULL))
})

test_that("data missing needed column", {
  expect_error(chk_arg_bodysize(data = data.frame(WTKG = 1)))
})

# end