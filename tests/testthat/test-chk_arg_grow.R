# correct usage

data0 <- sim_kid(masterseed = 123)

test_that("correct usage 1", {
  expect_no_error(chk_arg_grow(data = data0, grow_time = 2, tstep = 1, tunit = "month"))
})

test_that("correct usage 2", {
  expect_no_error(chk_arg_grow(data = data0, grow_time = 2, tstep = 1, tunit = "year"))
})

# errors

test_that("data not data frame", {
  expect_error(chk_arg_grow(data = NULL, grow_time = 2, tstep = 1))
})

test_that("grow_time not numeric", {
  expect_error(chk_arg_grow(data = data0, grow_time = "2", tstep = 1))
})

test_that("grow_time length != 1", {
  expect_error(chk_arg_grow(data = data0, grow_time = c(1,2), tstep = 1))
})

test_that("grow_time negative", {
  expect_error(chk_arg_grow(data = data0, grow_time = -2, tstep = 1))
})

test_that("tstep not numeric", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = "1"))
})

test_that("tstep length != 1", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = c(1,2)))
})

test_that("tstep negative", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = -1))
})

test_that("tunit not character", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = 1, tunit = 1))
})

test_that("tunit length != 1", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = 1, tunit = c("month", "year")))
})

test_that("tunit not month or year", {
  expect_error(chk_arg_grow(data = data0, grow_time = 2, tstep = 1, tunit = "hour"))
})

test_that("data missing needed column", {
  expect_error(chk_arg_grow(data = data0 %>% dplyr::select(-CHART), grow_time = 2, tstep = 1, tunit = "month"))
})
