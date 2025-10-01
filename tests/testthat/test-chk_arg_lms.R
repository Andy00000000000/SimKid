# Errors

test_that("z NA", {
  expect_error(chk_arg_lms(
    z = c(1,NA), l = -0.2165012, m = 12.74154, s = 0.1081660
  ))
})

test_that("l has NA", {
  expect_error(chk_arg_lms(
    z = rep(0,2), l = c(-0.2165012,NA),
    m = rep(12.74154,2), s = rep(0.1081660,2)
  ))
})

test_that("m has NA", {
  expect_error(chk_arg_lms(
    z = rep(0,2), l = rep(-0.2165012,2), m = c(NA,2), s = rep(0.1081660,2)
  ))
})

test_that("s has NA", {
  expect_error(chk_arg_lms(
    z = rep(0,2), l = rep(-0.2165012,2), m = rep(12.74154,2), s = c(NA,2)
  ))
})

test_that("z not numeric", {
  expect_error(chk_arg_lms(
    z = "A", l = -0.2165012, m = 12.74154, s = 0.1081660
  ))
})

test_that("l not numeric", {
  expect_error(chk_arg_lms(
    z = -1, l = "A", m = 12.74154, s = 0.1081660
  ))
})

test_that("m not numeric", {
  expect_error(chk_arg_lms(
    z = -1, l = -0.2165012, m = "A", s = 0.1081660
  ))
})

test_that("s not numeric", {
  expect_error(chk_arg_lms(
    z = -1, l = -0.2165012, m = 12.74154, s = "A"
  ))
})

test_that("length l != length m", {
  expect_error(chk_arg_lms(
    z = rep(-1,2), l = rep(-0.2165012,2), m = 12.74154, s = rep(0.1081660,2)
  ))
})

test_that("length l != length s", {
  expect_error(chk_arg_lms(
    z = rep(-1,2), l = rep(-0.2165012,2), 
    m = rep(12.74154,2), s = rep(0.1081660,3)
  ))
})

test_that("length m != length s", {
  expect_error(chk_arg_lms(
    z = rep(-1,2), l = rep(-0.2165012,2), 
    m = rep(12.74154,2), s = rep(0.1081660,1)
  ))
})

test_that("length l != length z when length l >1", {
  expect_error(chk_arg_lms(
    z = rep(-1,2), l = rep(-0.2165012,3), 
    m = rep(12.74154,3), s = rep(0.1081660,3)
  ))
})

# Correct usage

test_that("correct 1: All length 1", {
  expect_no_error(chk_arg_lms(
    z = 0, l = -0.2165012, m = 12.74154, s = 0.1081660
  ))
})

test_that("correct 2: Z length 1", {
  expect_no_error(chk_arg_lms(
    z = 0, l = rep(-0.2165012,2), m = rep(12.74154,2), s = rep(0.1081660,2)
  ))
})

test_that("correct 3: LMS length 1", {
  expect_no_error(chk_arg_lms(
    z = c(qnorm(0.25), 0, qnorm(0.75)), l = -0.2165012, 
    m = 12.74154, s = 0.1081660
  ))
})

test_that("correct 4: All equal length > 1", {
  expect_no_error(chk_arg_lms(
    z = c(0, qnorm(0.25)), l = c(-0.6213197, -1.0244713), 
    m = c(14.40263, 13.94108), s = c(0.1118745, 0.1194917)
  ))
})

# End