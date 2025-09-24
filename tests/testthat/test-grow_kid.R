demo0 <- sim_kid(num = 2, masterseed = 123)

test_that("default correct", {
  expect_no_error(grow_kid(demo0))
})

test_that("default correct and without warnings", {
  expect_no_warning(grow_kid(demo0))
})

test_that("real example correct 1", {
  expect_no_error(grow_kid(demo0, grow_time = 2))
})

test_that("real example correct 2", {
  expect_no_error(grow_kid(sim_kid(num = 1, masterseed = 123, agemax = 24), grow_time = 2))
})

test_that("fenton gives error", {
  expect_error(grow_kid(sim_kid(num = 1, masterseed = 123, age0to2yr_growthchart = "FENTON"), grow_time = 2))
})