## correct usage ####

test_that("default correct", {
  expect_no_error(sim_kid())
})

test_that("default correct and without warnings", {
  expect_no_warning(sim_kid())
})

## END ####
