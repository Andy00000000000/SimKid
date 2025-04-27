## Unit Tests for init_arg

test_that("initialize agemin not NULL", {
  initargs <- init_arg(agemin = 1)
  expect_equal(initargs$agemin, 1)
})

test_that("initialize agemax not NULL", {
  initargs <- init_arg(agemax = 10)
  expect_equal(initargs$agemax, 10)
})

test_that("initialize htwt_percentile_min not NULL", {
  initargs <- init_arg(htwt_percentile_min = 0.1)
  expect_equal(initargs$htwt_percentile_min, 0.1)
})

test_that("initialize htwt_percentile_max not NULL", {
  initargs <- init_arg(htwt_percentile_max = 0.9)
  expect_equal(initargs$htwt_percentile_max, 0.9)
})

test_that("initialize agemin not FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "CDC")
  expect_equal(initargs$agemin, 0)
})

test_that("initialize agemin FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "FENTON")
  expect_equal(initargs$agemin, 22)
})

test_that("initialize agemax not FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "CDC")
  expect_equal(initargs$agemax, 239.99)
})

test_that("initialize agemax FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "FENTON")
  expect_equal(initargs$agemax, 40.99)
})

test_that("initialize htwt_percentile_min not FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "CDC")
  expect_equal(initargs$htwt_percentile_min, 0.001)
})

test_that("initialize htwt_percentile_min FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "FENTON")
  expect_equal(initargs$htwt_percentile_min, 0.01)
})

test_that("initialize htwt_percentile_max not FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "CDC")
  expect_equal(initargs$htwt_percentile_max, 0.999)
})

test_that("initialize htwt_percentile_max FENTON", {
  initargs <- init_arg(age0to2yr_growthchart = "FENTON")
  expect_equal(initargs$htwt_percentile_max, 0.99)
})

## END ####

