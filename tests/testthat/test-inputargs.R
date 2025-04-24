## correct usage ####

test_that("default correct", {
  expect_no_error(chk_arg())
})

test_that("num correct", {
  expect_no_error(chk_arg(num = 2))
})

test_that("agedistr = correct character of unif", {
  expect_no_error(chk_arg(agedistr = "unif"))
})

test_that("agedistr = correct character of norm", {
  expect_no_error(chk_arg(agedistr = "norm", agemean = 48, agesd = 1))
})

test_that("age0to2yr_growthchart = CDC correct", {
  expect_no_error(chk_arg(age0to2yr_growthchart = "CDC"))
})

test_that("age0to2yr_growthchart = WHO correct", {
  expect_no_error(chk_arg(age0to2yr_growthchart = "WHO"))
})

test_that("age0to2yr_growthchart = FENTON correct", {
  expect_no_error(chk_arg(age0to2yr_growthchart = "FENTON"))
})

test_that("age0to2yr_growthchart = Fenton correct", {
  expect_no_error(chk_arg(age0to2yr_growthchart = "Fenton"))
})

test_that("agemean equal to agemin and agemax", {
  expect_no_error(chk_arg(agedistr = "norm", agemean = 48, agesd = 0, agemin = 48, agemax = 48))
})

## num ####

test_that("num = length > 1", {
  expect_error(chk_arg(num = c(1,2)))
})

test_that("num = NULL", {
  expect_error(chk_arg(num = NULL))
})

test_that("num = NA", {
  expect_error(chk_arg(num = NA))
})

test_that("num = Inf", {
  expect_error(chk_arg(num = Inf))
})

test_that("num = not numeric", {
  expect_error(chk_arg(num = "100"))
})

test_that("num = not integer", {
  expect_error(chk_arg(num = 100.2))
})

test_that("num == 0", {
  expect_error(chk_arg(num = 0))
})

test_that("num < 0", {
  expect_error(chk_arg(num = -1))
})

## agedistr ####

test_that("agedistr = length > 1", {
  expect_error(chk_arg(agedistr = c("unif","unif")))
})

test_that("agedistr = NULL", {
  expect_error(chk_arg(agedistr = NULL))
})

test_that("agedistr = NA", {
  expect_error(chk_arg(agedistr = NA))
})

test_that("agedistr = not character", {
  expect_error(chk_arg(agedistr = 123))
})

test_that("agedistr = not correct character", {
  expect_error(chk_arg(agedistr = "123"))
})

## agemin and agemax ####

test_that("agemin not length = 1", {
  expect_error(chk_arg(agemin = c(1,2)))
})

test_that("agemax not length = 1", {
  expect_error(chk_arg(agemax = c(1,2)))
})

test_that("agemin not numeric", {
  expect_error(chk_arg(agemin = TRUE))
})

test_that("agemax not numeric", {
  expect_error(chk_arg(agemax = as.factor("a")))
})

test_that("agemin < 0", {
  expect_error(chk_arg(agemin = -1))
})

test_that("agemax > 240", {
  expect_error(chk_arg(agemax = 241))
})

test_that("agemax = 240", {
  expect_error(chk_arg(agemax = 240))
})

test_that("agemin < 22 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agemin = 21, age0to2yr_growthchart = "FENTON"))
})

test_that("agemax > 41 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agemax = 42, age0to2yr_growthchart = "FENTON"))
})

test_that("agemax = 41 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agemax = 41, age0to2yr_growthchart = "FENTON"))
})

test_that("agemax >= agemin", {
  expect_error(chk_arg(agemax = 24, agemin = 25))
})

## agemean and agesd ####

test_that("agemean not used", {
  expect_warning(chk_arg(agemean = 1))
})

test_that("agesd not used", {
  expect_warning(chk_arg(agesd = 1))
})

test_that("agemean and agesd used", {
  expect_no_warning(chk_arg(agedistr = "norm", agemean = 1, agesd = 0))
})

test_that("agemean missing when agenorm", {
  expect_error(chk_arg(agedistr = "norm", agesd = 1))
})

test_that("agesd missing when agenorm", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48))
})

test_that("agemean not numeric", {
  expect_error(chk_arg(agedistr = "norm", agemean = "a", agesd = 1))
})

test_that("agesd not numeric", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48, agesd = "a"))
})

test_that("agemean not length = 1", {
  expect_error(chk_arg(agedistr = "norm", agemean = c(1,2), agesd = 1))
})

test_that("agesd not length = 1", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48, agesd = c(1,2)))
})

test_that("agesd < 0", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48, agesd = -1))
})

test_that("agemean < agemin", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48, agesd = 1, agemin = 49))
})

test_that("agemean > agemax", {
  expect_error(chk_arg(agedistr = "norm", agemean = 48, agesd = 1, agemax = 30))
})

test_that("agemean < 0", {
  expect_error(chk_arg(agedistr = "norm", agemean = -1, agesd = 1))
})

test_that("agemean > 240", {
  expect_error(chk_arg(agedistr = "norm", agemean = 250, agesd = 1))
})

test_that("agemean = 240", {
  expect_error(chk_arg(agedistr = "norm", agemean = 240, agesd = 1))
})

test_that("agemean < 22 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agedistr = "norm", agemean = 21, agesd = 1, age0to2yr_growthchart = "FENTON"))
})

test_that("agemean > 41 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agedistr = "norm", agemean = 42, agesd = 1, age0to2yr_growthchart = "FENTON"))
})

test_that("agemean = 41 when age0to2yr_growthchart = FENTON", {
  expect_error(chk_arg(agedistr = "norm", agemean = 41, agesd = 1, age0to2yr_growthchart = "FENTON"))
})

## age0to2yr_growthchart ####

test_that("age0to2yr_growthchart not length = 1", {
  expect_error(chk_arg(age0to2yr_growthchart = rep("CDC",2)))
})

test_that("age0to2yr_growthchart not character", {
  expect_error(chk_arg(age0to2yr_growthchart = 1))
})

test_that("age0to2yr_growthchart not correct", {
  expect_error(chk_arg(age0to2yr_growthchart = "CDCa"))
})

## END ####
