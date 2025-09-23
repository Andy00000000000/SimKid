## correct usage ####

test_that("default correct", {
  expect_no_error(chk_arg())
})

test_that("default correct and without warnings", {
  expect_no_warning(chk_arg())
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

test_that("agedistr = correct character of nperage", {
  expect_no_error(chk_arg(agedistr = "nperage"))
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

test_that("prob_female = 0", {
  expect_no_error(chk_arg(prob_female = 0))
})

test_that("prob_female = 1", {
  expect_no_error(chk_arg(prob_female = 1))
})

test_that("prob_female = 0.25", {
  expect_no_error(chk_arg(prob_female = 0.25))
})

test_that("age0isbirth = TRUE", {
  expect_no_error(chk_arg(age0isbirth = TRUE))
})

test_that("age0isbirth = FALSE", {
  expect_no_error(chk_arg(age0isbirth = FALSE))
})

test_that("age2to20yr_correlate_htwt = TRUE", {
  expect_no_error(chk_arg(age2to20yr_correlate_htwt = TRUE))
})

test_that("age2to20yr_correlate_htwt = FALSE", {
  expect_no_error(chk_arg(age2to20yr_correlate_htwt = FALSE))
})

test_that("masterseed = NULL", {
  expect_no_error(chk_arg(masterseed = NULL))
})

test_that("htwt_percentile_min = 0.1", {
  expect_no_error(chk_arg(htwt_percentile_min = 0.1))
})

test_that("htwt_percentile_max = 0.1", {
  expect_no_error(chk_arg(htwt_percentile_max = 0.1))
})

test_that("htwt_percentile_min = 0.1 and htwt_percentile_max = 0.2", {
  expect_no_error(chk_arg(htwt_percentile_min = 0.1, htwt_percentile_max = 0.2))
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

test_that("masterseed = NULL", {
  expect_no_error(chk_arg(masterseed = NULL))
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

## prob_female ####

test_that("prob_female < 0", {
  expect_error(chk_arg(prob_female = -0.1))
})

test_that("prob_female > 1", {
  expect_error(chk_arg(prob_female = 20))
})

test_that("prob_female = NULL", {
  expect_error(chk_arg(prob_female = NULL))
})

test_that("prob_female = NA", {
  expect_error(chk_arg(prob_female = NA))
})

test_that("prob_female length > 1", {
  expect_error(chk_arg(prob_female = c(0,0.2,1)))
})

test_that("prob_female not numeric", {
  expect_error(chk_arg(prob_female = "abc"))
})

## age0isbirth ####

test_that("age0isbirth = NA", {
  expect_error(chk_arg(age0isbirth = NA))
})

test_that("age0isbirth = NULL", {
  expect_error(chk_arg(age0isbirth = NULL))
})

test_that("age0isbirth length > 1", {
  expect_error(chk_arg(age0isbirth = c(TRUE,TRUE)))
})

test_that("age0isbirth not logical", {
  expect_error(chk_arg(age0isbirth = 1))
})

test_that("age0isbirth when age0to2yr_growthchart = FENTON", {
  expect_warning(chk_arg(age0isbirth = TRUE, age0to2yr_growthchart = "FENTON"))
})

## age2to20yr_correlate_htwt ####

test_that("age2to20yr_correlate_htwt = NA", {
  expect_error(chk_arg(age2to20yr_correlate_htwt = NA))
})

test_that("age2to20yr_correlate_htwt = NULL", {
  expect_error(chk_arg(age2to20yr_correlate_htwt = NULL))
})

test_that("age2to20yr_correlate_htwt length > 1", {
  expect_error(chk_arg(age2to20yr_correlate_htwt = c(TRUE,TRUE)))
})

test_that("age2to20yr_correlate_htwt not logical", {
  expect_error(chk_arg(age2to20yr_correlate_htwt = 1))
})

test_that("age2to20yr_correlate_htwt when age0to2yr_growthchart = FENTON", {
  expect_warning(chk_arg(age2to20yr_correlate_htwt = FALSE, age0to2yr_growthchart = "FENTON"))
})

## htwt_percentile_min ####

test_that("htwt_percentile_min = length > 1", {
  expect_error(chk_arg(htwt_percentile_min = c(1,2)))
})

test_that("htwt_percentile_min = NA", {
  expect_error(chk_arg(htwt_percentile_min = NA))
})

test_that("htwt_percentile_min = Inf", {
  expect_error(chk_arg(htwt_percentile_min = Inf))
})

test_that("htwt_percentile_min = not numeric", {
  expect_error(chk_arg(htwt_percentile_min = "100"))
})

test_that("htwt_percentile_min < min allowed", {
  expect_error(chk_arg(htwt_percentile_min = 0.000999))
})

test_that("htwt_percentile_min < min suggested for Fenton", {
  expect_warning(chk_arg(htwt_percentile_min = 0.009, age0to2yr_growthchart = "FENTON"))
})

## htwt_percentile_max ####

test_that("htwt_percentile_max = length > 1", {
  expect_error(chk_arg(htwt_percentile_max = c(1,2)))
})

test_that("htwt_percentile_max = NA", {
  expect_error(chk_arg(htwt_percentile_max = NA))
})

test_that("htwt_percentile_max = Inf", {
  expect_error(chk_arg(htwt_percentile_max = Inf))
})

test_that("htwt_percentile_max = not numeric", {
  expect_error(chk_arg(htwt_percentile_max = "100"))
})

test_that("htwt_percentile_max > max allowed", {
  expect_error(chk_arg(htwt_percentile_max = 0.9991))
})

test_that("htwt_percentile_max = htwt_percentile_min when age2to20yr_correlate_htwt is TRUE", {
  expect_error(chk_arg(htwt_percentile_min = 0.6, htwt_percentile_max = 0.6, age2to20yr_correlate_htwt = TRUE))
})

test_that("htwt_percentile_max = htwt_percentile_min when age2to20yr_correlate_htwt is FALSE", {
  expect_no_error(chk_arg(htwt_percentile_min = 0.6, htwt_percentile_max = 0.6, age2to20yr_correlate_htwt = FALSE))
})

test_that("htwt_percentile_max < htwt_percentile_min", {
  expect_error(chk_arg(htwt_percentile_min = 0.6, htwt_percentile_max = 0.5))
})

test_that("htwt_percentile_max < htwt_percentile_min when age2to20yr_correlate_htwt is FALSE", {
  expect_error(chk_arg(htwt_percentile_min = 0.6, htwt_percentile_max = 0.5, age2to20yr_correlate_htwt = FALSE))
})

test_that("htwt_percentile_max > max suggested for Fenton", {
  expect_warning(chk_arg(htwt_percentile_max = 0.991, age0to2yr_growthchart = "FENTON"))
})

## masterseed ####

test_that("masterseed = length > 1", {
  expect_error(chk_arg(masterseed = c(1,2)))
})

test_that("masterseed = NA", {
  expect_error(chk_arg(masterseed = NA))
})

test_that("masterseed = Inf", {
  expect_error(chk_arg(masterseed = Inf))
})

test_that("masterseed = not numeric", {
  expect_error(chk_arg(masterseed = "100"))
})

test_that("masterseed = not integer", {
  expect_error(chk_arg(masterseed = 100.2))
})

test_that("masterseed == 0", {
  expect_error(chk_arg(masterseed = 0))
})

test_that("masterseed < 0", {
  expect_error(chk_arg(masterseed = -1))
})

## END ####
