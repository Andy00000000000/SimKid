## correct usage no errors ####

test_that("default correct", {
  expect_no_error(sim_kid())
})

test_that("default correct and without warnings", {
  expect_no_warning(sim_kid())
})

test_that("agedistr = norm correct", {
  expect_no_error(sim_kid(agedistr = "norm", agemean = 6, agesd = 1))
})

test_that("agedistr = nperage correct", {
  expect_no_error(sim_kid(agedistr = "nperage"))
})

test_that("age0to2yr_growthchart = WHO correct", {
  expect_no_error(sim_kid(age0to2yr_growthchart = "WHO"))
})

test_that("age0to2yr_growthchart = FENTON correct", {
  expect_no_error(sim_kid(age0to2yr_growthchart = "FENTON"))
})

test_that("age0isbirth = TRUE correct", {
  expect_no_error(sim_kid(age0isbirth = TRUE))
})

test_that("age2to20yr_correlate_htwt = FALSE correct", {
  expect_no_error(sim_kid(age2to20yr_correlate_htwt = FALSE))
})

## specific tests on output ####

test_that("class of data frame correct", {
  expect_equal(class(sim_kid()),"data.frame")
})

test_that("nrows correct for given num", {
  expect_equal(nrow(sim_kid(num = 3)),3)
})

test_that("column names correct for output", {
  expect_equal(
    colnames(sim_kid()),
    c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG","PHTCM")
  )
})

test_that("No NA for SEXF output", {
  expect_equal(
    any(is.na(sim_kid()$SEXF)),
    FALSE
  )
})

test_that("No NA for AGE output", {
  expect_equal(
    any(is.na(sim_kid()$AGE)),
    FALSE
  )
})

test_that("No NA for WTKG output", {
  expect_equal(
    any(is.na(sim_kid()$WTKG)),
    FALSE
  )
})

## END ####
