test_that("correct works", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG","PHTCM")
  expect_no_error(chk_out(demo = demo, num = 1))
})

test_that("not data frame fails 1", {
  expect_error(chk_out(demo = NULL, num = 1))
})

test_that("not data frame fails 2", {
  expect_error(chk_out(demo = NA, num = 1))
})

test_that("not data frame fails 2", {
  expect_error(chk_out(demo = 1, num = 1))
})

test_that("incorrect number of rows fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG","PHTCM")
  expect_error(chk_out(demo = demo, num = 2))
})

test_that("incorrect number of columns fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 14))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG")
  expect_error(chk_out(demo = demo, num = 1))
})

test_that("incorrect column names fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG", "ABC")
  expect_error(chk_out(demo = demo, num = 1))
})

test_that("NA SEXF fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG", "PHTCM")
  demo[1,"SEXF"] <- NA
  expect_error(chk_out(demo = demo, num = 1))
})

test_that("NA AGE fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG", "PHTCM")
  demo[1,"AGE"] <- NA
  expect_error(chk_out(demo = demo, num = 1))
})

test_that("NA WTKG fails", {
  demo <- as.data.frame(matrix(0, nrow = 1, ncol = 15))
  colnames(demo) <- c("ID","SEXF","AGEMO","AGE","GAWK","WTKG","HTCM","BMI","BSA1","BSA2","BSA3","ZWTKG","ZHTCM","PWTKG", "PHTCM")
  demo[1,"WTKG"] <- NA
  expect_error(chk_out(demo = demo, num = 1))
})
