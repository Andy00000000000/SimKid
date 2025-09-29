data1 <- sim_kid(masterseed = 123)
p <- validate_kid(data1)
p_class <- unlist(lapply(seq_len(length(p)), function(i){class(p[[i]])}))

data2 <- sim_kid(age0to2yr_growthchart = "FENTON", masterseed = 123)

data3 <- sim_kid(masterseed = 123)
data3 <- grow_kid(data3, grow_time = 3)

test_that("correct usage 1", {
  expect_no_error(validate_kid(data1))
})

test_that("correct usage 2", {
  expect_no_error(validate_kid(data1, age0isbirth = TRUE, overlay_percentile = 0.90))
})

test_that("correct usage 3", {
  expect_no_error(validate_kid(data2))
})

test_that("correct usage 4", {
  expect_no_error(validate_kid(data3))
})

test_that("correct usage gives correct output 1", {
  expect_equal(all(p_class == "NULL"), FALSE)
})

test_that("correct usage gives correct output 2", {
  expect_equal(any(p_class == "ggplot"), TRUE)
})