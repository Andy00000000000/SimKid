## Tests for get_seeds()

test_that("default correct no warning", {
  expect_no_warning(get_seeds())
})

test_that("nseed length > 1", {
  expect_warning(get_seeds(nseed = c(1,2)))
})

test_that("nseed not numeric", {
  expect_warning(get_seeds(nseed = "abc"))
})

test_that("nseed not integer", {
  expect_warning(get_seeds(nseed = 10.2))
})

test_that("nseed = 0", {
  expect_warning(get_seeds(nseed = 0))
})

test_that("nseed < 0", {
  expect_warning(get_seeds(nseed = -10))
})

test_that("nseed input error causes default", {
  expect_equal(length(suppressWarnings(get_seeds(nseed = -10))), 1000L)
})

test_that("default missing nseed to 1000", {
  expect_equal(length(get_seeds()), 1000L)
})

test_that("nseed = 2 to length 2", {
  expect_equal(length(get_seeds(nseed = 2)), 2L)
})

test_that("no masterseed has different results", {
  expect_equal(all(get_seeds() == get_seeds()), FALSE)
})

test_that("masterseed has same results", {
  expect_equal(
    all(get_seeds(masterseed = 513) == get_seeds(masterseed = 513)), 
    TRUE
  )
})

## END ####
