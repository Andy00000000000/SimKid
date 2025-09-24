test_that("multiplication works", {
  expect_no_error(calc_bmi_bsa(data = data.frame(WTKG = 1, HTCM = 1)))
})