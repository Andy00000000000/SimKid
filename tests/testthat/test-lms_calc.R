# correct usage

test_that("correct 1: All length 1", {
  expect_no_error(lms_calc(z = 0, l = -0.2165012, m = 12.74154, s = 0.1081660))
})

test_that("correct 2: Z length 1", {
  expect_no_error(lms_calc(
    z = 0, l = rep(-0.2165012,2), m = rep(12.74154,2), s = rep(0.1081660,2)
  ))
})

test_that("correct 3: LMS length 1", {
  expect_no_error(lms_calc(
    z = c(qnorm(0.25), 0, qnorm(0.75)), 
    l = -0.2165012, 
    m = 12.74154, 
    s = 0.1081660
  ))
})

test_that("correct 4: All equal length > 1", {
  expect_no_error(lms_calc(
    z = c(0, qnorm(0.25)), 
    l = c(-0.6213197, -1.0244713), 
    m = c(14.40263, 13.94108), 
    s = c(0.1118745, 0.1194917)
  ))
})

# correct calculations

test_that("correct calculation 1", {
  
  kid0 <- internal_kid0 %>%
    dplyr::filter(
      .data$CHART == "CDC", .data$VAR == "WTKG", 
      .data$SEXF == 0, .data$AGEGRP == "[24,25)"
    )
  
  expect_equal(
    lms_calc(z = qnorm(0.50), l = kid0$L, m = kid0$M, s = kid0$S),
    kid0$P50
  )
})

test_that("correct calculation 2", {
  
  kid0 <- internal_kid0 %>%
    dplyr::filter(
      .data$CHART == "CDC", .data$VAR == "WTKG", 
      .data$SEXF == 0, .data$AGEGRP == "[216,217)"
    )
  
  expect_equal(
    lms_calc(
      z = c(qnorm(0.25), qnorm(0.50), qnorm(0.75)), 
      l = kid0$L, m = kid0$M, s = kid0$S
    ),
    c(kid0$P25, kid0$P50, kid0$P75)
  )
})

test_that("correct calculation 3", {
  
  kid0 <- internal_kid0 %>%
    dplyr::filter(
      .data$CHART == "CDC", .data$VAR == "WTKG", 
      .data$AGEGRP == "[220,221)"
    )
  
  expect_equal(
    lms_calc(
      z = c(qnorm(0.25), qnorm(0.50)), l = kid0$L, m = kid0$M, s = kid0$S
    ),
    c(kid0[which(kid0$SEXF == 0), "P25"], kid0[which(kid0$SEXF == 1), "P50"])
  )
})

# end