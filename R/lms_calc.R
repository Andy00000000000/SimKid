#' Calculate the dependent variable (height or weight) using LMS parameters 
#' from anthropometric growth charts at a given Z score
#' 
#' @param z Numeric or numerical vector of Z score(s) associated with a given 
#' percentile of the dependent variable. Default of 0 (i.e., 50th percentile).
#' @param l Numeric or numerical vector of L parameter(s) from an 
#' anthropometric growth chart.
#' @param m Numeric or numerical vector of M parameter(s) from an 
#' anthropometric growth chart.
#' @param s Numeric or numerical vector of S parameter(s) from an 
#' anthropometric growth chart.
#'
#' @return Numeric or vector of numeric dependent variable value(s). Value 
#' is calculated according to: if `l` (rounded to 6 decimal places) is equal to 
#' 0, then `= l*exp(s*z)`; otherwise `= m*(1+l*s*z)^(1/l))`.
#' @export
#'
#' @examples
#' # calculate weight (kg) for a male 2 year old at the 50th percentile of 
#' # weight for age and sex using CDC Growth Chart LMS parameters.
#' wtkg <- lms_calc(
#' z = qnorm(50/100), 
#' l = -0.2165012, 
#' m = 12.74154, 
#' s = 0.1081660
#' )
#' 
#' # calculate weight (kg) for male 2 year old at the 25th and 50th percentiles 
#' # of weight.
#' wtkg <- lms_calc(
#' z = c(qnorm(25/100),
#' qnorm(50/100)), 
#' l = -0.2165012, 
#' m = 12.74154, 
#' s = 0.1081660
#' )
#' 
#' # calculate weight (kg) for male at 50th percentile and female at 25th 
#' # percentile of weight.
#' wtkg <- lms_calc(
#'   z = c(0, qnorm(0.25)),
#'   l = c(-0.6213197, -1.0244713),
#'   m = c(14.40263, 13.94108),
#'   s = c(0.1118745, 0.1194917)
#' )
lms_calc <- function(z = 0, l = NA, m = NA, s = NA){
  
  chk_arg_lms(z = z, l = l, m = m, s = s)
  
  # l, m, s are same length checked by chk_arg_lms
  if(length(z) > 1 && length(l) == 1){
    l <- rep(l, length(z))
    m <- rep(m, length(z))
    s <- rep(s, length(z))
  }
  
  if(length(l) > 1 && length(z) == 1){
    z <- rep(z, length(l))
  }
  
  dplyr::if_else(
    round(l,1E-6) == 0, 
    m*exp(s*z), 
    m*(1+l*s*z)^(1/l)
  )
}