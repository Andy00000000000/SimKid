#' Initialize inputs for `sim_kid()`
#'
#' Initializes the input arguments for `sim_kid()`
#'
#' @inheritParams sim_kid
#'
#' @return A list of select input arguments.
#'
#' @noRd
init_arg <- function(
    age0to2yr_growthchart = NULL,
    agemin = NULL,
    agemax = NULL,
    htwt_percentile_min = NULL,
    htwt_percentile_max = NULL
){
  
  agemin <- ifelse(is.null(agemin), ifelse(age0to2yr_growthchart == "FENTON", 22, 0), agemin)
  agemax <- ifelse(is.null(agemax), ifelse(age0to2yr_growthchart == "FENTON", 40.99, 239.99), agemax)
  htwt_percentile_min <- ifelse(is.null(htwt_percentile_min), ifelse(age0to2yr_growthchart == "FENTON", 0.01, 0.001), htwt_percentile_min)
  htwt_percentile_max <- ifelse(is.null(htwt_percentile_max), ifelse(age0to2yr_growthchart == "FENTON", 0.99, 0.999), htwt_percentile_max)
  
  list(agemin = agemin, agemax = agemax, htwt_percentile_min = htwt_percentile_min, htwt_percentile_max = htwt_percentile_max)
}