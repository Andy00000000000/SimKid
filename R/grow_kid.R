#' Grow the simulated virtual subjects using anthropometric growth chart data
#' 
#' @description
#' Following creation of a virtual population using `sim_kid()`, each virtual subject grows from their baseline age.
#' It is assumed that each virtual subject remains at the same respective percentiles of height- and weight-for-age-and-sex as they were at baseline.
#' For example, if `sim_kid()` created a male 2 year old at the 25th percentile of height and the 30th percentile of weight, 
#' then if allowed to grow to 3 years old, this subject would be at the 25th percentile of height and 30th percentile of weight 
#' for 3 year old males according to the given anthropometric growth chart.
#' Note that this function will not work for virtual preterm newborns created using the Fenton growth chart data.
#'
#' @param data A data frame created by `sim_kid()`.
#' @param grow_time A non-negative numeric specifying the duration of time the virtual subjects are allowed to grow for.
#' @param tstep A positive numeric specifying the time step for growth. Default of `1`.
#' @param tunit The time unit. Options of:
#' * `month` (default).
#' * `year`
#'
#' @return A data frame with columns matching those of `data` and the number of rows equal to `nrow(data)+grow_time/tstep`.
#' @export
#'
#' @examples
#' demo0 <- sim_kid()
#' demo <- grow_kid(data = demo0, grow_time = 12) # growth for 1 year at monthly time step
grow_kid <- function(data = NULL, grow_time = 0, tstep = 1, tunit = "month"){
  
  chk_arg_grow(data = data, grow_time = grow_time, tstep = tstep, tunit = tunit)
  
  if(tunit == "year"){
    grow_time <- grow_time*12
    tstep <- tstep*12
  }
  
}