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
#' @param grow_time A non-negative numeric specifying the duration of time in months the virtual subjects are allowed to grow for. Will be rounded to the nearest month.
#' @param tstep A positive numeric specifying the time step for growth in months. Default of `1`. Will be rounded to the nearest month.
#' @param age0isbirth Logical `TRUE` or `FALSE` matching the `sim_kid()` input option used. Default of `FALSE`.
#'
#' @return A data frame with columns matching those of `data` and the number of rows equal to `nrow(data)*(1+grow_time/tstep)-nsubtract`. Where nsubtract is the number of records with age greater than 240 months.
#' @export
#'
#' @examples
#' demo0 <- sim_kid()
#' demo <- grow_kid(data = demo0, grow_time = 12) # growth for 1 year at monthly time step
grow_kid <- function(data = NULL, grow_time = 0, tstep = 1, age0isbirth = FALSE){
  
  tstep <- round(tstep)
  grow_time <- round(grow_time)
  
  chk_arg_grow(data = data, grow_time = grow_time, tstep = tstep, age0isbirth = age0isbirth)
  
  if(any(data$CHART == "FENTON") == TRUE){
    stop("Error: grow_kid() does not apply to simulated newborns using the Fenton growth chart data.")
  }
  
  columns <- colnames(data)
  columns_timevary <- c("WTKG","HTCM","BMI","BSA1","BSA2","BSA3")
  columns_timeinvary <- columns[which(!columns %in% columns_timevary)]
  
  MONTH <- seq(0, grow_time, tstep)
  
  tmp <- tidyr::expand(data %>% dplyr::group_by(.data$ID), MONTH)%>%
    as.data.frame()
  
  data1 <- tmp %>%
    dplyr::left_join(data[,columns_timeinvary], by = "ID")%>%
    dplyr::left_join(data[,c("ID",columns_timevary)] %>% dplyr::mutate(MONTH = 0), by = c("ID","MONTH"))
  
  data1 <- data1[,c(columns,"MONTH")]
  
  data1 <- data1 %>%
    dplyr::mutate(AGEMO = .data$AGEMO + .data$MONTH)%>%
    dplyr::mutate(AGE = round(.data$AGEMO/12,3))
  
  nsubtract <- nrow(data1[which(data1$AGEMO > 239),])
  
  data1 <- data1 %>%
    dplyr::filter(.data$AGEMO <= 239)
  
  if(nrow(data1[which(data1$AGEMO <= 24),]) > 0){
    data1 <- helper_kid_0to2yr(
      demo0 = data1, age0isbirth = age0isbirth, age0to2yr_growthchart = unique(data1[which(data1$AGEMO <= 24), "CHART"]), sim_z = FALSE
    )$demo
  }
  
  if(nrow(data1[which(data1$AGEMO > 24),]) > 0){
    data1 <- suppressWarnings(helper_kid_2to20yr(demo0 = data1, sim_z = FALSE)$demo)
  }
  
  data1 <- calc_bmi_bsa(data1)
  
  suppressWarnings(chk_out(demo = data1 %>% dplyr::select(-.data$MONTH), num = nrow(data)*(1+grow_time/tstep)-nsubtract))
  
  data1
}