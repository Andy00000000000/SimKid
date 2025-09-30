#' Validate the simulated virtual subjects to anthropometric growth chart data
#' 
#' @description
#' Following creation of a virtual population using `sim_kid()`, overlay scatter plots are used to validate that the virtual population is reflective of the respective anthropometric growth chart data.
#'
#' @param data A data frame created by `sim_kid()`.
#' @param age0isbirth Logical `TRUE` or `FALSE` matching the `sim_kid()` input option used. Default of `FALSE`.
#' @param overlay_percentile `NA` (default) for no ribbon overlay of simulated percentiles. Or a numeric greater than `0` and less than `1` specifying the simulated percentile interval to overlay. For example, input of `0.90` would overlay the 5th and 95th percentiles of simulated data.
#' @param alpha Numeric between `0` and `1` specifying the simulated data transparency in validation plots. Default of `0.4`.
#'
#' @return A list of plot objects.
#' @export
#'
#' @examples
#' demo0 <- sim_kid() # single subject
#' validation_plots <- validate_kid(data = demo0)
validate_kid <- function(data = NULL, age0isbirth = FALSE, overlay_percentile = NA, alpha = 0.4){

  p1 <- NULL
  p2 <- NULL
  p3 <- NULL
  p4 <- NULL
  p5 <- NULL
  
  if("MONTH" %in% colnames(data)){ # to pass chk_out of dataset from grow_kid
    data <- suppressWarnings(data %>% dplyr::select(-.data$MONTH))
  }
  
  chk_out(data, num = nrow(data))
  chk_arg_val(age0isbirth = age0isbirth, overlay_percentile = overlay_percentile, alpha = alpha)
  
  age0to2yr_chart <- data[1,"CHART"]
  
  if(age0to2yr_chart != "FENTON"){
    #### Manipulate charts ####
    
    ped0 <- internal_kid0 %>%
      dplyr::mutate(AGEMO = substr(.data$AGEGRP, 2, (nchar(.data$AGEGRP)-1)))%>%
      dplyr::mutate(AGEMO = gsub(",.*", "", .data$AGEMO))%>%
      dplyr::mutate(AGEMO = suppressWarnings(as.numeric(.data$AGEMO)))%>%
      dplyr::mutate(USECHART = age0to2yr_chart)%>%
      dplyr::filter(.data$CHART == .data$USECHART | .data$AGEMO > 24)
    
    if(age0isbirth == T){
      ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "(0,1)")
    }else{
      ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "[0,0]")
    }
    
    htwt0 <- internal_htwt0 %>%
      dplyr::mutate(HTCM = substr(.data$HTCMGRP, 2, (nchar(.data$HTCMGRP)-1)))%>%
      dplyr::mutate(HTCM = gsub(",.*", "", .data$HTCM))%>%
      dplyr::mutate(HTCM = as.numeric(.data$HTCM))%>%
      dplyr::filter(.data$HTCMGRP != "[77,77]", .data$HTCMGRP != "[45,45]")%>%
      dplyr::mutate(USECHART = age0to2yr_chart)%>%
      dplyr::filter((.data$CHART == .data$USECHART & .data$VAR == "HTWT") | (.data$CHART == "CDC" & .data$VAR == "HTWT_GT2YR"))
    
    #### weight vs age ####
    
    p1 <- helper_valplot(data = data, ped0 = ped0, age0to2yr_chart = age0to2yr_chart, x = "AGEMO", y = "WTKG", overlay_percentile = overlay_percentile, alpha = alpha)
    
    #### height vs age ####
    
    p2 <- helper_valplot(data = data, ped0 = ped0, age0to2yr_chart = age0to2yr_chart, x = "AGEMO", y = "HTCM", overlay_percentile = overlay_percentile, alpha = alpha)
    
    #### weight vs height ####
    
    p3 <- helper_valplot(data = data, ped0 = htwt0 %>% dplyr::mutate(VAR = "WTKG"), age0to2yr_chart = age0to2yr_chart, x = "HTCM", y = "WTKG", overlay_percentile = overlay_percentile, alpha = alpha)
    p3 <- suppressMessages(p3 + ggplot2::scale_x_continuous()) # remove default breaks
    
    #### bmi vs age ####
    
    p4 <- helper_valplot(data = data, ped0 = ped0, age0to2yr_chart = age0to2yr_chart, x = "AGEMO", y = "BMI", overlay_percentile = overlay_percentile, alpha = alpha)
    
    #### end ####
  }else{
    #### Manipulate charts ####
    
    ped0 <- internal_kid0 %>%
      dplyr::filter(.data$CHART == age0to2yr_chart)%>%
      dplyr::mutate(GAWK = substr(.data$AGEGRP, 17, (nchar(.data$AGEGRP)-1)))%>%
      dplyr::mutate(GAWK = gsub(",.*", "", .data$GAWK))%>%
      dplyr::mutate(GAWK = as.numeric(.data$GAWK))
    
    #### weight vs age ####
    
    p5 <- helper_valplot(data = data, ped0 = ped0, age0to2yr_chart = age0to2yr_chart, x = "GAWK", y = "WTKG", overlay_percentile = overlay_percentile, alpha = alpha)
    
    #### end ####
  }
  
  list(p2,p1,p3,p4,p5)
}