#' Helper function for `sim_kid()` to simulate height and weight per CDC or WHO Growth Charts for Ages 0 to 2 year
#'
#' @param demo0 Input data frame of demographics with columns of `SEXF` and `GAWK`.
#' @param seedl Vector of integers specifying seeds for reproducibility.
#' @param seedindex Integer specifying the seed index to begin with.
#' @param zscore_min Minimum allowed Z score for weight.
#' @param zscore_max Maximum allowed Z score for weight.
#' @param age0isbirth A logical that specifies whether age equal to zero denotes birth.
#'   * `TRUE`: Age of `0` is birth.
#'   * `FALSE` (the default): Age of `0` is ages from birth to less than one month.
#' @param age0to2yr_growthchart A string that specifies which anthropometric growth charts are used for ages less than or equal to 2 years old.
#'   * `"CDC"` (the default): United States Centers for Disease Control and Prevention growth charts are used.
#'   * `"WHO"`: World Health Organization growth charts are used.
#'
#' @return A data frame with rows and columns matching `demo0`.
#'
#' @noRd
helper_kid_0to2yr <- function(
    demo0 = NULL,
    seedl = NULL,
    seedindex = NULL,
    zscore_min = NULL,
    zscore_max = NULL,
    age0isbirth = NULL,
    age0to2yr_growthchart = NULL
){
  ## MANIP TO READY ####
  
  demo <- demo0 %>% dplyr::filter(.data$AGEMO <= 24)
  
  ped0 <- internal_kid0 %>%
    dplyr::filter(.data$CHART == age0to2yr_growthchart) %>%
    dplyr::mutate(AGEMO = substr(.data$AGEGRP, 2, (nchar(.data$AGEGRP)-1)))%>%
    dplyr::mutate(AGEMO = gsub(",.*", "", .data$AGEMO))%>%
    dplyr::mutate(AGEMO = suppressWarnings(as.numeric(.data$AGEMO)))%>%
    dplyr::filter(.data$AGEMO <= 24)
  
  if(age0isbirth == T){
    ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "(0,1)")
  }else{
    ped0 <- ped0 %>% dplyr::filter(.data$AGEGRP != "[0,0]")
  }
  
  htwt0 <- internal_htwt0 %>%
    dplyr::filter(.data$CHART == age0to2yr_growthchart) %>%
    dplyr::filter(.data$VAR == "HTWT")%>%
    dplyr::mutate(HTCM = substr(.data$HTCMGRP, 2, (nchar(.data$HTCMGRP)-1)))%>%
    dplyr::mutate(HTCM = gsub(",.*", "", .data$HTCM))%>%
    dplyr::mutate(HTCM = as.numeric(.data$HTCM))%>%
    dplyr::filter(.data$HTCMGRP != "[77,77]", .data$HTCMGRP != "[45,45]")
  
  digits_htcm <- ifelse(age0to2yr_growthchart == "CDC", 0, 1)
  
  ## CALCULATE HTCM ####
  
  withr::with_seed(seedl[seedindex], demo$ZHTCM <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max))
  seedindex <- seedindex + 1
  
  demo <- demo %>%
    dplyr::mutate(VAR = "HTCM")%>%
    dplyr::left_join(ped0, by = c("VAR","SEXF","AGEMO"))%>%
    dplyr::mutate(HTCM = ifelse(round(.data$L,1E-6) == 0, .data$M*exp(.data$S*.data$ZHTCM), .data$M*(1+.data$L*.data$S*.data$ZHTCM)^(1/.data$L)))%>%
    dplyr::mutate(HTCM_DIGITS = digits_htcm)%>%
    dplyr::mutate(HTCM = ifelse(.data$HTCM_DIGITS == 1, 0.5*round(2*.data$HTCM), round(.data$HTCM))) # nearest 0.5
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## CALCULATE WTKG ####
  
  withr::with_seed(seedl[seedindex], demo$ZWTKG <- msm::rtnorm(nrow(demo), 0, 1, zscore_min, zscore_max))
  seedindex <- seedindex + 1
  
  demo <- demo %>%
    dplyr::mutate(HTCM = ifelse(.data$HTCM < 45, 45, .data$HTCM)) %>% # ZZZ lower limit of weight vs height chart
    dplyr::left_join(htwt0, by = c("SEXF","HTCM"))%>%
    dplyr::mutate(WTKG = ifelse(round(.data$L,1E-6) == 0, .data$M*exp(.data$S*.data$ZWTKG), .data$M*(1+.data$L*.data$S*.data$ZWTKG)^(1/.data$L)))
  
  demo <- demo[,which(colnames(demo) %in% colnames(demo0))]
  
  ## RETURN ####
  
  return(list(demo = demo, seedindex = seedindex))
  
  ## END ####
}